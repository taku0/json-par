;;; json-par-test-editing.el --- Test for json-par-mode: editing  -*- lexical-binding: t -*-

;; Copyright (C) 2021 taku0

;; Author: taku0 (http://github.com/taku0)

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Test for json-par-mode: editing.
;; Execute json-par-run-test-editing interactively or in batch mode.

;;; Code:

(require 'json-mode)
(require 'json-par)
(require 'json-par-test)

(defun json-par-run-test-editing
    (&optional error-buffer error-counts progress-reporter)
  "Run editing test for `json-par-mode'.

ERROR-BUFFER is the buffer to output errors.
ERROR-COUNTS is a association list holding counts of errors. Updated
destructively.
PROGRESS-REPORTER is the progress-reporter."
  (interactive)
  (if (not json-par-test-running)
      (json-par-run-test '(json-par-run-test-editing))
    (let ((current-line 0)
          (context-start-line 0)
          (context-start 0)
          (context-end 0)
          (context-text nil)
          (expected-start 0)
          (expected-end 0)
          (expected-text nil)
          (actions '()))
      (setq default-directory
            (concat (file-name-as-directory json-par-test-basedir)
                    (file-name-as-directory "json-files")
                    "editing"))
      (dolist (json-file (file-expand-wildcards "*.json"))
        (redisplay)
        (with-temp-buffer
          (switch-to-buffer (current-buffer))
          (let ((coding-system-for-read 'utf-8-unix))
            (insert-file-contents json-file))
          (setq current-line 0)
          (while (not (eobp))
            (when (not noninteractive)
              (progress-reporter-update progress-reporter))
            (setq current-line (1+ current-line))
            (cond
             ;; Empty line
             ((= (line-beginning-position) (line-end-position))
              nil)

             ((looking-at " *// *context-start")
              (setq context-start-line current-line)
              (setq actions '())
              (save-excursion
                (forward-line)
                (setq context-start (point))))

             ((looking-at " *// *context-end")
              (setq context-end (point))
              (setq context-text (buffer-substring-no-properties
                                  context-start
                                  context-end)))

             ((looking-at " *// *action: *\\((.*)\\)")
              (push (read (match-string-no-properties 1)) actions))

             ((looking-at " *// *expected-start")
              (save-excursion
                (forward-line)
                (setq expected-start (point))))

             ((looking-at " *// *expected-end")
              (setq expected-end (point))
              (setq expected-text (buffer-substring-no-properties
                                   expected-start
                                   expected-end)))

             ((looking-at " *// *test-end")
              (let* ((status (json-par-test-editing-1
                              json-file
                              context-start-line
                              context-text
                              (reverse actions)
                              expected-text
                              error-buffer))
                     (count-assoc (assq status error-counts)))
                (setcdr count-assoc (1+ (cdr count-assoc))))))
            (forward-line)))))))

(defun json-par-test-editing-1
    (json-file line context-text actions expected-text error-buffer)
  "Run one editing test for command `json-par-mode'.

JSON-FILE is the filename of the current test case.
LINE is the line number of the test case.
CONTEXT-TEXT is the text before applying ACTIONS.
ACTIONS is a list of expressions to be evaluated.
EXPECTED-TEXT is the expected text after applying ACTIONS.
ERROR-BUFFER is the buffer to output errors."
  (let ((status 'ok)
        initial-point
        actual-point
        actual-text
        reversed)
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert context-text)
      (cond
       ;; Simple point
       ((progn
          (goto-char (point-min))
          (search-forward (concat "/*|*/") (point-max) t))
        (delete-region (match-beginning 0) (match-end 0))
        (setq initial-point (point)))

       ;; Active region
       ((progn
          (goto-char (point-min))
          (search-forward (concat "/*[|*/") (point-max) t))
        (delete-region (match-beginning 0) (match-end 0))
        (setq initial-point (point))
        (goto-char (point-min))
        (search-forward (concat "/*]*/") (point-max) t)
        (delete-region (match-beginning 0) (match-end 0))
        (setq initial-point (list initial-point (point))))

       ;; Active region (reversed)
       ((progn
          (goto-char (point-min))
          (search-forward (concat "/*[*/") (point-max) t))
        (delete-region (match-beginning 0) (match-end 0))
        (setq initial-point (point))
        (goto-char (point-min))
        (search-forward (concat "/*|]*/") (point-max) t)
        (delete-region (match-beginning 0) (match-end 0))
        (setq initial-point (list (point) initial-point))))
      (unless initial-point
        (json-par-show-error
         error-buffer json-file line
         "error"
         "editing: no initial point"))
      (jsonc-mode)
      (json-par-mode 1)
      (transient-mark-mode 1)
      (setq-local indent-tabs-mode nil)
      (setq-local js-indent-level 4)
      (if (number-or-marker-p initial-point)
          (goto-char initial-point)
        (goto-char (nth 0 initial-point))
        (set-mark (nth 1 initial-point)))
      (mapc (lambda (action)
              (let ((deactivate-mark nil))
                (eval action)
                (when deactivate-mark
                  (deactivate-mark))))
            actions)
      (if (region-active-p)
          (setq actual-point (list (point) (mark)))
        (setq actual-point (point)))
      (if (number-or-marker-p actual-point)
          (progn
            (goto-char actual-point)
            (insert "/*|*/"))
        (setq reversed (< (nth 1 actual-point) (nth 0 actual-point)))
        (goto-char (max (nth 0 actual-point)
                        (nth 1 actual-point)))
        (insert (if reversed "/*|]*/" "/*]*/"))
        (goto-char (min (nth 0 actual-point)
                        (nth 1 actual-point)))
        (insert (if reversed "/*[*/" "/*[|*/")))
      (setq actual-text (buffer-string))
      (when (not (equal expected-text actual-text))
        (setq status 'error)
        (json-par-show-error
         error-buffer json-file line
         "error"
         (concat
          "editing: expected\n"
          expected-text
          "but\n"
          actual-text)))
      status)))

(provide 'json-par-test-editing)

;;; json-par-test-editing.el ends here
