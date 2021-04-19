;;; json-par-test-motion.el --- Test for json-par-mode: motion  -*- lexical-binding: t -*-

;; Copyright (C) 2021 taku0

;; Authors: taku0 (http://github.com/taku0)

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

;; Test for json-par-mode: motion.
;; Execute json-par-run-test-motion interactively or in batch mode.

;;; Code:

(require 'json-mode)
(require 'json-par)

(defun json-par-run-test-motion
    (&optional error-buffer error-counts progress-reporter)
  "Run motion test for `json-par-mode'.

ERROR-BUFFER is the buffer to output errors.
ERROR-COUNTS is a association list holding counts of errors. Updated
destructively.
PROGRESS-REPORTER is the progress-reporter."
  (interactive)
  (if (not json-par-test-running)
      (json-par-run-test '(json-par-run-test-motion))
    (let ((current-line 0)
          (context-start-line 0)
          (context-start 0)
          (context-end 0)
          (context-text nil)
          (actions '()))
      (setq default-directory
            (concat (file-name-as-directory json-par-test-basedir)
                    (file-name-as-directory "json-files")
                    "motion"))
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

             ((looking-at " *// *test-end")
              (let* ((status (json-par-test-motion-1
                              json-file
                              context-start-line
                              context-text
                              (reverse actions)
                              error-buffer))
                     (count-assoc (assq status error-counts)))
                (setcdr count-assoc (1+ (cdr count-assoc))))))
            (forward-line)))))))

(defun json-par-test-motion-1
    (json-file line context-text actions error-buffer)
  "Run one motion test for command `json-par-mode'.

JSON-FILE is the filename of the current test case.
LINE is the line number of the test case.
CONTEXT-TEXT is the text before applying ACTIONS.
ACTIONS is a list of expressions to be evaluated.
EXPECTED-TEXT is the expected text after applying ACTIONS.
ERROR-BUFFER is the buffer to output errors."
  (let ((status 'ok)
        (i 0)
        (expected-points '())
        (actual-points '())
        region-beginning
        reversed)
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert context-text)
      (while (progn
               (goto-char (point-min))
               (search-forward (concat "/*" (prin1-to-string i))
                               (point-max)
                               t))
        (setq region-beginning (match-beginning 0))
        (cond
         ;; Simple point
         ((looking-at "\\*/")
          (delete-region region-beginning (match-end 0))
          (push (point-marker) expected-points)
          (setq i (1+ i)))

         ;; Active region
         ((looking-at "\\[\\(|\\)?\\*/")
          (setq reversed (not (match-beginning 1)))
          (delete-region region-beginning (match-end 0))
          (search-forward (concat
                           "/*"
                           (if reversed "|" "")
                           "]"
                           (prin1-to-string i)
                           "*/")
                          (point-max))
          (delete-region (match-beginning 0) (match-end 0))
          (push (if reversed
                    (list (point-marker) (copy-marker region-beginning))
                  (list (copy-marker region-beginning) (point-marker)))
                expected-points)
          (setq i (1+ i)))

         ;; Other junks; ignored
         (t nil)))
      (setq expected-points (reverse expected-points))
      (jsonc-mode)
      (json-par-mode 1)
      (transient-mark-mode 1)
      (if (markerp (car expected-points))
          (goto-char (car expected-points))
        (goto-char (nth 0 (car expected-points)))
        (set-mark (nth 1 (car expected-points))))
      (push (car expected-points) actual-points)
      (dolist (action actions)
        (eval action)
        (if (region-active-p)
            (push (list (point-marker) (copy-marker (mark)))
                  actual-points)
          (push (point-marker) actual-points)))
      (setq actual-points (reverse actual-points))
      (when (not (equal expected-points actual-points))
        (setq status 'error)
        (setq i 0)
        (dolist (actual-point actual-points)
          (if (markerp actual-point)
              (progn
                (goto-char actual-point)
                (insert-before-markers "/*"
                                       (prin1-to-string i)
                                       "*/"))
            (setq reversed (< (nth 1 actual-point) (nth 0 actual-point)))
            (goto-char (min (nth 0 actual-point)
                            (nth 1 actual-point)))
            (insert-before-markers "/*"
                                   (prin1-to-string i)
                                   "["
                                   (if reversed "" "|")
                                   "*/")
            (goto-char (max (nth 0 actual-point)
                            (nth 1 actual-point)))
            (insert-before-markers "/*"
                                   (if reversed "|" "")
                                   "]"
                                   (prin1-to-string i)
                                   "*/"))
          (setq i (1+ i)))
        (json-par-show-error
         error-buffer json-file line
         "error"
         (concat
          "motion: expected\n"
          context-text
          "but\n"
          (buffer-string))))
      status)))

(provide 'json-par-test-motion)

;;; json-par-test-motion.el ends here
