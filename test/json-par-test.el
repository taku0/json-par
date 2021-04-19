;;; json-par-test.el --- Test for json-par-mode  -*- lexical-binding: t -*-

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

;; Tests for json-par-mode.
;; Execute json-par-run-test interactively or in batch mode.

;;; Code:

;; (add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(require 'json-par-test-motion)
(require 'json-par-test-editing)

(defvar json-par-test-basedir
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar json-par-test-running nil)

(defun json-par-setup-error-buffer ()
  "Initialize and switch to the error buffer.

Return the error-buffer"
  (pop-to-buffer (get-buffer-create "*json-par-test*"))
  (fundamental-mode)
  (setq buffer-read-only nil)
  (erase-buffer)
  (current-buffer))

(defvar json-par-tests
  '(json-par-run-test-motion json-par-run-test-editing))

(defun json-par-run-test (&optional tests)
  "Run TESTS for `json-par-mode'."
  (interactive)
  (setq tests (or tests json-par-tests))
  (let ((error-buffer
         (if noninteractive nil (json-par-setup-error-buffer)))
        (error-counts (list
                       (cons 'error 0)
                       (cons 'warning 0)
                       (cons 'info 0)
                       (cons 'ok 0)))
        (progress-reporter (unless noninteractive
                             (make-progress-reporter "Running tests..."))))
    (setq json-par-test-running t)
    (unwind-protect
        (dolist (test tests)
          (funcall test error-buffer error-counts progress-reporter))
      (setq json-par-test-running nil))
    (when (not noninteractive)
      (progress-reporter-done progress-reporter))
    (json-par-print-message
     error-buffer
     (concat
      "Errors: " (prin1-to-string (assoc-default 'error error-counts)) "\n"
      "Warning: " (prin1-to-string (assoc-default 'warning error-counts)) "\n"
      "Info: " (prin1-to-string (assoc-default 'info error-counts)) "\n"
      "OK: " (prin1-to-string (assoc-default 'ok error-counts)) "\n"))
    (if noninteractive
        (kill-emacs (min 63 (assoc-default 'error error-counts)))
      (compilation-mode))))

(defun json-par-show-error (error-buffer file line level message)
  "Show an error message to the ERROR-BUFFER or stdout.

If the Emacs is in the batch mode, the message is printed to the stdout.
Otherwise, the message is appended to the ERROR-BUFFER.

FILE is the filename of the test case.
LINE is the line number of the error.
LEVEL is the error level (e.g. error, warning).
MESSAGE is the error message."
  (let ((formatted
         (concat
          "json-par-test:"
          file
          ":"
          (prin1-to-string line)
          ": "
          level
          ": "
          message
          "\n")))
    (json-par-print-message error-buffer formatted)))

(defun json-par-print-message (error-buffer message)
  "Print a message to the ERROR-BUFFER or stdout.

If the Emacs is in the batch mode, MESSAGE is printed to the stdout.
Otherwise, MESSAGE is appended to the ERROR-BUFFER."
  (if noninteractive
      (princ message)
    (with-current-buffer error-buffer
      (goto-char (point-max))
      (insert-and-inherit message))))

(provide 'json-par-test)

;;; json-par-test.el ends here
