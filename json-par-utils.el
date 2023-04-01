;;; json-par-utils.el --- Utilities for JSON Par mode -*- lexical-binding: t -*-

;; Copyright (C) 2021 taku0
;;
;; Author: taku0 (http://github.com/taku0)
;; URL: https://github.com/taku0/json-par

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

;; Utility functions for JSON Par mode..

;;; Code:

(def-edebug-spec json-par-flat-progn-edebug-spec
  (&or
   [[&or "let" "let*" "letrec" "cl-letf" "cl-letf*"]
    (&rest &or symbolp (gate symbolp &optional form))
    &rest json-par-flat-progn-edebug-spec
    ]
   [[&or "lambda" "cl-lambda*"]
    &rest sexp]
   ;; TODO other functions
   atom
   form))

(defmacro json-par-flat-progn (&rest body)
  "`progn' without last parentheses.

Evaluate expressions in BODY in order.

If BODY contains a symbol, the symbol and the following expressions are wrapped
with parentheses.

Example:

  (json-par-flat-progn
    (a)
    (b)
    save-excursion
    (c)
    if (d) (e)
    (f)
    let ((g h))
    (i))

is expanded to:

  (progn
  (a)
  (b)
  (save-excursion
    (c)
    (if (d)
        (e)
      (f)
      (let ((g h))
        (i)))))"
  (declare (debug (json-par-flat-progn-edebug-spec))
           (indent 0))
  (let* ((result (list 'progn))
         (current result))
    (dolist (form body)
      (if (symbolp form)
          (progn
            (setcdr current (list (list form)))
            (setq current (cadr current)))
        (setcdr current (list form))
        (setq current (cdr current))))
    result))

(defun json-par--free-marker (marker)
  "Make MARKER pointing nowhere and return the old position."
  (prog1 (marker-position marker)
    (set-marker marker nil nil)))

(defun json-par--plist-except (keywords plist)
  "Return copy of PLIST except keys in KEYWORDS and corresponding values.

KEYWORDS can be a symbol rather than a list."
  (when (symbolp keywords)
    (setq keywords (list keywords)))
  (let* ((result (list t))
         (cell result))
    (while plist
      (unless (memq (car plist) keywords)
        (setcdr cell (list (car plist) (cadr plist)))
        (setq cell (cddr cell)))
      (setq plist (cddr plist)))
    (cdr result)))

(defmacro json-par--huge-edit (start end &rest body)
  "Evaluate form BODY with optimizations for huge edits.

Run the change hooks just once like `combine-change-calls'.

Create undo entries as if the contents from START to END are replaced at once.

BODY must not modify buffer outside the region (START END), nor move any markers
out of the region."
  (declare (debug (form form def-body)) (indent 2))
  (let ((start-value (make-symbol "start"))
        (end-value (make-symbol "end")))
    `(let ((,start-value ,start)
           (,end-value ,end))
       ;; WORKAROUND: If buffer-undo-list is nil, combine-change-calls shows
       ;; unnecessary message.
       ;; https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=977630b5285809a57e50ff5f38d9c34247b549a7
       (unless buffer-undo-list
         (push (point) buffer-undo-list))
       (,(if (fboundp 'combine-change-calls)
             'combine-change-calls
           'combine-after-change-calls)
        ,start-value
        ,end-value
        (json-par--huge-edit-1 ,start-value ,end-value (lambda () ,@body))))))

(defun json-par--huge-edit-1 (start end body)
  "Evaluate a function BODY with optimizations for huge edits.

Create undo entries as if the contents from START to END are replaced at once.

BODY must not modify buffer outside the region (START END), nor move any markers
out of the region."
  (let ((old-undo-list buffer-undo-list)
        (undo-inhibit-record-point t)
        deletion-undo-list)
    (buffer-disable-undo)
    (buffer-enable-undo)
    (unwind-protect
        (atomic-change-group
          (delete-region start end)
          (setq deletion-undo-list buffer-undo-list)
          (primitive-undo (length deletion-undo-list) deletion-undo-list))
      (setq buffer-undo-list old-undo-list))
    (setq start (copy-marker start))
    (setq end (copy-marker end))
    (buffer-disable-undo)
    (unwind-protect
        (funcall body)
      (setq buffer-undo-list
            (append (cons
                     (cons (json-par--free-marker start)
                           (json-par--free-marker end))
                     deletion-undo-list)
                    old-undo-list)))))

(defvar-local json-par--dwim-function nil
  "Function to be called by `json-par-dwim'.")

(defun json-par-dwim ()
  "Do what the user means for the previous command."
  (interactive)
  (when json-par--dwim-function
    (funcall json-par--dwim-function)))

(provide 'json-par-utils)

;;; json-par-utils.el ends here
