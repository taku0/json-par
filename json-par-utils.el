;;; json-par-utils.el --- Utilities for JSON Par mode -*- lexical-binding: t -*-

;; Copyright (C) 2021 taku0
;;
;; Authors: taku0 (http://github.com/taku0)
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


(provide 'json-par-utils)

;;; json-par-utils.el ends here
