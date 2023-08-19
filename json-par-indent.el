;;; json-par.el --- Indenting lines in JSON Par mode -*- lexical-binding: t -*-

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

;; Indentation functions for JSON Par mode.
;; Lightweight alternative to `js-indent-function' and
;; `indent-region-line-by-line'.

;;; Code:

(require 'json-par-lexer)
(require 'json-par-motion)

(defvar json-par--fixup-adviced-functions nil
  "Functions to be adviced with `json-par--fixup-advice'.")

(defun json-par--calculate-indent
    (&optional previous-indentation parent-indentation)
  "Return indentation column of the current line.

If PREVIOUS-INDENTATION is non-nil, it is used as the indentation column of the
previous member.

If PARENT-INDENTATION is non-nil, it is used as the indentation column of the
parent member."
  (save-excursion
    (beginning-of-line)
    (let ((string-like-beginning-position
           (json-par--string-like-beginning-position))
          next-token
          previous-token
          (offset (if (boundp 'js-indent-level)
                      (symbol-value 'js-indent-level)
                    2)))
      (if string-like-beginning-position
          ;; Inside strings and comments.
          (current-indentation)
        (if (looking-at "[\s\t]*$")
            ;; The current line is empty.
            ;; Pretend there is a token.
            (setq next-token (json-par-token 'other (point) (point)))
          (setq next-token (save-excursion
                             (json-par-forward-token-or-list-or-comment))))
        (cond
         ;; Indenting a close bracket.
         ((json-par-token-close-bracket-p next-token)
          (or parent-indentation
              (progn
                (goto-char (json-par-token-end next-token))
                (json-par--current-indentation))))

         ;; Indenting a colon.
         ((and (json-par-token-colon-p next-token)
               (save-excursion
                 (json-par--object-key-p (json-par-backward-token))))
          (+ (or previous-indentation (json-par--current-indentation)) offset))

         ;; Otherwise.
         (t
          (setq previous-token (save-excursion (json-par-backward-token)))
          (if (json-par-token-colon-p previous-token)
              (+ (or previous-indentation
                     (json-par--current-indentation))
                 offset)
            (or previous-indentation
                (if (json-par--goto-end-of-previous-member)
                    (json-par--current-indentation)
                  (if (json-par-token-open-bracket-p (json-par-backward-token))
                      (+ (or parent-indentation
                             (json-par--current-indentation))
                         offset)
                    0))))))))))

(defun json-par--current-indentation ()
  "Return the indentation of the current member.

It is the indentation of the current of preceding member at the beginning of a
line or the beginning of the containing array/object."
  (save-excursion
    (json-par-beginning-of-member)
    (json-par--backward-spaces t)
    (while (and (not (bolp))
                (json-par--goto-end-of-previous-member))
      (json-par-beginning-of-member)
      (json-par--backward-spaces t))
    (skip-chars-forward "\s\t")
    (current-column)))

(defun json-par-indent-line ()
  "Indent the current line.

Lightweight alternative to `js-indent-function'."
  (interactive)
  (let* ((indentation-column (json-par--calculate-indent))
         (current-indent
          (save-excursion (back-to-indentation) (current-column))))
    (if (<= (current-column) current-indent)
        ;; The cursor is on the left margin.  Moving to the new indent.
        (indent-line-to indentation-column)
      ;; Keeps current relative position.
      (save-excursion (indent-line-to indentation-column)))))

(push #'json-par-indent-line json-par--fixup-adviced-functions)

(defun json-par-indent-region (start end)
  "Indent the region from START to END."
  (interactive "r")
  (save-excursion
    (let ((indentations '())
          (json-par--already-out-of-comment nil)
          (json-par--already-out-of-atom nil)
          next-token)
      (setq end (copy-marker end))
      (goto-char start)
      (json-par--out-comment)
      (json-par--out-atom)
      (setq json-par--already-out-of-comment t)
      (setq json-par--already-out-of-atom t)
      (while (< (point) end)
        (skip-chars-backward "\s\t")
        (when (bolp)
          (if indentations
              (indent-line-to (json-par--calculate-indent
                               (nth 0 indentations)
                               (nth 1 indentations)))
            (json-par-indent-line)
            (push (json-par--current-indentation) indentations)))
        (setq next-token (json-par-forward-token-or-comment))
        (skip-chars-forward "\s\t\n")
        (cond
         ((json-par-token-open-bracket-p next-token)
          (push
           (if (save-excursion
                 (goto-char (json-par-token-end next-token))
                 (skip-chars-forward "\s\t")
                 (eolp))
               (save-excursion
                 (goto-char (json-par-token-start next-token))
                 (+ (if indentations
                        (car indentations)
                      (json-par--current-indentation))
                    (if (boundp 'js-indent-level)
                        (symbol-value 'js-indent-level)
                      2)))
             (current-column))
           indentations))
         ((json-par-token-close-bracket-p next-token)
          (pop indentations)))))
    (json-par--free-marker end)))

(push #'json-par-indent-region json-par--fixup-adviced-functions)

(provide 'json-par-indent)

;;; json-par-indent.el ends here
