;;; json-par-transpose.el --- Transposing in JSON Par mode -*- lexical-binding: t -*-

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

;; Functions for transposing members in JSON Par mode.

;;; Code:

(require 'json-par-lexer)
(require 'json-par-motion)

(defun json-par-transpose-member-backward (&optional arg)
  "Interchange the current member with the previous one.

With ARG, repeat that times.  If ARG is negative, transpose forward.

If the current member is the first one, do nothing."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (if (< arg 0)
      (json-par-transpose-member-forward (- arg))
    (dotimes (_ arg)
      (json-par--transpose-member-backward-1))))

(defun json-par-transpose-member-forward (&optional arg)
  "Interchange the current member with the next one.

With ARG, repeat that times.  If ARG is negative, transpose backward.

If the current member is the last one, do nothing."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (if (< arg 0)
      (json-par-transpose-member-backward (- arg))
    (dotimes (_ arg)
      (json-par--transpose-member-forward-1))))

(defun json-par--transpose-member-backward-1 ()
  "Interchange the current member with the previous one.

If the current member is the first one, do nothing."
  (let* ((start-of-current-member (save-excursion
                                    (json-par-beginning-of-member)
                                    (json-par--backward-spaces)
                                    (skip-chars-forward "\s\t\n")
                                    (point)))
         (end-of-current-member (save-excursion
                                  (goto-char start-of-current-member)
                                  (json-par-end-of-member)
                                  (json-par--forward-spaces)
                                  (skip-chars-backward "\s\t\n")
                                  (point)))
         (offset (- (point) start-of-current-member))
         (start-of-previous-member (save-excursion
                                     (json-par-backward-member)
                                     (json-par-beginning-of-member)
                                     (json-par--backward-spaces)
                                     (skip-chars-forward "\s\t\n")
                                     (point)))
         (end-of-previous-member (save-excursion
                                   (goto-char start-of-previous-member)
                                   (json-par-end-of-member)
                                   (json-par--forward-spaces)
                                   (skip-chars-backward "\s\t\n")
                                   (point))))
    (unless (= start-of-current-member start-of-previous-member)
      (transpose-regions
       start-of-current-member
       end-of-current-member
       start-of-previous-member
       end-of-previous-member)
      (goto-char (+ start-of-previous-member offset)))))

(defun json-par--transpose-member-forward-1 ()
  "Interchange the current member with the next one.

If the current member is the last one, do nothing."
  (let* ((start-of-current-member (save-excursion
                                    (json-par-beginning-of-member)
                                    (json-par--backward-spaces)
                                    (skip-chars-forward "\s\t\n")
                                    (point)))
         (end-of-current-member (save-excursion
                                  (goto-char start-of-current-member)
                                  (json-par-end-of-member)
                                  (json-par--forward-spaces)
                                  (skip-chars-backward "\s\t\n")
                                  (point)))
         (offset (- (point) end-of-current-member))
         (start-of-next-member (save-excursion
                                 (json-par-forward-member)
                                 (json-par-beginning-of-member)
                                 (json-par--backward-spaces)
                                 (skip-chars-forward "\s\t\n")
                                 (point)))
         (end-of-next-member (save-excursion
                               (goto-char start-of-next-member)
                               (json-par-end-of-member)
                               (json-par--forward-spaces)
                               (skip-chars-backward "\s\t\n")
                               (point))))
    (unless (= start-of-current-member start-of-next-member)
      (transpose-regions
       start-of-current-member
       end-of-current-member
       start-of-next-member
       end-of-next-member)
      (goto-char (+ end-of-next-member offset)))))


(provide 'json-par-transpose)

;;; json-par-transpose.el ends here
