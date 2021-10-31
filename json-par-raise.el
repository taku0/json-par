;;; json-par-raise.el --- Raising member in JSON Par mode -*- lexical-binding: t -*-

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

;; Functions for replaceing the parent member with the current member.

;;; Code:

(require 'json-par-utils)
(require 'json-par-motion)

(defun json-par-raise-member (&optional arg)
  "Replace the parent member with the current member or selection.

If ARG is given, repeat that time."
  (interactive "p")
  (let* ((start (if (use-region-p)
                    (region-beginning)
                  (save-excursion
                    (json-par-beginning-of-member)
                    (json-par--backward-spaces)
                    (skip-chars-forward "\s\t\n")
                    (point))))
         (end (if (use-region-p)
                  (region-end)
                (save-excursion
                  (goto-char start)
                  (json-par-end-of-member)
                  (json-par--forward-spaces)
                  (skip-chars-backward "\s\t\n")
                  (point))))
         start-of-parent
         end-of-parent)
    (json-par-up-backward arg)
    (json-par-beginning-of-member)
    (setq start-of-parent (point-marker))
    (setq end-of-parent
          (save-excursion
            (json-par-end-of-member)
            (point-marker)))
    (transpose-regions (point) (point) start end)
    (goto-char (- start-of-parent (- end start)))
    (delete-region (json-par--free-marker start-of-parent)
                   (json-par--free-marker end-of-parent))))


(provide 'json-par-raise)

;;; json-par-raise.el ends here
