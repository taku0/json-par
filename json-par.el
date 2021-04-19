;;; json-par.el --- Minor mode for structural editing of JSON -*- lexical-binding: t -*-

;; Copyright (C) 2021 taku0
;;
;; Authors: taku0 (http://github.com/taku0)
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4") (json-mode "1.7.0"))
;; Keywords: abbrev, convenience, files
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

;; Minor mode for structural editing of JSON, inspired by lispy.
;;
;; Features:
;;
;; - Ctrl-less, yet modeless:
;;   If the point is not in a string, each key is interpreted as a command like
;;   the vi editor.  If the point is in a string, keys are inserted as is.
;;
;; - Structural movement:
;;   Moving to the next/previous member, parents, and others.
;;
;; - dabbrev-like completion:
;;   Press `?' to insert a guessed value or key at the point.
;;
;; - Converting single-line to/from multiline:
;;   [ 1, 2, 3 ]
;;
;;   to/from
;;
;;   [
;;     1,
;;     2,
;;     3
;;   ]
;;
;; - Cloning members:
;;   Clone the current/parent/ancestor member with or without values.
;;
;; - Marking/deleting various things:
;;   Mark or delete the current value, current member, parent, siblings, and
;;   others.
;;
;; - And More!

;;; Code:

(require 'json-mode)
(require 'json-par-utils)
(require 'json-par-keymap)
(require 'json-par-lexer)
(require 'json-par-motion)
(require 'json-par-insert)
(require 'json-par-delete)
(require 'json-par-clone)
(require 'json-par-mark-narrow)
(require 'json-par-oneline-multiline)
(require 'json-par-raise)
(require 'json-par-split-join)
(require 'json-par-transpose)
(require 'json-par-guess)
(require 'json-par-indent)


;;; Customizations

;;;###autoload
(defgroup json-par nil
  "Minor mode for structural editing of JSON."
  :group 'json-mode
  :prefix "json-par-")


;;; post-self-insert

(defun json-par--post-self-insert ()
  "Miscellaneous logic for electric indentation.

Call `json-par--post-newline' after line break."
  (cond
   ((and electric-indent-mode (= last-command-event ?\n))
    (json-par--post-newline))))


;;; Disabling JSON Par mode

(defun json-par-disable-temporary ()
  "Disable JSON Par mode until executing the next command."
  (interactive)
  (json-par-mode 0)
  (add-hook 'post-command-hook #'json-par--reenable nil t))

(defun json-par--reenable ()
  "Reenable JSON Par mode after `json-par-disable-temporary'."
  (unless (eq this-command 'json-par-disable-temporary)
    (json-par-mode)
    (remove-hook 'post-command-hook #'json-par--reenable t)))

(defun json-par-quit ()
  "Disable JSON Par mode."
  (interactive)
  (json-par-mode 0))


;;; Keymap

(defvar json-par-mode-map
  (let ((map (make-sparse-keymap)))
    (json-par-define-special-key map "j" #'json-par-forward-member)
    (json-par-define-special-key map "k" #'json-par-backward-member)
    (json-par-define-special-key map "J" #'json-par-forward-record)
    (json-par-define-special-key map "K" #'json-par-backward-record)
    (json-par-define-special-key map "h" #'json-par-up-backward)
    (json-par-define-special-key map "l" #'json-par-up-forward)
    (json-par-define-special-key map "i" #'json-par-down)
    (json-par-define-special-key map "a" #'json-par-beginning-of-member)
    (json-par-define-special-key map "e" #'json-par-end-of-member
                                 #'json-par--special-for-e-p)
    (json-par-define-special-key map "A" #'json-par-beginning-of-list)
    (json-par-define-special-key map "E" #'json-par-end-of-list)
    (json-par-define-special-key map "v" #'json-par-beginning-of-object-value)
    (json-par-define-special-key map "V" #'json-par-delete-object-values)
    (json-par-define-special-key map "b" #'pop-to-mark-command)
    (json-par-define-special-key map "g" #'json-par-goto-key)
    (json-par-define-special-key map "t" #'json-par-insert-true)
    (json-par-define-special-key map "f" #'json-par-insert-false)
    (json-par-define-special-key map "n" #'json-par-insert-null)
    (json-par-define-special-key map "[" #'json-par-insert-square-brackets)
    (json-par-define-special-key map "]" #'json-par-up-forward)
    (json-par-define-special-key map "{" #'json-par-insert-curly-brackets)
    (json-par-define-special-key map "}" #'json-par-up-forward)
    (define-key map "\"" #'json-par-insert-double-quotes)
    (json-par-define-special-key map "," #'json-par-insert-comma)
    (json-par-define-special-key map ":" #'json-par-insert-colon)
    (dolist (key (list "-" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
      (json-par-define-special-key
       map
       key
       #'json-par-insert-self-as-number
       #'json-par--special-but-not-number-p))
    (json-par-define-special-key map "M" #'json-par-multiline)
    (json-par-define-special-key map "O" #'json-par-oneline)
    (define-key
      map
      [remap delete-backward-char]
      #'json-par-delete-backward-char)
    (define-key
      map
      [remap delete-forward-char]
      #'json-par-delete-forward-char)
    (define-key
      map
      [remap delete-char]
      #'json-par-delete-forward-char)
    (json-par-define-special-key map "m" #'json-par-mark-more)
    (json-par-define-special-key map "N" #'json-par-narrow)
    (define-key
      map
      [remap mark-sexp]
      #'json-par-mark-more)
    (json-par-define-special-key map "S" #'json-par-split)
    (json-par-define-special-key map "F" #'json-par-join)
    (json-par-define-special-key map "c" #'json-par-clone-prefix-command)
    (json-par-define-special-key
     map
     "C"
     #'json-par-clone-without-value-prefix-command)
    (json-par-define-special-key map "w" #'json-par-transpose-member-backward)
    (json-par-define-special-key map "s" #'json-par-transpose-member-forward)
    (json-par-define-special-key map "r" #'json-par-raise-member)
    (json-par-define-special-key map "?" #'json-par-insert-guessed)
    (json-par-define-special-key map "q" #'json-par-disable-temporary)
    (json-par-define-special-key map "Q" #'json-par-quit)
    (json-par-define-special-key map "d" #'json-par-delete-prefix-command)
    (define-key map "\t" #'json-par-tab)
    (define-key map '[backtab] #'json-par-mark-head-of-member)
    (define-key map (kbd "S-TAB") #'json-par-mark-head-of-member)
    map)
  "Keymap for JSON Par mode.")


;;; Entry point

(defvar-local json-par-old-forward-sexp-function nil
  "`forward-sexp-function' before JSON Par mode is enabled.

It is restored when JSON Par mode is disabled.")

(defvar-local json-par-old-indent-line-function nil
  "`indent-line-function' before JSON Par mode is enabled.

It is restored when JSON Par mode is disabled.")

(defvar-local json-par-old-indent-region-function nil
  "`indent-region-function' before JSON Par mode is enabled.

It is restored when JSON Par mode is disabled.")

;;;###autoload
(define-minor-mode json-par-mode
  "Toggle minor mode for structural editing of JSON.

\\{json-par-mode-map}"
  :keymap json-par-mode-map
  :group 'json-par
  :lighter " Par"
  (if json-par-mode
      (progn
        (let ((cell (assq 'json-par-mode minor-mode-map-alist)))
          (when cell
            (setq minor-mode-map-alist
                  (cons cell (delq cell minor-mode-map-alist)))))
        (setq-local json-par-old-forward-sexp-function forward-sexp-function)
        (setq-local forward-sexp-function #'json-par-forward-sexp)
        (setq-local json-par-old-indent-line-function indent-line-function)
        (setq-local indent-line-function #'json-par-indent-line)
        (setq-local indent-region-function #'json-par-indent-region)
        (add-hook 'post-self-insert-hook #'json-par--post-self-insert nil t)
        (json-par--add-install-advice-to-deferred-hook-functions-to-hooks)
        (json-par--install-advice-to-deferred-hook-functions))
    (when (eq forward-sexp-function #'json-par-forward-sexp)
      (setq-local forward-sexp-function json-par-old-forward-sexp-function))
    (when (eq indent-line-function #'json-par-indent-line)
      (setq-local indent-line-function json-par-old-indent-line-function))
    (when (eq indent-region-function #'json-par-indent-region)
      (setq-local indent-region-function json-par-old-indent-region-function))
    (remove-hook 'post-self-insert-hook #'json-par--post-self-insert t)
    (json-par--remove-advice-from-deferred-hook-functions)))

(provide 'json-par)

;;; json-par.el ends here
