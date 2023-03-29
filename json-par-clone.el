;;; json-par-clone.el --- Cloning members in JSON Par mode -*- lexical-binding: t -*-

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

;; Functions for cloning members in JSON Par mode.

;;; Code:

(require 'json-par-lexer)
(require 'json-par-motion)
(require 'json-par-insert)
(require 'json-par-keymap)
(require 'json-par-delete)


;;; Variables

(defvar-local json-par--clone-level-locations '()
  "List of the locations of the ancestor members to be cloned.

The first element is the start position of the member to be cloned.  The last
element is the start position the parent member.")

(defvar-local json-par--clone-level-overlay nil
  "Overlay to indicate the member to be cloned.")

(defvar json-par--fixup-adviced-functions nil
  "Functions to be adviced with `json-par--fixup-advice'.")

;;; Functions

(defun json-par-clone-member-forward
    (&optional arg level delete-values mark-key)
  "Clone the current member and insert after the current member.

If ARG is given, repeat that times.  If the ARG is negative, insert before
the current member.

If LEVEL is non-nil, clone LEVEL-th parent instead.  Use
`json-par-clone-increase-level' to set LEVEL for interactive use.

If DELETE-VALUES is non-nil and the cloned member is an object/array, delete the
values of the object/array but not the keys, colons, nor commas.  If LEVEL is
positive, delete only the values of the current innermost object.  If LEVEL is
nil or zero, delete the values of the cloned member.

Move to the last member cloned.

If the cloned member has an object key and MARK-KEY is non-nil or called
interactively, mark the contents of the key."
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)
    (length json-par--clone-level-locations)
    nil
    t))
  (unless arg
    (setq arg 1))
  (if (< arg 0)
      (json-par-clone-member-backward (- arg) level delete-values mark-key)
    (let (after-cloned)
      (dotimes (_ arg)
        (setq after-cloned (json-par--clone-member-1 t level delete-values)))
      (when (and mark-key
                 after-cloned
                 (save-excursion
                   (goto-char after-cloned)
                   (json-par-beginning-of-member)
                   (json-par--object-key-p (json-par-forward-token))))
        (goto-char after-cloned)
        (json-par-beginning-of-member)
        (json-par--delete-or-mark-string
         (save-excursion (json-par-forward-token))
         'mark-inner
         'forward
         t)))))

(push #'json-par-clone-member-forward json-par--fixup-adviced-functions)

(defun json-par-clone-member-backward
    (&optional arg level delete-values mark-key)
  "Clone the current member and insert before the current member.

If ARG is given, repeat that times.  If the ARG is negative, insert after
the current member.

If LEVEL is non-nil, clone LEVEL-th parent instead.  Use
`json-par-clone-increase-level' to set LEVEL for interactive use.

If DELETE-VALUES is non-nil and the cloned member is an object/array, delete the
values of the object/array but not the keys, colons, nor commas.  If LEVEL is
positive, delete only the values of the current innermost object.  If LEVEL is
nil or zero, delete the values of the cloned member.

Move to the last member cloned.

If the cloned member has an object key and MARK-KEY is non-nil or called
interactively, mark the contents of the key."
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)
    (length json-par--clone-level-locations)
    nil
    t))
  (unless arg
    (setq arg 1))
  (if (< arg 0)
      (json-par-clone-member-forward (- arg) level delete-values mark-key)
    (let (after-cloned)
      (dotimes (_ arg)
        (setq after-cloned (json-par--clone-member-1 nil level delete-values)))
      (when (and mark-key
                 after-cloned
                 (save-excursion
                   (goto-char after-cloned)
                   (json-par-beginning-of-member)
                   (json-par--object-key-p (json-par-forward-token))))
        (goto-char after-cloned)
        (json-par-beginning-of-member)
        (json-par--delete-or-mark-string
         (save-excursion (json-par-forward-token))
         'mark-inner
         'forward
         t)))))

(push #'json-par-clone-member-backward json-par--fixup-adviced-functions)

(defun json-par--clone-member-1 (forward level delete-values)
  "Clone the current member and insert before/after the current member.

If FORWARD is non-nil, insert after the current member.  Insert before it
otherwise.

If LEVEL is non-nil, clone LEVEL-th parent instead.

If DELETE-VALUES is non-nil and the cloned member is an object/array, delete the
values of the object/array but not the keys, colons, nor commas.  If LEVEL is
positive, delete only the values of the current innermost object.  If LEVEL is
nil or zero, delete the values of the cloned member.

Move to the cloned member.

Return the location after the cloned member."
  (let* ((pos (point))
         start
         end
         offset
         member-string
         after-cloned)
    (json-par--out-comment)
    (json-par--out-atom)
    (when level
      (json-par-up-backward level))
    (setq start (progn
                  (json-par-beginning-of-member)
                  (json-par--backward-spaces)
                  (skip-chars-forward "\s\t\n")
                  (point)))
    (setq end (progn
                (goto-char start)
                (json-par-end-of-member)
                (json-par--forward-spaces)
                (skip-chars-backward "\s\t\n")
                (point)))
    (setq offset (- pos end))
    (setq member-string (buffer-substring start end))
    (goto-char (if forward end start))
    (json-par--insert-value member-string t t)
    (setq after-cloned (copy-marker (point)))
    (goto-char (+ (point) offset))
    (when delete-values
      (save-excursion
        (if (or (null level) (zerop level))
            (progn
              (goto-char after-cloned)
              (when (member (char-before) '(?} ?\]))
                (backward-char)
                (json-par-delete-object-values t)))
          (json-par-delete-object-values t))))
    (json-par--free-marker after-cloned)))

(defun json-par-clone-member-forward-without-value
    (&optional arg level mark-key)
  "Clone the current member and insert after it without values inside.

If ARG is given, repeat that times.  If the ARG is negative, insert before
the current member.

If LEVEL is non-nil, clone LEVEL-th parent instead.

If the cloned member is an object/array, delete the values of the object/array
but not the keys, colons, nor commas.  If LEVEL is positive, delete only the
values of the current innermost object.  If LEVEL is nil or zero, delete the
values of the cloned member.

Move to the last member cloned.

If the cloned member has an object key and MARK-KEY is non-nil or called
interactively, mark the contents of the key."
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)
    (length json-par--clone-level-locations)
    t))
  (json-par-clone-member-forward arg level t mark-key))

(push #'json-par-clone-member-forward-without-value
      json-par--fixup-adviced-functions)

(defun json-par-clone-member-backward-without-value
    (&optional arg level mark-key)
  "Clone the current member and insert before it without values inside.

If ARG is given, repeat that times.  If the ARG is negative, insert after
the current member.

If LEVEL is non-nil, clone LEVEL-th parent instead.

If the cloned member is an object/array, delete the values of the object/array
but not the keys, colons, nor commas.  If LEVEL is positive, delete only the
values of the current innermost object.  If LEVEL is nil or zero, delete the
values of the cloned member.

Move to the last member cloned.

If the cloned member has an object key and MARK-KEY is non-nil or called
interactively, mark the contents of the key."
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)
    (length json-par--clone-level-locations)
    t))
  (json-par-clone-member-backward arg level t mark-key))

(push #'json-par-clone-member-backward-without-value
      json-par--fixup-adviced-functions)

(defun json-par-clone-parent-forward
    (&optional arg level delete-values mark-key)
  "Clone the containing object/array and insert after it.

If ARG is given, repeat that times.  If the ARG is negative, insert before
the containing object/array.

If LEVEL is non-nil, clone (1+ LEVEL)-th parent instead.

If DELETE-VALUES is non-nil, delete the values of the object/array but not the
keys, colons, nor commas.  If LEVEL is non-nil, delete only the values of the
current innermost object.

Move to the same member in the last object/array cloned.

If the cloned member has an object key and MARK-KEY is non-nil or called
interactively, mark the contents of the key."
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)
    (length json-par--clone-level-locations)
    nil
    t))
  (json-par-clone-member-forward arg (1+ (or level 0)) delete-values mark-key))

(push #'json-par-clone-parent-forward json-par--fixup-adviced-functions)

(defun json-par-clone-parent-backward
    (&optional arg level delete-values mark-key)
  "Clone the containing object/array and insert before it.

If ARG is given, repeat that times.  If the ARG is negative, insert after
the containing object/array.

If LEVEL is non-nil, clone (1+ LEVEL)-th parent instead.

If DELETE-VALUES is non-nil, delete the values of the object/array but not the
keys, colons, nor commas.  If LEVEL is non-nil, delete only the values of the
current innermost object.

Move to the same member in the last object/array cloned.

If the cloned member has an object key and MARK-KEY is non-nil or called
interactively, mark the contents of the key."
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)
    (length json-par--clone-level-locations)
    nil
    t))
  (json-par-clone-member-backward arg (1+ (or level 0)) delete-values mark-key))

(push #'json-par-clone-parent-backward json-par--fixup-adviced-functions)

(defun json-par-clone-parent-forward-without-value
    (&optional arg level mark-key)
  "Clone the containing object/array and insert after it without values.

If ARG is given, repeat that times.  If the ARG is negative, insert before
the containing object/array.

If LEVEL is non-nil, clone (1+ LEVEL)-th parent instead.  Delete only the
values of the current innermost object.

Move to the same member in the last object/array cloned.

If the cloned member has an object key and MARK-KEY is non-nil or called
interactively, mark the contents of the key."
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)
    (length json-par--clone-level-locations)
    t))
  (json-par-clone-parent-forward arg level t mark-key))

(push #'json-par-clone-parent-forward-without-value
      json-par--fixup-adviced-functions)

(defun json-par-clone-parent-backward-without-value
    (&optional arg level mark-key)
  "Clone the containing object/array and insert before it without values.

If ARG is given, repeat that times.  If the ARG is negative, insert after
the containing object/array.

If LEVEL is non-nil, clone (1+ LEVEL)-th parent instead.  Delete only the
values of the current innermost object.

Move to the same member in the last object/array cloned.

If the cloned member has an object key and MARK-KEY is non-nil or called
interactively, mark the contents of the key."
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)
    (length json-par--clone-level-locations)
    t))
  (json-par-clone-parent-backward arg level t mark-key))

(push #'json-par-clone-parent-backward-without-value
      json-par--fixup-adviced-functions)


;;; Keymaps

(defvar json-par-clone-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'json-par-clone-member-forward)
    (define-key map "v" #'json-par-clone-without-value-prefix-command)
    (define-key map "j" #'json-par-clone-member-forward)
    (define-key map "k" #'json-par-clone-member-backward)
    (define-key map "h" #'json-par-clone-increase-level-prefix-command)
    (define-key map "l" #'json-par-clone-increase-level-prefix-command)
    (define-key map "i" #'json-par-clone-decrease-level-prefix-command)
    (define-key map "b" #'json-par-clone-decrease-level-prefix-command)
    (define-key map "J" #'json-par-clone-parent-forward)
    (define-key map "K" #'json-par-clone-parent-backward)
    (dolist (prefix '("" "C-" "M-" "C-M-"))
      (dolist (key '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
        (define-key
          map
          (kbd (concat prefix key))
          #'json-par-prefix-digit-argument))
      (define-key
        map
        (kbd (concat prefix "-"))
        #'json-par-prefix-negative-argument))
    (define-key map "\C-u" #'json-par-prefix-universal-argument)
    (define-key map [t] #'json-par-prefix-default)
    map)
  "Temporary keymap for `json-par-clone-prefix-command'.")

(defvar json-par-clone-without-value-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'json-par-clone-member-forward-without-value)
    (define-key map "C" #'json-par-clone-member-forward-without-value)
    (define-key map "j" #'json-par-clone-member-forward-without-value)
    (define-key map "k" #'json-par-clone-member-backward-without-value)
    (define-key map "h" #'json-par-clone-increase-level-prefix-command)
    (define-key map "l" #'json-par-clone-increase-level-prefix-command)
    (define-key map "i" #'json-par-clone-decrease-level-prefix-command)
    (define-key map "b" #'json-par-clone-decrease-level-prefix-command)
    (define-key map "J" #'json-par-clone-parent-forward-without-value)
    (define-key map "K" #'json-par-clone-parent-backward-without-value)
    (dolist (prefix '("" "C-" "M-" "C-M-"))
      (dolist (key '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
        (define-key
          map
          (kbd (concat prefix key))
          #'json-par-prefix-digit-argument))
      (define-key
        map
        (kbd (concat prefix "-"))
        #'json-par-prefix-negative-argument))
    (define-key map "\C-u" #'json-par-prefix-universal-argument)
    (define-key map [t] #'json-par-prefix-default)
    map)
  "Temporary keymap for `json-par-clone-without-value-prefix-command'.")


;;; Handling key sequence for cloning members

;; Commands for cloning members can be invoked with key sequence.
;;
;; Examples:
;;
;; - Key sequence "c j": clone the current member below.
;; - Key sequence "C J": clone the parent object below without values.
;; - Key sequence "c v h h k":
;;     clone the grand-parent object above without values.
;;
;; This is implemented with temporary keymap installed by `set-transient-map'
;; via `json-par-prefix-command'.
;;
;; "h" key and "i" key, mapped to
;; `json-par-clone-increase-level-prefix-command' and
;; `json-par-clone-decrease-level-prefix-command' respectively, sets the
;; level parameter.  Those commands also shows an overlay to indicate the member
;; to be cloned.
;;
;; The level parameter and overlay is valid only for the immediately following
;; invocation of the cloning commands, and used only if invoked
;; interactively.
;;
;; Why not `read-multiple-choice':
;;
;; - I don't like the style of the help message.
;; - The keymap should be customizable.


(defun json-par-clone-prefix-command (arg)
  "Clone current member or parent object/array.

Actually, set a temporary keymap `json-par-clone-prefix-map' to read direction
and target.

ARG is passed to the commands bound in the keymap."
  (interactive "P")
  (json-par-prefix-command
   arg
   (when (called-interactively-p 'interactive)
     "clone (? for help)%s: ")
   (concat
    "Clone current member or parent object/array.

Actually, set the following temporary keymap to read direction and target.\n\n"
    (json-par--format-command-help json-par-clone-prefix-map))
   json-par-clone-prefix-map))

(defun json-par-clone-without-value-prefix-command (arg)
  "Clone current member or parent object/array without values inside.

Actually, set a temporary keymap `json-par-clone-without-value-prefix-map'
to read direction and target.

ARG is passed to the commands bound in the keymap."
  (interactive "P")
  (json-par-prefix-command
   arg
   (when (called-interactively-p 'interactive)
     "clone without value (? for help)%s: ")
   (concat
    "Clone current member or parent object/array without values.

Actually, set the following temporary keymap to read direction and target.\n\n"
    (json-par--format-command-help json-par-clone-without-value-prefix-map))
   json-par-clone-without-value-prefix-map))

(defun json-par-clone-increase-level ()
  "Increase the level parameter of the next invocation of cloning commands.

Those commands are affected:

- `json-par-clone-member-forward'
- `json-par-clone-member-backward'
- `json-par-clone-member-forward-without-value'
- `json-par-clone-member-backward-without-value'
- `json-par-clone-parent-forward'
- `json-par-clone-parent-backward'
- `json-par-clone-parent-forward-without-value'
- `json-par-clone-parent-backward-without-value'

Only immediately following invocation is affected.

Intended to be invoked directory by user, like \N{U+4D}-x
json-par\N{U+2D}clone-increase-level or other custom key bindings."
  (interactive)
  (unless json-par--clone-level-overlay
    (setq json-par--clone-level-overlay
          (make-overlay (point) (point)))
    (overlay-put json-par--clone-level-overlay 'display "*")
    (overlay-put json-par--clone-level-overlay 'face 'highlight))
  (save-excursion
    (goto-char (or (car json-par--clone-level-locations) (point)))
    (json-par--out-comment)
    (json-par--out-atom)
    (json-par-up-backward)
    (push (point) json-par--clone-level-locations)
    (move-overlay json-par--clone-level-overlay (point) (1+ (point)))
    (add-hook
     'post-command-hook
     #'json-par--delete-clone-level-overlay
     nil
     t)))

(defun json-par-clone-increase-level-prefix-command (arg)
  "Increase the level parameter of the next invocation of cloning commands.

Those commands are affected:

- `json-par-clone-member-forward'
- `json-par-clone-member-backward'
- `json-par-clone-member-forward-without-value'
- `json-par-clone-member-backward-without-value'
- `json-par-clone-parent-forward'
- `json-par-clone-parent-backward'
- `json-par-clone-parent-forward-without-value'
- `json-par-clone-parent-backward-without-value'

Only immediately following invocation is affected.

ARG is passed to the next command.

Intended to be invoked via the keymap of `json-par-clone-prefix-command' or
`json-par-clone-without-value-prefix-command'."
  (interactive "P")
  (json-par-clone-increase-level)
  (json-par-prefix-command
   arg
   json-par--prefix-prompt
   json-par--prefix-help-form
   json-par--prefix-keymap))

(defun json-par-clone-decrease-level ()
  "Decrease the level parameter of the next invocation of cloning commands.

Those commands are affected:

- `json-par-clone-member-forward'
- `json-par-clone-member-backward'
- `json-par-clone-member-forward-without-value'
- `json-par-clone-member-backward-without-value'
- `json-par-clone-parent-forward'
- `json-par-clone-parent-backward'
- `json-par-clone-parent-forward-without-value'
- `json-par-clone-parent-backward-without-value'

Only immediately following invocation is affected.

Intended to be invoked directory by user, like \N{U+4D}-x
json-par\N{U+2D}clone-decrease-level or other custom key bindings."
  (interactive)
  (when json-par--clone-level-locations
    (pop json-par--clone-level-locations)
    (let ((pos (car json-par--clone-level-locations)))
      (when pos
        (move-overlay json-par--clone-level-overlay pos (1+ pos))
        (add-hook
         'post-command-hook
         #'json-par--delete-clone-level-overlay
         nil
         t))))
  (unless json-par--clone-level-locations
    (delete-overlay json-par--clone-level-overlay)))

(defun json-par-clone-decrease-level-prefix-command (arg)
  "Decrease the level parameter of the next invocation of cloning commands.

Those commands are affected:

- `json-par-clone-member-forward'
- `json-par-clone-member-backward'
- `json-par-clone-member-forward-without-value'
- `json-par-clone-member-backward-without-value'
- `json-par-clone-parent-forward'
- `json-par-clone-parent-backward'
- `json-par-clone-parent-forward-without-value'
- `json-par-clone-parent-backward-without-value'

Only immediately following invocation is affected.

ARG is passed to the next command.

Intended to be invoked via the keymap of `json-par-clone-prefix-command' or
`json-par-clone-without-value-prefix-command'."
  (interactive "P")
  (json-par-clone-decrease-level)
  (json-par-prefix-command
   arg
   json-par--prefix-prompt
   json-par--prefix-help-form
   json-par--prefix-keymap))

(defun json-par--delete-clone-level-overlay ()
  "Delete overlay installed by `json-par-clone-increase-level'.

Intended to be a `post-command-hook'."
  (unless (memq this-command
                '(json-par-clone-increase-level
                  json-par-clone-decrease-level
                  json-par-clone-increase-level-prefix-command
                  json-par-clone-decrease-level-prefix-command
                  json-par-clone-prefix-command
                  json-par-clone-without-value-prefix-command
                  json-par-prefix-universal-argument
                  json-par-prefix-digit-argument
                  json-par-prefix-negative-argument
                  universal-argument
                  universal-argument-more
                  digit-argument
                  negative-argument))
    (remove-hook
     'post-command-hook
     #'json-par--delete-clone-level-overlay
     t)
    (delete-overlay json-par--clone-level-overlay)
    (setq json-par--clone-level-locations '())))


(provide 'json-par-clone)

;;; json-par-clone.el ends here
