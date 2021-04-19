;;; json-par-delete.el --- Deleting/marking things in JSON Par mode -*- lexical-binding: t -*-

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

;; Functions for deleting or marking things in JSON Par mode.

;;; Code:

(require 'cl-lib)
(require 'json-par-lexer)
(require 'json-par-motion)
(require 'json-par-keymap)
(require 'json-par-indent)

(declare-function
 json-par--oneline-after
 "json-par-oneline-multiline"
 (min-level &optional keep-newlines-around-brackets delete-spaces-if-oneline))

(declare-function
 json-par-oneline
 "json-par-oneline-multiline"
 (&optional min-level))


;;; Customizations

(defcustom json-par-hungry-delete t
  "When non-nil, delete successive spaces at once.

`json-par-delete-backward-char' and `json-par-delete-forward-char' are
affected."
  :type 'boolean
  :group 'json-par
  :safe 'booleanp)

(defcustom json-par-action-when-deleting-successive-empty-lines 'leave-one
  "Action when deleting successive empty lines.

- `delete-one': just delete one line.
- `leave-one': leave one and delete the rest.
- `delete-all': delete all lines.

This affects `json-par-delete-backward-char', `json-par-delete-forward-char',
and `json-par-join-line'."
  :type '(choice (const :tag "Just delete one line" 'delete-one)
                 (const :tag "Leave one and delete the rest" 'leave-one)
                 (const :tag "Delete all lines" 'delete-all))
  :group 'json-par
  :safe 'symbolp)

(defcustom json-par-action-when-joining-non-empty-lines 'just-delete
  "Action when joining non-empty lines.

- `just-delete': just delete the line break.
- `delete-line-breaks-between-members': delete all line breaks between members
  (but not before/after members).
- `delete-line-breaks-inside-brackets': delete all line breaks inside the
  current array/object.

This affects `json-par-delete-backward-char', `json-par-delete-forward-char',
and `json-par-join-line'."
  :type '(choice (const :tag "Just delete one line" 'delete-one)
                 (const :tag "Leave one and delete the rest" 'leave-one)
                 (const :tag "Delete all lines" 'delete-all))
  :group 'json-par
  :safe 'symbolp)

(defcustom json-par-action-when-deleting-value-or-member 'mark-or-delete
  "Action for deleting a value or member.

- `delete': delete the value/member unconditionally.
- `mark-or-delete': delete only if the region to be deleted is marked.
  Otherwise, mark the region.
- `mark': mark the region to be deleted unconditionally.

This affects the following commands:

- `json-par-delete-current-member'
- `json-par-delete-backward-member'
- `json-par-delete-forward-member'
- `json-par-delete-current-value-or-key'"
  :type '(choice (const :tag "Delete" delete)
                 (const :tag "Mark or delete" mark-or-delete)
                 (const :tag "Mark" mark))
  :group 'json-par
  :safe 'symbolp)

(defcustom json-par-action-when-deleting-brackets-from-inside
  'mark-or-delete-inner
  "Action for deleting a bracket from inside.

Action for deleting an open bracket backward, or an close bracket forward.

- `delete-outer': delete whole object/array.
- `delete-inner': delete contents of the object/array if not empty.  Delete
  whole object/array otherwise.
- `mark-outer': mark whole object/array.
- `mark-inner': mark contents of the object/array if not empty.  Mark whole
  object/array otherwise.
- `mark-or-delete-inner': mark contents of the object/array if not empty.  Mark
  whole object/array otherwise.  If already marked, delete it.
- `exit': move the point out of the object/array.
- `none': do nothing.

This affects the following commands:

- `json-par-delete-backward-member'
- `json-par-delete-forward-member'
- `json-par-delete-backward-token-or-list'
- `json-par-delete-forward-token-or-list'
- `json-par-delete-backward-char'
- `json-par-delete-forward-char'"
  :type '(choice (const :tag "Delete outer" delete-outer)
                 (const :tag "Delete inner" delete-inner)
                 (const :tag "Mark outer" mark-outer)
                 (const :tag "Mark inner" mark-inner)
                 (const :tag "Mark or delete inner" mark-or-delete-inner)
                 (const :tag "Exit" exit)
                 (const :tag "None" none))
  :group 'json-par
  :safe 'symbolp)

(defcustom json-par-action-when-deleting-brackets-from-outside 'mark-inner
  "Action for deleting a bracket from outside.

Action for deleting an open bracket forward, or an close bracket backward.

- `delete-outer': delete whole object/array.
- `delete-inner': delete contents of the object/array if not empty.  Delete
  whole object/array otherwise.
- `mark-outer': mark whole object/array.
- `mark-or-delete-outer': mark whole object/array.  If it is already marked,
  delete it.
- `mark-inner': mark contents of the object/array if not empty.  Mark whole
  object/array otherwise.
- `enter': move the point into the object/array.

This affects the following commands:

- `json-par-delete-backward-token-or-list'
- `json-par-delete-forward-token-or-list'
- `json-par-delete-backward-char'
- `json-par-delete-forward-char'"
  :type '(choice (const :tag "Delete outer" delete-outer)
                 (const :tag "Delete inner" delete-inner)
                 (const :tag "Mark outer" mark-outer)
                 (const :tag "Mark or delete outer" mark-or-delete-outer)
                 (const :tag "Mark inner" mark-inner)
                 (const :tag "Enter" enter))
  :group 'json-par
  :safe 'symbolp)

(defcustom json-par-action-when-deleting-string-from-inside
  'mark-or-delete-inner
  "Action for deleting a double quote from inside of a string.

- `delete-outer': delete whole string.
- `delete-inner': delete contents of the string if not empty.  Delete whole
  string otherwise.
- `mark-outer': mark whole string.
- `mark-inner': mark contents of the string if not empty.  Mark whole string
  otherwise.
- `mark-or-delete-inner': mark contents of the string.  Mark whole string
  otherwise.  If already marked, delete it.
- `exit': move the point out of the string.
- `none': do nothing.

This affects the following commands:

- `json-par-delete-backward-char'
- `json-par-delete-forward-char'"
  :type '(choice (const :tag "Delete outer" delete-outer)
                 (const :tag "Delete inner" delete-inner)
                 (const :tag "Mark outer" mark-outer)
                 (const :tag "Mark inner" mark-inner)
                 (const :tag "Mark or delete inner" mark-or-delete-inner)
                 (const :tag "Exit" exit)
                 (const :tag "None" none))
  :group 'json-par
  :safe 'symbolp)

(defcustom json-par-action-when-deleting-string-from-outside 'mark-inner
  "Action for deleting a double quote from outside of a string.

- `delete-outer': delete whole string.
- `delete-inner': delete contents of the string if not empty.  Delete whole
  string otherwise.
- `mark-outer': mark whole string.
- `mark-or-delete-outer': mark whole string.  If it is already marked, delete
  it.
- `mark-inner': mark contents of the string if not empty.  Mark whole string
  otherwise.
- `enter': move the point into the string.

This affects the following commands:

- `json-par-delete-backward-char'
- `json-par-delete-forward-char'"
  :type '(choice (const :tag "Delete outer" delete-outer)
                 (const :tag "Delete inner" delete-inner)
                 (const :tag "Mark outer" mark-outer)
                 (const :tag "Mark or delete outer" mark-or-delete-outer)
                 (const :tag "Mark inner" mark-inner)
                 (const :tag "Enter" enter))
  :group 'json-par
  :safe 'symbolp)

(defcustom json-par-action-when-deleting-colon 'mark-or-delete
  "Action for deleting a colon.

- `delete': delete whole member (key-value pair) unconditionally.
- `mark-or-delete': delete only if the whole member is marked.  Otherwise, mark
  the whole member.
- `mark': mark whole member unconditionally.
- `skip': move the point to the opposite site.

This affects the following commands:

- `json-par-delete-backward-token-or-list'
- `json-par-delete-forward-token-or-list'
- `json-par-delete-backward-char'
- `json-par-delete-forward-char'"
  :type '(choice (const :tag "Delete member" delete)
                 (const :tag "Mark or delete member" mark-or-delete)
                 (const :tag "Mark member" mark)
                 (const :tag "Skip" skip))
  :group 'json-par
  :safe 'symbolp)

(defcustom json-par-action-when-deleting-comma 'mark-or-delete
  "Action for deleting a comma.

- `delete': delete whole member (key-value pair) beyond the comma
  unconditionally.
- `mark-or-delete': delete only if the whole member beyond the comma is marked.
  Otherwise, mark the whole member.
- `mark': mark whole member beyond the comma unconditionally.
- `skip': move the point to the opposite site.

This affects the following commands:

- `json-par-delete-backward-token-or-list'
- `json-par-delete-forward-token-or-list'
- `json-par-delete-backward-char'
- `json-par-delete-forward-char'"
  :type '(choice (const :tag "Delete member" delete)
                 (const :tag "Mark or delete member" mark-or-delete)
                 (const :tag "Mark member" mark)
                 (const :tag "Skip" skip))
  :group 'json-par
  :safe 'symbolp)


;;; Functions

(defun json-par--delete-or-mark-region
    (start end action &optional mark-direction)
  "Delete or mark region from START to END depending on ACTION.

See `json-par-action-when-deleting-value-or-member' for details of ACTION.

If the point is middle of the region to be marked, mark backward if
MARK-DIRECTION is a symbol `backward' and mark forward otherwise.

Return symbol `delete' if the region is deleted.  Return symbol `mark'
otherwise."
  (when (eq action 'mark-or-delete)
    (setq action (if (and
                      (use-region-p)
                      (= (region-beginning) start)
                      (= (region-end) end))
                     'delete
                   'mark)))
  (if (eq action 'mark)
      (progn
        (setq mark-direction
              (cond
               ((<= (point) start)
                'forward)
               ((<= end (point))
                'backward)
               (t
                mark-direction)))
        (if (eq mark-direction 'backward)
            (progn
              (goto-char end)
              (push-mark start t t)
              (setq deactivate-mark nil))
          (goto-char start)
          (push-mark end t t)
          (setq deactivate-mark nil)))
    (delete-region start end))
  action)

(defun json-par--delete-or-mark-or-exit-list (action direction)
  "Delete, mark, or exit from an object/array depending on ACTION.

See `json-par-action-when-deleting-brackets-from-inside' for details of ACTION.

Assuming the point is after an open bracket if DIRECTION is `backward', or
before a close bracket otherwise.  This also affects the direction of marking."
  (cond
   ((eq action 'none)
    nil)

   ((eq action 'exit)
    (if (eq direction 'backward)
        (json-par-backward-token)
      (json-par-forward-token)))

   (t
    (json-par--delete-or-mark-list
     (save-excursion
       (if (eq direction 'backward)
           (progn
             (json-par-backward-token)
             (json-par-forward-token-or-list))
         (json-par-forward-token)
         (json-par-backward-token-or-list)))
     action
     (if (eq direction 'backward) 'forward 'backward)
     nil))))

(defun json-par--delete-or-mark-or-enter-list (action direction)
  "Delete, mark, or enter into an object/array depending on ACTION.

See `json-par-action-when-deleting-brackets-from-outside' for details of ACTION.

Assuming the point is after an close bracket if DIRECTION is `backward', or
before a open bracket otherwise.  This also affects the direction of marking."
  (if (eq action 'enter)
      (if (eq direction 'backward)
          (progn
            (json-par-backward-token)
            (skip-chars-backward "\s\t\n"))
        (json-par-forward-token)
        (skip-chars-forward "\s\t\n"))
    (json-par--delete-or-mark-list
     (save-excursion
       (if (eq direction 'backward)
           (json-par-backward-token-or-list)
         (json-par-forward-token-or-list)))
     action
     direction
     nil)))

(defun json-par--delete-or-mark-list (whole-list action direction allow-empty)
  "Delete or mark an array/object depending on ACTION.

WHOLE-LIST is a token representing the array/object.

See `json-par-action-when-deleting-brackets-from-inside' and
`json-par-action-when-deleting-brackets-from-outside' for details of ACTION.
ACTION must not be `exit' or `enter'."
  (let (start
        end)
    (cond
     ;; Delete outer
     ((eq action 'delete-outer)
      (json-par-delete-current-value-or-key
       (if (eq direction 'backward)
           (json-par-token-end whole-list)
         (json-par-token-start whole-list))
       nil
       (if (eq direction 'backward) 'preceding 'following)
       'delete
       direction))

     ;; Mark outer
     ((memq action '(mark-outer mark-or-delete-outer))
      (if (eq direction 'backward)
          (progn
            (setq start (json-par-token-start whole-list))
            (if (json-par-token-matching-brackets-p whole-list)
                (setq end (json-par-token-end whole-list))
              (setq end (point-max))))
        (setq end (json-par-token-end whole-list))
        (if (json-par-token-matching-brackets-p whole-list)
            (setq start (json-par-token-start whole-list))
          (setq start (point-min))))
      (json-par--delete-or-mark-region
       start
       end
       (if (eq action 'mark-outer) 'mark 'mark-or-delete)
       direction))

     ;; Delete/mark inner
     ((memq action '(delete-inner mark-inner mark-or-delete-inner))
      (if (eq direction 'backward)
          (progn
            (setq start (1+ (json-par-token-start whole-list)))
            (if (json-par-token-matching-brackets-p whole-list)
                (setq end (1- (json-par-token-end whole-list)))
              (setq end (point-max))))
        (setq end (1- (json-par-token-end whole-list)))
        (if (json-par-token-matching-brackets-p whole-list)
            (setq start (1+ (json-par-token-start whole-list)))
          (setq start (point-min))))
      (setq start (save-excursion
                    (goto-char start)
                    (skip-chars-forward "\s\t\n")
                    (point)))
      (setq end (save-excursion
                  (goto-char end)
                  (skip-chars-backward "\s\t\n")
                  (point)))
      (if (and (not allow-empty) (<= end start))
          (json-par--delete-or-mark-list
           whole-list
           (cond
            ((eq action 'delete-inner) 'delete-outer)
            ((eq action 'mark-inner) 'mark-outer)
            ((eq action 'mark-or-delete-inner) 'mark-or-delete-outer))
           direction
           allow-empty)
        (when (<= end start)
          (setq start (save-excursion
                        (goto-char start)
                        (skip-chars-backward "\s\t\n")
                        (point)))
          (setq end (save-excursion
                      (goto-char end)
                      (skip-chars-forward "\s\t\n")
                      (point))))
        (when (eq (json-par--delete-or-mark-region
                   start
                   end
                   (cond
                    ((eq action 'delete-inner) 'delete)
                    ((eq action 'mark-inner) 'mark)
                    ((eq action 'mark-or-delete-inner) 'mark-or-delete))
                   direction)
                  'delete)
          (goto-char start)))))))

(cl-defun json-par-delete-backward-token-or-list
    (&optional
     (action 'delete)
     &key
     (action-when-deleting-brackets-from-inside 'delete-outer)
     (action-when-deleting-brackets-from-outside 'delete-outer)
     (action-when-deleting-colon 'delete)
     (action-when-deleting-comma 'delete))
  "Delete or mark the preceding token or object/array.

ACTION is one of `delete', `mark-or-delete', or `mark', defaults to `delete'
when called from Lisp program, or the value of
`json-par-action-when-deleting-value-or-member' variable when called
interactively.

- `delete': delete the token/object/array unconditionally.
- `mark-or-delete': delete only if the region to be deleted is marked.
  Otherwise, mark the region.
- `mark': mark the region to be deleted unconditionally.

At the beginning of the buffer, delete/mark the spaces and line breaks before
the point.

After a comma, delete/mark the previous member.

After an open bracket, action depends on
ACTION-WHEN-DELETING-BRACKETS-FROM-INSIDE.  It is one of the following:

- `delete-outer': delete whole object/array.
- `delete-inner': delete contents of the object/array if not empty.  Delete
   whole object/array otherwise.
- `mark-outer': mark whole object/array.
- `mark-inner': mark contents of the object/array if not empty.  Mark whole
   object/array otherwise.
- `mark-or-delete-inner': mark contents of the object/array if not empty.  Mark
   whole object/array otherwise.  If already marked, delete it.
- `exit': move the point out of the object/array.
- `none': do nothing.

The default value is `delete-outer' when called from Lisp program, or the value
of variable `json-par-action-when-deleting-brackets-from-inside' when called
interactively.

After a close bracket, action depends on
ACTION-WHEN-DELETING-BRACKETS-FROM-OUTSIDE.  It is one of the following:

- `delete-outer': delete whole object/array.
- `delete-inner': delete contents of the object/array if not empty.  Delete
   whole object/array otherwise.
- `mark-outer': mark whole object/array.
- `mark-or-delete-outer': mark whole object/array.  If it is already marked,
   delete it.
- `mark-inner': mark contents of the object/array if not empty.  Mark whole
   object/array otherwise.
- `enter': move the point into the object/array.

The default value is `delete-outer' when called from Lisp program, or the value
of variable `json-par-action-when-deleting-brackets-from-outside' when called
interactively.

After a colon, action depends on ACTION-WHEN-DELETING-COLON.  It is one of the
following:

- `delete': delete whole member (key-value pair) unconditionally.
- `mark-or-delete': delete only if the whole member is marked.  Otherwise, mark
  the whole member.
- `mark': mark whole member unconditionally.
- `skip': move the point to the opposite site.

The default value is `delete' when called from Lisp program, or the value of
variable `json-par-action-when-deleting-colon' when called interactively.

After a comma, action depends on ACTION-WHEN-DELETING-COMMA.  It is one of the
following:

- `delete': delete whole member (key-value pair) beyond the comma
  unconditionally.
- `mark-or-delete': delete only if the whole member beyond the comma is marked.
  Otherwise, mark the whole member.
- `mark': mark whole member beyond the comma unconditionally.
- `skip': move the point to the opposite site.

The default value is `delete' when called from Lisp program, or the value of
variable `json-par-action-when-deleting-comma' when called interactively.

Otherwise, delete/mark the preceding value."
  (interactive
   (list
    json-par-action-when-deleting-value-or-member
    :action-when-deleting-brackets-from-inside
    json-par-action-when-deleting-brackets-from-inside
    :action-when-deleting-brackets-from-outside
    json-par-action-when-deleting-brackets-from-outside
    :action-when-deleting-colon
    json-par-action-when-deleting-colon
    :action-when-deleting-comma
    json-par-action-when-deleting-comma))
  (let ((pos (point))
        before-comment-pos
        previous-token)
    (json-par--out-comment)
    (setq before-comment-pos (point))
    (json-par--out-atom)
    (setq previous-token (save-excursion (json-par-backward-token)))
    (cond
     ;; Beginning of the buffer.
     ((json-par-token-outside-of-buffer-p previous-token)
      (goto-char before-comment-pos)
      (when (/= before-comment-pos pos)
        (forward-comment 1))
      (json-par--delete-or-mark-region
       (json-par-token-end previous-token)
       (point)
       action
       'backward))

     ;; After comma.
     ((json-par-token-comma-p previous-token)
      (json-par--delete-or-mark-or-skip-comma
       action-when-deleting-comma
       'backward))

     ;; After open bracket.
     ((json-par-token-open-bracket-p previous-token)
      (json-par--delete-or-mark-or-exit-list
       action-when-deleting-brackets-from-inside
       'backward))

     ;; After close bracket.
     ((json-par-token-close-bracket-p previous-token)
      (json-par--delete-or-mark-or-enter-list
       action-when-deleting-brackets-from-outside
       'backward))

     ;; After colon.
     ((json-par-token-colon-p previous-token)
      (json-par--delete-or-mark-or-skip-colon
       action-when-deleting-colon
       'backward))

     ;; After `other' token.
     ((json-par-token-other-p previous-token)
      (json-par--delete-or-mark-region
       (json-par-token-start previous-token)
       (json-par-token-end previous-token)
       action
       'backward))

     ;; After value.
     ((json-par-token-atom-p previous-token)
      (json-par-delete-current-value-or-key
       (point)
       nil
       'preceding
       action
       'backward)))))

(cl-defun json-par-delete-forward-token-or-list
    (&optional
     (action 'delete)
     &key
     (action-when-deleting-brackets-from-inside 'delete-outer)
     (action-when-deleting-brackets-from-outside 'delete-outer)
     (action-when-deleting-colon 'delete)
     (action-when-deleting-comma 'delete))
  "Delete or mark the following token or object/array.

ACTION is one of `delete', `mark-or-delete', or `mark', defaults to `delete'
when called from Lisp program, or the value of
`json-par-action-when-deleting-value-or-member' variable when called
interactively.

- `delete': delete the token/object/array unconditionally.
- `mark-or-delete': delete only if the region to be deleted is marked.
  Otherwise, mark the region.
- `mark': mark the region to be deleted unconditionally.

At the end of the buffer, delete/mark the spaces and line breaks after the
point.

Before a comma, delete/mark the next member.

Before a close bracket, action depends on
ACTION-WHEN-DELETING-BRACKETS-FROM-INSIDE.  It is one of the following:

- `delete-outer': delete whole object/array.
- `delete-inner': delete contents of the object/array if not empty.  Delete
   whole object/array otherwise.
- `mark-outer': mark whole object/array.
- `mark-inner': mark contents of the object/array if not empty.  Mark whole
   object/array otherwise.
- `mark-or-delete-inner': mark contents of the object/array if not empty.  Mark
   whole object/array otherwise.  If already marked, delete it.
- `exit': move the point out of the object/array.
- `none': do nothing.

The default value is `delete-outer' when called from Lisp program, or the value
of variable `json-par-action-when-deleting-brackets-from-inside' when called
interactively.

Before an open bracket, action depends on
ACTION-WHEN-DELETING-BRACKETS-FROM-OUTSIDE.  It is one of the following:

- `delete-outer': delete whole object/array.
- `delete-inner': delete contents of the object/array if not empty.  Delete
   whole object/array otherwise.
- `mark-outer': mark whole object/array.
- `mark-or-delete-outer': mark whole object/array.  If it is already marked,
   delete it.
- `mark-inner': mark contents of the object/array if not empty.  Mark whole
   object/array otherwise.
- `enter': move the point into the object/array.

The default value is `delete-outer' when called from Lisp program, or the value
of variable `json-par-action-when-deleting-brackets-from-outside' when called
interactively.

Before a colon, action depends on ACTION-WHEN-DELETING-COLON.  It is one of the
following:

- `delete': delete whole member (key-value pair) unconditionally.
- `mark-or-delete': delete only if the whole member is marked.  Otherwise, mark
  the whole member.
- `mark': mark whole member unconditionally.
- `skip': move the point to the opposite site.

The default value is `delete' when called from Lisp program, or the value of
variable `json-par-action-when-deleting-colon' when called interactively.

Before a comma, action depends on ACTION-WHEN-DELETING-COMMA.  It is one of the
following:

- `delete': delete whole member (key-value pair) beyond the comma
  unconditionally.
- `mark-or-delete': delete only if the whole member beyond the comma is marked.
  Otherwise, mark the whole member.
- `mark': mark whole member beyond the comma unconditionally.
- `skip': move the point to the opposite site.

The default value is `delete' when called from Lisp program, or the value of
variable `json-par-action-when-deleting-comma' when called interactively.

Otherwise, delete/mark the preceding value."
  (interactive
   (list
    json-par-action-when-deleting-value-or-member
    :action-when-deleting-brackets-from-inside
    json-par-action-when-deleting-brackets-from-inside
    :action-when-deleting-brackets-from-outside
    json-par-action-when-deleting-brackets-from-outside
    :action-when-deleting-colon
    json-par-action-when-deleting-colon
    :action-when-deleting-comma
    json-par-action-when-deleting-comma))
  (json-par--out-comment)
  (when (looking-at "/[/*]")
    (forward-comment 1))
  (let* ((current-atom (json-par--current-atom))
         (next-token
          (if (json-par-token-inside-p current-atom)
              current-atom
            (save-excursion
              (json-par--out-atom)
              (json-par-forward-token)))))
    (cond
     ;; End of the buffer.
     ((json-par-token-outside-of-buffer-p next-token)
      (json-par--delete-or-mark-region
       (point)
       (json-par-token-end next-token)
       action
       'forward))

     ;; Before comma.
     ((json-par-token-comma-p next-token)
      (json-par--delete-or-mark-or-skip-comma
       action-when-deleting-comma
       'forward))

     ;; Before close bracket.
     ((json-par-token-close-bracket-p next-token)
      (json-par--delete-or-mark-or-exit-list
       action-when-deleting-brackets-from-inside
       'forward))

     ;; Before open bracket.
     ((json-par-token-open-bracket-p next-token)
      (json-par--delete-or-mark-or-enter-list
       action-when-deleting-brackets-from-outside
       'forward))

     ;; Before colon.
     ((json-par-token-colon-p next-token)
      (json-par--delete-or-mark-or-skip-colon
       action-when-deleting-colon
       'forward))

     ;; Before `other' token.
     ((json-par-token-other-p next-token)
      (json-par--delete-or-mark-region
       (json-par-token-start next-token)
       (json-par-token-end next-token)
       action
       'forward))

     ;; Before value.
     ((json-par-token-atom-p next-token)
      (json-par-delete-current-value-or-key
       (point)
       nil
       'following
       action
       'forward)))))

(defun json-par-delete-current-value-or-key
    (&optional pos keep-member-if-empty preferred-comma action mark-direction)
  "Delete or mark the current value or key.

If POS is given, the value or key at the POS is deleted.

Inside empty brackets, delete the spaces and line breaks between it.

If the point is where a value is expected but missing, delete the whole member
unless KEEP-MEMBER-IF-EMPTY is non-nil.  PREFERRED-COMMA affects which comma is
deleted.

Before or after a key, delete it.  If the key is missing, delete the whole
member.

Otherwise, delete the value.

ACTION is one of `delete', `mark-or-delete', or `mark', defaults to `delete'
when called from Lisp program, or the value of
`json-par-action-when-deleting-value-or-member' variable when called
interactively.

- `delete': delete the value/member unconditionally.
- `mark-or-delete': delete only if the region to be deleted is marked.
  Otherwise, mark the region.
- `mark': mark the region to be deleted unconditionally.

If the point is middle of the region to be marked, mark backward if
MARK-DIRECTION is a symbol `backward' and mark forward otherwise."
  (interactive
   (list
    nil
    nil
    nil
    json-par-action-when-deleting-value-or-member
    nil))
  (unless pos
    (setq pos (point)))
  (unless preferred-comma
    (setq preferred-comma 'following))
  (unless action
    (setq action 'delete))
  (unless mark-direction
    (setq mark-direction 'forward))
  (save-excursion
    (goto-char pos)
    (json-par--out-comment)
    (json-par--out-atom)
    (setq pos (point)))
  (let* ((next-token (save-excursion
                       (goto-char pos)
                       (json-par-forward-token)))
         (previous-token (save-excursion
                           (goto-char pos)
                           (json-par-backward-token)))
         token-before-value
         token-after-value
         end-position)
    (cond
     ;; Inside an empty brackets
     ((and (or (json-par-token-open-bracket-p previous-token)
               (json-par-token-outside-of-buffer-p previous-token))
           (or (json-par-token-close-bracket-p next-token)
               (json-par-token-outside-of-buffer-p next-token)))
      (unless keep-member-if-empty
        (json-par--delete-or-mark-region
         (json-par-token-end previous-token)
         (json-par-token-start next-token)
         action
         mark-direction)))

     ;; Between two commas or other places expecting a value (but not before a
     ;; colon)
     ((and (memq (json-par-token-type previous-token)
                 '({ \( \[ \, : outside-of-buffer))
           (memq (json-par-token-type next-token)
                 '(} \) \] \, outside-of-buffer)))
      (unless keep-member-if-empty
        (json-par-delete-current-member
         1
         action
         pos
         preferred-comma
         mark-direction)))

     ;; Before a key
     ((json-par--object-key-p next-token)
      (json-par--delete-key
       next-token
       keep-member-if-empty
       action
       mark-direction))

     ;; After a key
     ((json-par--object-key-p previous-token)
      (json-par--delete-key
       previous-token
       keep-member-if-empty
       action
       mark-direction))

     ;; Before colon (missing key)
     ((json-par-token-colon-p next-token)
      (unless keep-member-if-empty
        (json-par-delete-current-member 1 action pos 'following 'forward)))

     ;; otherwise
     (t
      (setq token-before-value
            (save-excursion
              (goto-char pos)
              (json-par-end-of-member)
              (json-par-backward-token-or-list)
              (json-par-backward-token)))
      (setq token-after-value
            (save-excursion
              (goto-char pos)
              (json-par-end-of-member)
              (json-par-forward-token)))
      (if (and (or (json-par-token-open-bracket-p token-before-value)
                   (json-par-token-outside-of-buffer-p token-before-value))
               (or (json-par-token-close-bracket-p token-after-value)
                   (json-par-token-outside-of-buffer-p token-after-value)))
          (json-par-delete-current-member
           1
           action
           pos
           preferred-comma
           mark-direction)
        (setq end-position (save-excursion
                             (goto-char pos)
                             (json-par-end-of-member)
                             (json-par--forward-spaces)
                             (skip-chars-backward "\s\t\n")
                             (point)))
        (json-par--delete-or-mark-region
         (save-excursion
           (goto-char end-position)
           (json-par-backward-token-or-list)
           (json-par--backward-spaces)
           (skip-chars-forward "\s\t\n")
           (point))
         end-position
         action
         mark-direction))))))

(defun json-par--delete-key (token keep-member-if-empty action mark-direction)
  "Delete a object key TOKEN.

If the key is empty, delete the whole member unless KEEP-MEMBER-IF-EMPTY.

If the key is not empty, replace it with am empty key.

For ACTION and MARK-DIRECTION, see `json-par-delete-current-value-or-key'."
  (if (and (json-par-token-empty-string-p token) (not keep-member-if-empty))
      (json-par-delete-current-member
       1
       action
       (json-par-token-start token)
       'following
       'forward)
    (setq action (json-par--delete-or-mark-region
                  (1+ (json-par-token-start token))
                  (1- (json-par-token-end token))
                  action
                  mark-direction))
    (when (eq action 'delete)
      (goto-char (1+ (json-par-token-start token))))))

(defun json-par--delete-or-mark-or-skip-colon (action direction)
  "Delete, mark, or skip a colon depending on ACTION.

See `json-par-action-when-deleting-colon' for details of ACTION.

DIRECTION is the direction of mark."
  (if (eq action 'skip)
      (if (eq direction 'backward)
          (progn
            (json-par-backward-token)
            (json-par--backward-spaces)
            (when (memq (char-before) '(nil ?\, ?\[ ?\( ?{))
              (json-par--forward-spaces)))
        (json-par-forward-token)
        (json-par--forward-spaces t))
    (json-par-delete-current-member
     1
     action
     (point)
     (if (eq direction 'backward) 'preceding 'following)
     direction)))

(defun json-par--delete-or-mark-or-skip-comma (action direction)
  "Delete, mark, or skip a comma depending on ACTION.

See `json-par-action-when-deleting-comma' for details of ACTION.

DIRECTION is the direction of mark."
  (if (eq action 'skip)
      (if (eq direction 'backward)
          (progn
            (json-par-backward-token)
            (json-par--backward-spaces)
            (when (memq (char-before) '(nil ?\, ?\[ ?\( ?{))
              (json-par--forward-spaces)))
        (json-par-forward-token)
        (json-par--forward-spaces))
    (if (eq direction 'backward)
        (if (save-excursion
              (json-par--forward-spaces)
              (memq (char-after) '(nil ?\, ?\] ?\) ?})))
            (json-par-delete-current-member
             1
             action
             (point)
             'preceding
             direction)
          (json-par-delete-backward-member 1 action))
      (if (save-excursion
            (json-par--backward-spaces)
            (memq (char-before) '(nil ?\, ?\[ ?\( ?{)))
          (json-par-delete-current-member
           1
           action
           (point)
           'following
           direction)
        (json-par-delete-forward-member 1 action)))))

(cl-defun json-par-delete-backward-member
    (n
     &optional
     (action 'delete)
     &key
     (action-when-deleting-brackets-from-inside 'delete-outer))
  "Delete or mark the preceding N members.

If N is negative, delete the following members.

At the beginning of the buffer, delete the spaces and line breaks before the
point.

ACTION is one of `delete', `mark-or-delete', or `mark', defaults to `delete'
when called from Lisp program, or the value of variable
`json-par-action-when-deleting-value-or-member' when called interactively.

- `delete': delete the value/member unconditionally.
- `mark-or-delete': delete only if the region to be deleted is marked.
  Otherwise, mark the region.
- `mark': mark the region to be deleted unconditionally.

At the beginning of an array/object, action is determinated by
ACTION-WHEN-DELETING-BRACKETS-FROM-INSIDE.  It is one of the following:

- `delete-outer': delete whole object/array.
- `delete-inner': delete contents of the object/array if not empty.  Delete
   whole object/array otherwise.
- `mark-outer': mark whole object/array.
- `mark-inner': mark contents of the object/array if not empty.  Mark whole
   object/array otherwise.
- `mark-or-delete-inner': mark contents of the object/array if not empty.  Mark
   whole object/array otherwise.  If already marked, delete it.
- `exit': move the point out of the object/array.
- `none': do nothing.

The default value is `delete-outer' when called from Lisp program, or the value
of variable `json-par-action-when-deleting-brackets-from-inside' when called
interactively."
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)
    json-par-action-when-deleting-value-or-member
    :action-when-deleting-brackets-from-inside
    json-par-action-when-deleting-brackets-from-inside))
  (cond
   ((< n 0)
    (json-par-delete-forward-member
     (- n)
     action
     :action-when-deleting-brackets-from-inside
     action-when-deleting-brackets-from-inside))

   ((zerop n)
    nil)

   (t
    (let ((pos (point))
          before-comment-pos
          previous-token)
      (json-par--out-comment)
      (setq before-comment-pos (point))
      (json-par--out-atom)
      (setq previous-token (save-excursion (json-par-backward-token)))
      (cond
       ;; Out of buffer
       ((json-par-token-outside-of-buffer-p previous-token)
        (json-par--delete-or-mark-region
         (json-par-token-end previous-token)
         (save-excursion
           (goto-char before-comment-pos)
           (when (/= before-comment-pos pos)
             (forward-comment 1))
           (point))
         action))

       ;; After open bracket
       ((json-par-token-open-bracket-p previous-token)
        (json-par--delete-or-mark-or-exit-list
         action-when-deleting-brackets-from-inside
         'backward))

       ;; After comma
       ((json-par-token-comma-p previous-token)
        (json-par-delete-current-member
         (- n)
         action
         (json-par-token-start previous-token)
         'following
         'backward))

       ;; After value or colon
       ((or (json-par-token-close-bracket-p previous-token)
            (json-par-token-atom-p previous-token)
            (json-par-token-colon-p previous-token))
        (json-par-delete-current-member
         (- n)
         action
         (point)
         'preceding
         'backward)))))))

(cl-defun json-par-delete-forward-member
    (n
     &optional
     (action 'delete)
     &key
     (action-when-deleting-brackets-from-inside 'delete-outer))
  "Delete or mark the following N members.

If N is negative, delete the preceding members.

At the end of the buffer, delete the spaces and line breaks before the point.

ACTION is one of `delete', `mark-or-delete', or `mark', defaults to `delete'
when called from Lisp program, or the value of
`json-par-action-when-deleting-value-or-member' variable when called
interactively.

- `delete': delete the value/member unconditionally.
- `mark-or-delete': delete only if the region to be deleted is marked.
  Otherwise, mark the region.
- `mark': mark the region to be deleted unconditionally.

At the end of an array/object, action is determinated by
ACTION-WHEN-DELETING-BRACKETS-FROM-INSIDE.  It is one of the following:

- `delete-outer': delete whole object/array.
- `delete-inner': delete contents of the object/array if not empty.  Delete
   whole object/array otherwise.
- `mark-outer': mark whole object/array.
- `mark-inner': mark contents of the object/array if not empty.  Mark whole
   object/array otherwise.
- `mark-or-delete-inner': mark contents of the object/array if not empty.  Mark
   whole object/array otherwise.  If already marked, delete it.
- `exit': move the point out of the object/array.

The default value is `delete-outer' when called from Lisp program, or the value
of variable `json-par-action-when-deleting-brackets-from-inside' when called
interactively."
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)
    json-par-action-when-deleting-value-or-member
    :action-when-deleting-brackets-from-inside
    json-par-action-when-deleting-brackets-from-inside))
  (cond
   ((< n 0)
    (json-par-delete-backward-member
     (- n)
     action
     :action-when-deleting-brackets-from-inside
     action-when-deleting-brackets-from-inside))

   ((zerop n)
    nil)

   (t
    (json-par--out-comment)
    (when (looking-at "/[/*]")
      (forward-comment 1))
    (let* ((current-atom (json-par--current-atom))
           (next-token
            (if (json-par-token-inside-p current-atom)
                current-atom
              (save-excursion
                (json-par-forward-token)))))
      (cond
       ;; Out of buffer
       ((json-par-token-outside-of-buffer-p next-token)
        (json-par--delete-or-mark-region
         (point)
         (json-par-token-end next-token)
         action
         'forward))

       ;; Before close bracket
       ((json-par-token-close-bracket-p next-token)
        (json-par--delete-or-mark-or-exit-list
         action-when-deleting-brackets-from-inside
         'forward))

       ;; Before comma, or missing comma
       ((or (json-par-token-comma-p next-token)
            (and (not (json-par-token-inside-p current-atom))
                 (eq (json-par--position-in-member) 'after-member)))
        (json-par-delete-current-member
         n
         action
         (json-par-token-end next-token)
         'preceding
         'forward))

       ;; Before value or colon
       ((or
         (json-par-token-open-bracket-p next-token)
         (json-par-token-atom-p next-token)
         (json-par-token-colon-p next-token))
        (json-par-delete-current-member
         n
         action
         (point)
         'following
         'forward)))))))

(defun json-par-delete-current-member
    (&optional n action pos preferred-comma mark-direction)
  "Delete or mark the member under the point.

Also delete the comma unless PREFERRED-COMMA is `none'.

If N is given, also delete the following N - 1 members if positive, or the
preceding -N - 1 members if negative.

If POS is given, delete the member at POS.

If PREFERRED-COMMA is `preceding', prefer preceding comma to delete.  Prefer
following otherwise.  At the beginning of the array/object or line, always
prefer the following comma.  Likewise, at the end of the array/object or line,
always prefer the preceding comma.  Default is `following'.

ACTION is one of `delete', `mark-or-delete', or `mark', defaults to `delete'
when called from Lisp program, or the value of
`json-par-action-when-deleting-value-or-member' variable when called
interactively.

- `delete': delete the value/member unconditionally.
- `mark-or-delete': delete only if the region to be deleted is marked.
  Otherwise, mark the region.
- `mark': mark the region to be deleted unconditionally.

If the point is middle of the region to be marked, mark backward if
MARK-DIRECTION is a symbol `backward' and mark forward otherwise."
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)
    json-par-action-when-deleting-value-or-member))
  (unless n
    (setq n 1))
  (unless pos
    (setq pos (point)))
  (unless preferred-comma
    (setq preferred-comma (if (< 0 n) 'following 'preceding)))
  (unless action
    (setq action 'delete))
  (unless mark-direction
    (setq mark-direction (if (< 0 n) 'forward 'backward)))
  (if (zerop n)
      nil
    (let* ((start-of-member (save-excursion
                              (goto-char pos)
                              (json-par-beginning-of-member)
                              (when (< n -1)
                                (dotimes (_ (- (- n) 1))
                                  (json-par-backward-member)))
                              (json-par--backward-spaces)
                              (skip-chars-forward "\s\t\n")
                              (when (memq (char-after) '(nil ?\] ?\) ?}))
                                (cond
                                 ((save-excursion
                                    (json-par--backward-spaces t)
                                    (bolp))
                                  (json-par--backward-spaces t)
                                  (backward-char))
                                 ((memq (char-before) '(?\s ?\t))
                                  (backward-char))))
                              (point)))
           (end-of-member (save-excursion
                            (goto-char pos)
                            (json-par-beginning-of-member)
                            (json-par-end-of-member)
                            (when (< 1 n)
                              (dotimes (_ (- n 1))
                                (json-par-forward-member)))
                            (json-par--forward-spaces)
                            (skip-chars-backward "\s\t\n")
                            (when (memq (char-before) '(nil ?\[ ?\( ?{ ?,))
                              (skip-chars-forward "\s\t\n"))
                            (when (and (eq (char-before) ?\:)
                                       (memq (char-after) '(?\s ?\t)))
                              (forward-char))
                            (when (memq (char-after) '(?\] ?\) ?}))
                              (cond
                               ((save-excursion
                                  (json-par--backward-spaces t)
                                  (bolp))
                                (json-par--backward-spaces t)
                                (backward-char))
                               ((memq (char-before) '(?\s ?\t))
                                (backward-char))))
                            (point)))
           (next-token (save-excursion
                         (goto-char end-of-member)
                         (json-par-forward-token)))
           (previous-token (save-excursion
                             (goto-char start-of-member)
                             (json-par-backward-token)))
           (is-last-member
            (or (json-par-token-close-bracket-p next-token)
                (json-par-token-outside-of-buffer-p next-token)))
           (is-first-member
            (or (json-par-token-open-bracket-p previous-token)
                (json-par-token-outside-of-buffer-p previous-token)))
           (is-first-member-of-line
            (json-par--beginning-of-line-or-list-p start-of-member))
           (is-last-member-of-line
            (json-par--end-of-line-or-list-p
             (if (json-par-token-comma-p next-token)
                 (json-par-token-end next-token)
               end-of-member)))
           (is-solo-member-of-line
            (and is-first-member-of-line is-last-member-of-line))
           (start-of-next-member
            (save-excursion
              (goto-char (if (json-par-token-comma-p next-token)
                             (json-par-token-end next-token)
                           end-of-member))
              (skip-chars-forward "\s\t\n")
              (when (memq (char-after) '(nil ?\] ?\) ?}))
                (cond
                 ((save-excursion
                    (json-par--backward-spaces t)
                    (bolp))
                  (json-par--backward-spaces t)
                  (backward-char))
                 ((memq (char-before) '(?\s ?\t))
                  (backward-char))))
              (point)))
           (end-of-previous-member
            (save-excursion
              (goto-char (if (json-par-token-comma-p previous-token)
                             (json-par-token-start previous-token)
                           start-of-member))
              (skip-chars-backward "\s\t\n")
              (when (memq (char-before) '(nil ?\[ ?\( ?{ ?,))
                (skip-chars-forward "\s\t\n"))
              (when (and (eq (char-before) ?:)
                         (memq (char-after) '(?\s ?\t)))
                (forward-char))
              (when (memq (char-after) '(?\] ?\) ?}))
                (cond
                 ((save-excursion
                    (json-par--backward-spaces t)
                    (bolp))
                  (json-par--backward-spaces t)
                  (backward-char))
                 ((memq (char-before) '(?\s ?\t))
                  (backward-char))))
              (point)))
           start-of-region
           end-of-region)
      (cond
       ;; Deleting the sole member of an array/object
       ((and is-first-member is-last-member)
        (setq start-of-region (save-excursion
                                (goto-char start-of-member)
                                (json-par--backward-spaces)
                                (point)))
        (setq end-of-region (save-excursion
                              (goto-char end-of-member)
                              (json-par--forward-spaces)
                              (point))))

       ;; Keeping comma
       ((eq preferred-comma 'none)
        (setq start-of-region start-of-member)
        (setq end-of-region end-of-member))

       ;; Deleting the last member of an array/object
       (is-last-member
        (setq start-of-region end-of-previous-member)
        (setq end-of-region end-of-member))

       ;; Deleting the first member of an array/object
       (is-first-member
        (setq start-of-region start-of-member)
        (setq end-of-region start-of-next-member))

       ;; Deleting the solo member of the line
       (is-solo-member-of-line
        (if (eq preferred-comma 'preceding)
            (progn
              (setq start-of-region end-of-previous-member)
              (setq end-of-region end-of-member))
          (setq start-of-region start-of-member)
          (setq end-of-region start-of-next-member)))

       ;; Deleting a member at the end of a line
       (is-last-member-of-line
        (if (eq preferred-comma 'preceding)
            (progn
              (setq start-of-region end-of-previous-member)
              (setq end-of-region end-of-member))
          (setq start-of-region (save-excursion
                                  (goto-char start-of-member)
                                  (json-par--backward-spaces)
                                  (point)))
          (setq end-of-region (save-excursion
                                (goto-char
                                 (if (json-par-token-comma-p next-token)
                                     (json-par-token-end next-token)
                                   end-of-member))
                                (json-par--forward-spaces t)
                                (point)))))

       ;; Deleting a member at the start of a line
       (is-first-member-of-line
        (setq start-of-region start-of-member)
        (setq end-of-region start-of-next-member))

       ;; Otherwise
       (t
        (if (eq preferred-comma 'preceding)
            (progn
              (setq start-of-region end-of-previous-member)
              (setq end-of-region end-of-member))
          (setq start-of-region start-of-member)
          (setq end-of-region start-of-next-member))))
      (when (and (save-excursion
                   (goto-char start-of-region)
                   (json-par--backward-spaces)
                   (memq (char-before) '(nil ?\[ ?\( ?{)))
                 (save-excursion
                   (goto-char end-of-region)
                   (json-par--forward-spaces)
                   (memq (char-after) '(nil ?\] ?\) ?}))))
        (setq start-of-region (save-excursion
                                (goto-char start-of-region)
                                (json-par--backward-spaces)
                                (point)))
        (setq end-of-region (save-excursion
                              (goto-char end-of-region)
                              (json-par--forward-spaces)
                              (point))))
      (json-par--delete-or-mark-region start-of-region
                                       end-of-region
                                       action
                                       mark-direction))))

(cl-defun json-par-delete-backward-char
    (n
     &rest options
     &key
     (hungry-delete t)
     (action-when-deleting-value-or-member 'delete)
     (action-when-deleting-string-from-inside 'delete-outer)
     (action-when-deleting-string-from-outside 'delete-outer)
     (action-when-deleting-successive-empty-lines 'delete-all)
     (action-when-joining-non-empty-lines 'just-delete)
     &allow-other-keys)
  "Delete the preceding N characters (maybe) electrically.

If N is negative, delete the following characters.

- If the region is active, delete it.

- After \"extra spaces\" and HUNGRY-DELETE is non-nil, delete it.

  \"Extra spaces\" are:
  - spaces after proper indentation,
  - any spaces between a key and colon,
  - any spaces between a value and comma,
  - more than one spaces between other combination of tokens, or
  - trailing spaces at the end of a line.

- Inside spaces before indentation and HUNGRY-DELETE is non-nil, or the point is
  just after a line break, join the current line and the previous line.  See
  `json-par--join-line-backward' for details and
  ACTION-WHEN-DELETING-SUCCESSIVE-EMPTY-LINES and
  ACTION-WHEN-JOINING-NON-EMPTY-LINES parameters.  The former is default to
  `delete-all' when called from Lisp program, or the value of variable
  `json-par-action-when-deleting-successive-empty-lines' when called
  interactively.  The latter is default to `just-delete' when called from Lisp
  program, or the value of variable
  `json-par-action-when-joining-non-empty-lines' when called interactively.

- After other non-extra spaces or HUNGRY-DELETE is nil, delete one space.

- Just after a string, action depends on
  ACTION-WHEN-DELETING-STRING-FROM-OUTSIDE.  It is one of the following:

  - `delete-outer': delete whole string.
  - `delete-inner': delete contents of the string if not empty.  Delete whole
    string otherwise.
  - `mark-outer': mark whole string.
  - `mark-or-delete-outer': mark whole string.  If it is already marked, delete
    it.
  - `mark-inner': mark contents of the string if not empty.  Mark whole string
    otherwise.
  - `enter': move the point into the string.

  The default value is `delete-outer' when called from Lisp program, or the
  value of variable `json-par-action-when-deleting-string-from-outside' when
  called interactively.

- Just after a double quote at the beginning of a string, action
  depends on ACTION-WHEN-DELETING-STRING-FROM-INSIDE.  It is one of the
  following:

  - `delete-outer': delete whole string.
  - `delete-inner': delete contents of the string if not empty.  Delete whole
    string otherwise.
  - `mark-outer': mark whole string.
  - `mark-inner': mark contents of the string if not empty.  Mark whole string
    otherwise.
  - `mark-or-delete-inner': mark contents of the string.  Mark whole string
    otherwise.  If already marked, delete it.
  - `exit': move the point out of the string.
  - `none': do nothing.

  The default value is `delete-outer' when called from Lisp program, or the
  value of variable `json-par-action-when-deleting-string-from-inside' when
  called interactively.

- Inside a string, delete one character.

- Inside or just after a number, delete one character.

- Otherwise, delete/mark the preceding token.  See
  `json-par-delete-backward-token-or-list' for details.
  ACTION-WHEN-DELETING-VALUE-OR-MEMBER,
  ACTION-WHEN-DELETING-BRACKETS-FROM-INSIDE,
  ACTION-WHEN-DELETING-BRACKETS-FROM-OUTSIDE,
  ACTION-WHEN-DELETING-COLON, and
  ACTION-WHEN-DELETING-COMMA are passed to it.

The default value of HUNGRY-DELETE is t when called from Lisp program, or the
value of variable `json-par-hungry-delete' when called interactively.

OPTIONS holds keywords arguments."
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)
    :hungry-delete
    json-par-hungry-delete
    :action-when-deleting-value-or-member
    json-par-action-when-deleting-value-or-member
    :action-when-deleting-brackets-from-inside
    json-par-action-when-deleting-brackets-from-inside
    :action-when-deleting-brackets-from-outside
    json-par-action-when-deleting-brackets-from-outside
    :action-when-deleting-string-from-inside
    json-par-action-when-deleting-string-from-inside
    :action-when-deleting-string-from-outside
    json-par-action-when-deleting-string-from-outside
    :action-when-deleting-colon
    json-par-action-when-deleting-colon
    :action-when-deleting-comma
    json-par-action-when-deleting-comma
    :action-when-deleting-successive-empty-lines
    json-par-action-when-deleting-successive-empty-lines
    :action-when-joining-non-empty-lines
    json-par-action-when-joining-non-empty-lines))
  (if (< n 0)
      (apply #'json-par-delete-forward-char (- n) options)
    (dotimes (_ n)
      (json-par--delete-backward-char-1
       hungry-delete
       action-when-deleting-value-or-member
       action-when-deleting-string-from-inside
       action-when-deleting-string-from-outside
       action-when-deleting-successive-empty-lines
       action-when-joining-non-empty-lines
       options))))

(cl-defun json-par-delete-forward-char
    (n
     &rest options
     &key
     (hungry-delete t)
     (action-when-deleting-value-or-member 'delete)
     (action-when-deleting-string-from-inside 'delete-outer)
     (action-when-deleting-string-from-outside 'delete-outer)
     (action-when-deleting-successive-empty-lines 'delete-all)
     (action-when-joining-non-empty-lines 'just-delete)
     &allow-other-keys)
  "Delete the following N characters (maybe) electrically.

If N is negative, delete the preceding characters.

- If the region is active, delete it.

- Before \"extra spaces\" and HUNGRY-DELETE is non-nil, delete it.

  \"Extra spaces\" are:
  - spaces after proper indentation,
  - any spaces between a key and colon,
  - any spaces between a value and comma, or
  - more than one spaces between other combination of tokens.

- Inside trailing spaces at the end of the line and HUNGRY-DELETE is non-nil, or
  the point is just before a line break, join the current line and the next
  line.  See `json-par--join-line-forward' for details and
  ACTION-WHEN-JOINING-NON-EMPTY-LINES parameters.  The former is default to
  `delete-all' when called from Lisp program, or the value of variable
  `json-par-action-when-deleting-successive-empty-lines' when called
  interactively.  The latter is default to `just-delete' when called from Lisp
  program, or the value of variable
  `json-par-action-when-joining-non-empty-lines' when called interactively.

- Before other non-extra spaces or HUNGRY-DELETE is nil, delete one space.

- Just before a string, action depends on
  ACTION-WHEN-DELETING-STRING-FROM-OUTSIDE.  It is one of the following:

  - `delete-outer': delete whole string.
  - `delete-inner': delete contents of the string if not empty.  Delete whole
    string otherwise.
  - `mark-outer': mark whole string.
  - `mark-or-delete-outer': mark whole string.  If it is already marked, delete
    it.
  - `mark-inner': mark contents of the string if not empty.  Mark whole string
    otherwise.
  - `enter': move the point into the string.

  The default value is `delete-outer' when called from Lisp program, or the
  value of variable `json-par-action-when-deleting-string-from-outside' when
  called interactively.

- Just before a double quote at the end of a string, action depends on
  ACTION-WHEN-DELETING-STRING-FROM-INSIDE.  It is one of the following:

  - `delete-outer': delete whole string.
  - `delete-inner': delete contents of the string if not empty.  Delete whole
    string otherwise.
  - `mark-outer': mark whole string.
  - `mark-inner': mark contents of the string if not empty.  Mark whole string
    otherwise.
  - `mark-or-delete-inner': mark contents of the string.  Mark whole string
    otherwise.  If already marked, delete it.
  - `exit': move the point out of the string.
  - `none': do nothing.

  The default value is `delete-outer' when called from Lisp program, or the
  value of variable `json-par-action-when-deleting-string-from-inside' when
  called interactively.

- Inside a string, delete one character.

- Inside or just before a number, delete one character.

- Otherwise, delete/mark the next token.  See
  `json-par-delete-forward-token-or-list' for details.
  ACTION-WHEN-DELETING-VALUE-OR-MEMBER,
  ACTION-WHEN-DELETING-BRACKETS-FROM-INSIDE,
  ACTION-WHEN-DELETING-BRACKETS-FROM-OUTSIDE,
  ACTION-WHEN-DELETING-COLON, and
  ACTION-WHEN-DELETING-COMMA are passed to it.

The default value of HUNGRY-DELETE is t when called from Lisp program, or the
value of variable `json-par-hungry-delete' when called interactively.

OPTIONS holds keywords arguments."
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)
    :hungry-delete
    json-par-hungry-delete
    :action-when-deleting-value-or-member
    json-par-action-when-deleting-value-or-member
    :action-when-deleting-brackets-from-inside
    json-par-action-when-deleting-brackets-from-inside
    :action-when-deleting-brackets-from-outside
    json-par-action-when-deleting-brackets-from-outside
    :action-when-deleting-string-from-inside
    json-par-action-when-deleting-string-from-inside
    :action-when-deleting-string-from-outside
    json-par-action-when-deleting-string-from-outside
    :action-when-deleting-colon
    json-par-action-when-deleting-colon
    :action-when-deleting-comma
    json-par-action-when-deleting-comma
    :action-when-deleting-successive-empty-lines
    json-par-action-when-deleting-successive-empty-lines
    :action-when-joining-non-empty-lines
    json-par-action-when-joining-non-empty-lines))
  (if (< n 0)
      (apply #'json-par-delete-backward-char (- n) options)
    (dotimes (_ n)
      (json-par--delete-forward-char-1
       hungry-delete
       action-when-deleting-value-or-member
       action-when-deleting-string-from-inside
       action-when-deleting-string-from-outside
       action-when-deleting-successive-empty-lines
       action-when-joining-non-empty-lines
       options))))

(defun json-par--delete-backward-char-1
    (hungry-delete
     action-when-deleting-value-or-member
     action-when-deleting-string-from-inside
     action-when-deleting-string-from-outside
     action-when-deleting-successive-empty-lines
     action-when-joining-non-empty-lines
     options)
  "Delete a preceding character (maybe) electrically.

See `json-par-delete-backward-char' for details and OPTIONS, HUNGRY-DELETE,
ACTION-WHEN-DELETING-VALUE-OR-MEMBER,
ACTION-WHEN-DELETING-STRING-FROM-INSIDE,
ACTION-WHEN-DELETING-STRING-FROM-OUTSIDE,
ACTION-WHEN-DELETING-SUCCESSIVE-EMPTY-LINES, and
ACTION-WHEN-JOINING-NON-EMPTY-LINES."
  (let ((current-atom (json-par--current-atom))
        (string-like-beginning-position
         (json-par--string-like-beginning-position)))
    (when (eq (json-par-token-start current-atom) (point))
      (setq current-atom nil))
    (cond
     ;; Deleting the region
     ((use-region-p)
      (delete-region (region-beginning) (region-end)))

     ;; Deleting a line-break (optionally followed by spaces if hungry)
     ((or (bolp)
          (and hungry-delete
               (save-excursion
                 (skip-chars-backward "\s\t")
                 (bolp))))
      (if (and hungry-delete
               (< (json-par--indentation-offset) (current-column)))
          (json-par-indent-line)
        (json-par--join-line-backward
         hungry-delete
         nil
         action-when-deleting-successive-empty-lines
         action-when-joining-non-empty-lines)))

     ;; Deleting spaces
     ((memq (char-before) '(?\s ?\t))
      (if hungry-delete
          (let ((start (save-excursion
                         (skip-chars-backward "\s\t")
                         (point)))
                (end (point)))
            (when (and
                   ;; Leave one space when deleting more than one spaces,
                   (< 1 (- end start))
                   ;; unless we have spaces after point,
                   (not (memq (char-after) '(?\s ?\t)))
                   ;; or at the beginning/end of the line.
                   (not (bolp))
                   (not (eolp))
                   (or
                    ;; Do not leave one space before colon or comma...
                    (not (memq (char-after) '(?: ?,)))
                    ;; unless it is just after colon, comma, or open bracket.
                    (memq (char-before start) '(?: ?, ?\[ ?\( ?{))))
              (setq start (1+ start)))
            (if (and (= (- end start) 1)
                     (not (memq (char-after) '(?\s ?\t ?\n))))
                ;; Deleting one space.
                (json-par--delete-one-space-after start)
              (delete-region start end)))
        (delete-char -1)))

     ;; Inside or just after a string or an object key
     ((json-par-token-string-p current-atom)
      (json-par--delete-backward-char-for-string
       current-atom
       action-when-deleting-string-from-inside
       action-when-deleting-string-from-outside))

     ;; Inside or just after a number
     ((json-par-token-number-p current-atom)
      (if (eq (json-par-token-length current-atom) 1)
          (json-par-delete-current-value-or-key
           (point)
           nil
           'preceding
           'delete
           'backward)
        (delete-char -1)))

     ;; Deleting a comment starter
     ((and string-like-beginning-position
           (< 0 (- (point) string-like-beginning-position) 3))
      (json-par--delete-or-mark-or-exit-string
       (save-excursion
         (goto-char string-like-beginning-position)
         (json-par-forward-token-or-list-or-comment))
       action-when-deleting-string-from-inside
       'backward))

     ;; After a comment closer
     ((and
       (not string-like-beginning-position)
       (eq (char-before) ?/)
       (eq (char-before (1- (point))) ?*)
       (save-excursion (forward-comment -1)))
      (json-par--delete-or-mark-or-enter-string
       (save-excursion (json-par-backward-token-or-list-or-comment))
       action-when-deleting-string-from-outside
       'backward))

     ;; On a comment closer
     ((and
       string-like-beginning-position
       (save-excursion
         (goto-char string-like-beginning-position)
         (looking-at "/\\*"))
       (eq (1+ (point))
           (save-excursion
             (goto-char string-like-beginning-position)
             (forward-comment 1)
             (point))))
      (json-par--delete-or-mark-or-enter-string
       (save-excursion
         (goto-char string-like-beginning-position)
         (json-par-forward-token-or-list-or-comment))
       action-when-deleting-string-from-outside
       'backward))

     ;; Inside a comment
     (string-like-beginning-position
      (delete-char -1))

     ;; Otherwise
     (t
      (apply #'json-par-delete-backward-token-or-list
             action-when-deleting-value-or-member
             (json-par--plist-except
              '(:hungry-delete
                :action-when-deleting-value-or-member
                :action-when-deleting-string-from-inside
                :action-when-deleting-string-from-outside
                :action-when-deleting-successive-empty-lines
                :action-when-joining-non-empty-lines)
              options))))))

(defun json-par--delete-forward-char-1
    (hungry-delete
     action-when-deleting-value-or-member
     action-when-deleting-string-from-inside
     action-when-deleting-string-from-outside
     action-when-deleting-successive-empty-lines
     action-when-joining-non-empty-lines
     options)
  "Delete a following character (maybe) electrically.

See `json-par-delete-forward-char' for details and OPTIONS, HUNGRY-DELETE,
ACTION-WHEN-DELETING-VALUE-OR-MEMBER,
ACTION-WHEN-DELETING-STRING-FROM-INSIDE,
ACTION-WHEN-DELETING-STRING-FROM-OUTSIDE,
ACTION-WHEN-DELETING-SUCCESSIVE-EMPTY-LINES, and
ACTION-WHEN-JOINING-NON-EMPTY-LINES."
  (let ((current-atom (json-par--current-atom))
        (string-like-beginning-position
         (json-par--string-like-beginning-position)))
    (when (eq (json-par-token-end current-atom) (point))
      (setq current-atom nil))
    (cond
     ;; Deleting the region
     ((use-region-p)
      (delete-region (region-beginning) (region-end)))

     ;; Deleting a line-break
     ((eolp)
      (json-par--join-line-forward
       nil
       hungry-delete
       action-when-deleting-successive-empty-lines
       action-when-joining-non-empty-lines))

     ;; Deleting spaces
     ((memq (char-after) '(?\s ?\t))
      (if hungry-delete
          (let ((start (point))
                (end (save-excursion
                       (skip-chars-forward "\s\t")
                       (point))))
            (when (and
                   ;; Leave one space when deleting more than one spaces,
                   (< 1 (- end start))
                   ;; unless we have spaces before point,
                   (not (memq (char-before) '(?\s ?\t)))
                   ;; or at the beginning/end of the line.
                   (not (bolp))
                   (not (looking-at "[\s\t]*$"))
                   (or
                    ;; Do not leave one space before colon or comma...
                    (not (memq (char-after end) '(?: ?,)))
                    ;; unless it is just after colon, comma, or open bracket.
                    (memq (char-before start) '(?: ?, ?\[ ?\( ?{))))
              (setq end (1- end)))
            (if (and (= (- end start) 1)
                     (not (memq (char-before) '(?\s ?\t \n))))
                ;; Deleting one space.
                (json-par--delete-one-space-after start)
              (delete-region start end)))
        (delete-char 1)))

     ;; Inside or just before a string or an object key
     ((json-par-token-string-p current-atom)
      (json-par--delete-forward-char-for-string
       current-atom
       action-when-deleting-string-from-inside
       action-when-deleting-string-from-outside))

     ;; Inside or just before a number
     ((json-par-token-number-p current-atom)
      (if (eq (json-par-token-length current-atom) 1)
          (json-par-delete-current-value-or-key
           (point)
           nil
           'following
           'delete
           'forward)
        (delete-char 1)))

     ;; Before a comment starter
     ((and
       (not string-like-beginning-position)
       (looking-at "/[/*]")
       (save-excursion (forward-comment 1)))
      (json-par--delete-or-mark-or-enter-string
       (save-excursion (json-par-forward-token-or-list-or-comment))
       action-when-deleting-string-from-outside
       'forward))

     ;; On a comment starter
     ((and
       string-like-beginning-position
       (= (1- (point)) string-like-beginning-position))
      (json-par--delete-or-mark-or-enter-string
       (save-excursion
         (goto-char string-like-beginning-position)
         (json-par-forward-token-or-list-or-comment))
       action-when-deleting-string-from-outside
       'forward))

     ;; Deleting a comment closer
     ((and
       string-like-beginning-position
       (save-excursion
         (goto-char string-like-beginning-position)
         (forward-comment 1))
       (save-excursion
         (goto-char string-like-beginning-position)
         (looking-at "/\\*"))
       (< 0
          (- (save-excursion
               (goto-char string-like-beginning-position)
               (forward-comment 1)
               (point))
             (point))
          3))
      (json-par--delete-or-mark-or-exit-string
       (save-excursion
         (goto-char string-like-beginning-position)
         (json-par-forward-token-or-list-or-comment))
       action-when-deleting-string-from-inside
       'forward))

     ;; Inside a comment
     (string-like-beginning-position
      (delete-char 1))

     ;; Otherwise
     (t
      (apply #'json-par-delete-forward-token-or-list
             action-when-deleting-value-or-member
             (json-par--plist-except
              '(:hungry-delete
                :action-when-deleting-value-or-member
                :action-when-deleting-string-from-inside
                :action-when-deleting-string-from-outside
                :action-when-deleting-successive-empty-lines
                :action-when-joining-non-empty-lines)
              options))))))

(defun json-par--delete-one-space-after (start)
  "Delete one space after START and other related spaces.

After an open bracket or before a close bracket, also delete spaces at opposite
bracket.

Between members of a one-line array/object, also delete spaces inside the
array/object."
  (save-excursion
    (goto-char start)
    (delete-char 1)
    (let (whole-list)
      (cond
       ;; Beginning of an array/object.
       ;; Also delete spaces before opposite bracket.
       ((memq (char-before) '(?\[ ?\( ?{))
        (backward-char)
        (setq whole-list (json-par-forward-token-or-list))
        (when (json-par-token-matching-brackets-p whole-list)
          (backward-char)
          (json-par--delete-spaces-between-tokens
           (save-excursion
             (json-par--backward-spaces)
             (point))
           (point))))

       ;; End of an array/object.
       ;; Also delete spaces before opposite bracket.
       ((memq (char-after) '(?\] ?\) ?}))
        (forward-char)
        (setq whole-list (json-par-backward-token-or-list))
        (when (json-par-token-matching-brackets-p whole-list)
          (forward-char)
          (json-par--delete-spaces-between-tokens
           (point)
           (save-excursion
             (json-par--forward-spaces)
             (point)))))

       ;; Between members.
       ;; Also delete spaces inside the array/object.
       ((eq (char-before start) ?\,)
        (json-par-up-backward)
        (setq whole-list (json-par-forward-token-or-list))
        (when (and (json-par-token-matching-brackets-p whole-list)
                   (json-par-token-one-line-p whole-list))
          (json-par--delete-spaces-between-tokens
           (json-par-token-start whole-list)
           (json-par-token-end whole-list))))))))

(defun json-par--delete-spaces-between-tokens (start end)
  "Delete spaces and line breaks between tokens from START to END.

Spaces and line breaks in strings are not affected."
  (save-excursion
    (goto-char start)
    (setq end (copy-marker end))
    (delete-region (point)
                   (min end
                        (save-excursion
                          (skip-chars-forward "\s\t\n")
                          (point))))
    (while (< (point) end)
      (while (forward-comment 1)
        (delete-region (point)
                       (min end
                            (save-excursion
                              (skip-chars-forward "\s\t\n")
                              (point)))))
      (json-par-forward-token)
      (when (< (point) end)
        (delete-region (point)
                       (min end
                            (save-excursion
                              (skip-chars-forward "\s\t\n")
                              (point))))))
    (json-par--free-marker end)))

(defun json-par--delete-backward-char-for-string
    (current-atom
     action-when-deleting-string-from-inside
     action-when-deleting-string-from-outside)
  "Delete the preceding character in a string token CURRENT-ATOM.

See `json-par-delete-backward-char' for  ACTION-WHEN-DELETING-STRING-FROM-INSIDE
and ACTION-WHEN-DELETING-STRING-FROM-OUTSIDE."
  (cond
   ;; After the closing quote
   ((= (point) (json-par-token-end current-atom))
    (json-par--delete-or-mark-or-enter-string
     current-atom
     action-when-deleting-string-from-outside
     'backward))

   ;; After the opening quote
   ((= (point) (1+ (json-par-token-start current-atom)))
    (json-par--delete-or-mark-or-exit-string
     current-atom
     action-when-deleting-string-from-inside
     'backward))

   ;; Otherwise
   (t
    (delete-char -1))))

(defun json-par--delete-forward-char-for-string
    (current-atom
     action-when-deleting-string-from-inside
     action-when-deleting-string-from-outside)
  "Delete the following character in a string token CURRENT-ATOM.

See `json-par-delete-forward-char' for  ACTION-WHEN-DELETING-STRING-FROM-INSIDE
and ACTION-WHEN-DELETING-STRING-FROM-OUTSIDE."
  (cond
   ;; Before the opening quote
   ((= (point) (json-par-token-start current-atom))
    (json-par--delete-or-mark-or-enter-string
     current-atom
     action-when-deleting-string-from-outside
     'forward))

   ;; Before the closing quote
   ((= (1+ (point)) (json-par-token-end current-atom))
    (json-par--delete-or-mark-or-exit-string
     current-atom
     action-when-deleting-string-from-inside
     'forward))

   ;; Otherwise
   (t
    (delete-char 1))))

(defun json-par--delete-or-mark-or-exit-string (current-atom action direction)
  "Delete, mark, or exit from a string token CURRENT-ATOM depending on ACTION.

See `json-par-action-when-deleting-string-from-inside' for details of ACTION.

Assuming the point is after the string opener if DIRECTION is `backward', or
before the closer otherwise.  This also affects the direction of marking."
  (cond
   ((eq action 'none)
    nil)

   ((eq action 'exit)
    (if (eq direction 'backward)
        (goto-char (json-par-token-start current-atom))
      (goto-char (json-par-token-end current-atom))))

   (t
    (json-par--delete-or-mark-string
     current-atom
     action
     (if (eq direction 'backward) 'forward 'backward)
     nil))))

(defun json-par--delete-or-mark-or-enter-string (current-atom action direction)
  "Delete, mark, or enter to a string token CURRENT-ATOM depending on ACTION.

See `json-par-action-when-deleting-string-from-outside' for details of ACTION.

Assuming the point is after the string closer if DIRECTION is `backward', or
before the opener otherwise.  This also affects the direction of marking."
  (if (eq action 'enter)
      (let ((region-of-string-like-body
             (json-par--region-of-string-like-body
              (json-par-token-start current-atom)
              t)))
        (if (eq direction 'backward)
            (goto-char (cdr region-of-string-like-body))
          (goto-char (car region-of-string-like-body))))
    (json-par--delete-or-mark-string current-atom action direction nil)))

(defun json-par--delete-or-mark-string
    (current-atom action direction allow-empty)
  "Delete or mark a string token CURRENT-ATOM depending on ACTION.

See `json-par-action-when-deleting-string-from-inside' or
`json-par-action-when-deleting-string-from-outside' for details of ACTION.
ACTION must not be `enter' nor `exit'.

If the point is middle of the region to be marked, mark backward if
DIRECTION is a symbol `backward' and mark forward otherwise.

If ALLOW-EMPTY is non-nil, ACTION is `delete-inner', `mark-inner', or
`mark-or-delete-inner', and the string is empty, delete/mark the empty region
rather than outer."
  (cond
   ;; Delete outer
   ((eq action 'delete-outer)
    (if (json-par-token-comment-p current-atom)
        (json-par--delete-comment (json-par-token-start current-atom))
      (json-par-delete-current-value-or-key
       (json-par-token-start current-atom)
       nil
       (if (eq direction 'backward) 'preceding 'following)
       'delete
       direction)))

   ;; Mark outer
   ((memq action '(mark-outer mark-or-delete-outer))
    (if (json-par-token-string-p current-atom)
        (progn
          (json-par-delete-current-value-or-key
           (if (eq direction 'backward)
               (json-par-token-end current-atom)
             (json-par-token-start current-atom))
           nil
           (if (eq direction 'backward) 'preceding 'following)
           (if (eq action 'mark-outer) 'mark 'mark-or-delete)
           direction))
      (json-par--delete-or-mark-region
       (json-par-token-start current-atom)
       (if (json-par-token-single-line-comment-p current-atom)
           (1- (json-par-token-end current-atom))
         (json-par-token-end current-atom))
       (if (eq action 'mark-outer) 'mark 'mark-or-delete)
       direction)))

   ;; Delete/mark inner
   ((memq action '(delete-inner mark-inner mark-or-delete-inner))
    (let ((region-of-string-like-body (json-par--region-of-string-like-body
                                       (json-par-token-start current-atom)
                                       t)))
      (if (and (not allow-empty)
               (= (car region-of-string-like-body)
                  (cdr region-of-string-like-body)))
          (json-par--delete-or-mark-string
           current-atom
           (cond
            ((eq action 'delete-inner) 'delete-outer)
            ((eq action 'mark-inner) 'mark-outer)
            ((eq action 'mark-or-delete-inner) 'mark-or-delete-outer))
           direction
           allow-empty)
        (when (eq (json-par--delete-or-mark-region
                   (car region-of-string-like-body)
                   (cdr region-of-string-like-body)
                   (cond
                    ((eq action 'delete-inner) 'delete)
                    ((eq action 'mark-inner) 'mark)
                    ((eq action 'mark-or-delete-inner) 'mark-or-delete))
                   direction)
                  'delete)
          (goto-char (car region-of-string-like-body))))))))

(defun json-par--indentation-offset ()
  "Return the proper indentation amount for the current line."
  (let ((original-column (current-column))
        (original-indentation-string (delete-and-extract-region
                                      (line-beginning-position)
                                      (save-excursion
                                        (back-to-indentation)
                                        (point))))
        indentation-offset)
    (json-par-indent-line)
    (back-to-indentation)
    (setq indentation-offset (current-column))
    (delete-region (line-beginning-position) (point))
    (insert original-indentation-string)
    (forward-line 0)
    (forward-char original-column)
    indentation-offset))

(defun json-par--delete-comment (&optional start)
  "Delete a comment starting at START or current one if omitted."
  (unless start
    (setq start (json-par--string-like-beginning-position)))
  (goto-char start)
  (forward-comment 1)
  (when (bolp)
    (backward-char))
  (skip-chars-forward "\s\t")
  (delete-region start (point))
  (unless (save-excursion (skip-chars-backward "\s\t") (bolp))
    (delete-horizontal-space))
  (when (save-excursion (forward-line 0) (looking-at "[\s\t]*$"))
    (delete-horizontal-space)
    (delete-char -1)
    (forward-char)
    (skip-chars-forward "\s\t")))

(defun json-par-delete-object-value (&optional action)
  "Delete or mark the object value of the current member.

ACTION is one of `delete', `mark-or-delete', or `mark', defaults to `delete'
when called from Lisp program, or the value of
`json-par-action-when-deleting-value-or-member' variable when called
interactively.

- `delete': delete the value/member unconditionally.
- `mark-or-delete': delete only if the region to be deleted is marked.
  Otherwise, mark the region.
- `mark': mark the region to be deleted unconditionally."
  (interactive
   (list json-par-action-when-deleting-value-or-member))
  (unless action
    (setq action 'delete))
  (json-par-delete-current-value-or-key
   (save-excursion
     (json-par-beginning-of-object-value)
     (point))
   t
   nil
   action))

(defun json-par-delete-object-values ()
  "Delete the object values of the current object."
  (interactive)
  (json-par--find-member
   (lambda (_)
     (json-par-delete-object-value)
     nil)))

(defun json-par-delete-backward-parent (n &optional action)
  "Delete or mark the N th parent array/object containing the point.

ACTION is one of `delete', `mark-or-delete', or `mark', defaults to `delete'
when called from Lisp program, or the value of
`json-par-action-when-deleting-value-or-member' variable when called
interactively.

- `delete': delete the array/object unconditionally.
- `mark-or-delete': delete only if the region to be deleted is marked.
  Otherwise, mark the region.
- `mark': mark the region to be deleted unconditionally."
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)
    json-par-action-when-deleting-value-or-member))
  (unless action
    (setq action 'delete))
  (cond
   ((< n 0)
    (json-par-delete-forward-parent (- n) action))

   ((zerop n)
    nil)

   (t
    (json-par-up-forward n)
    (json-par-delete-current-value-or-key
     nil
     nil
     'preceding
     action
     'backward))))

(defun json-par-delete-forward-parent (n &optional action)
  "Delete or mark the N th parent array/object containing the point.

ACTION is one of `delete', `mark-or-delete', or `mark', defaults to `delete'
when called from Lisp program, or the value of
`json-par-action-when-deleting-value-or-member' variable when called
interactively.

- `delete': delete the array/object unconditionally.
- `mark-or-delete': delete only if the region to be deleted is marked.
  Otherwise, mark the region.
- `mark': mark the region to be deleted unconditionally."
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)
    json-par-action-when-deleting-value-or-member))
  (unless action
    (setq action 'delete))
  (cond
   ((< n 0)
    (json-par-delete-backward-parent (- n) action))

   ((zerop n)
    nil)

   (t
    (json-par-up-backward n)
    (json-par-delete-current-value-or-key
     nil
     nil
     'following
     action
     'forward))))

(defun json-par-delete-inner (&optional action)
  "Delete or mark contents of the current value/key.

If the current value/key is a string, delete non empty contents of it.  If the
string/key contains only spaces, delete it.  If the string/key is empty, just go
inside the string/key.

ACTION is one of `delete', `mark-or-delete', or `mark', defaults to `delete'
when called from Lisp program, or the value of
`json-par-action-when-deleting-value-or-member' variable when called
interactively.

- `delete': delete the contents unconditionally.
- `mark-or-delete': delete only if the region to be deleted is marked.
  Otherwise, mark the region.
- `mark': mark the region to be deleted unconditionally."
  (interactive
   (list json-par-action-when-deleting-value-or-member))
  (unless action
    (setq action 'delete))
  (let ((pos (point))
        (action-inner (cond
                       ((eq action 'delete) 'delete-inner)
                       ((eq action 'mark-or-delete) 'mark-or-delete-inner)
                       ((eq action 'mark) 'mark-inner))))
    (json-par-down)
    (cond
     ((< pos (point))
      (goto-char pos)
      (skip-chars-forward "\s\t\n")
      (if (memq (char-after) '(?\[ ?\( ?{))
          (json-par--delete-or-mark-list
           (save-excursion (json-par-forward-token-or-list))
           action-inner
           'forward
           t)
        (json-par--delete-or-mark-string
         (save-excursion (json-par-forward-token-or-list-or-comment))
         action-inner
         'forward
         t)))

     ((< (point) pos)
      (goto-char pos)
      (skip-chars-backward "\s\t\n")
      (if (memq (char-before) '(?\] ?\) ?}))
          (json-par--delete-or-mark-list
           (save-excursion (json-par-backward-token-or-list))
           action-inner
           'backward
           t)
        (json-par--delete-or-mark-string
         (save-excursion (json-par-backward-token-or-list-or-comment))
         action-inner
         'backward
         t))))))

(defun json-par-delete-head-of-member (&optional action)
  "Delete or mark the key of the current member if any, or the value otherwise.

ACTION is one of `delete', `mark-or-delete', or `mark', defaults to `delete'
when called from Lisp program, or the value of
`json-par-action-when-deleting-value-or-member' variable when called
interactively.

- `delete': delete the token/object/array unconditionally.
- `mark-or-delete': delete only if the region to be deleted is marked.
  Otherwise, mark the region.
- `mark': mark the region to be deleted unconditionally."
  (interactive
   (list json-par-action-when-deleting-value-or-member))
  (unless action
    (setq action 'delete))
  (json-par-delete-current-value-or-key
   (save-excursion
     (json-par-beginning-of-member)
     (point))
   t
   'following
   action
   'forward))

(defun json-par-delete-backward-inside-of-parent (&optional action)
  "Delete or mark contents of the parent array/object.

ACTION is one of `delete', `mark-or-delete', or `mark', defaults to `delete'
when called from Lisp program, or the value of
`json-par-action-when-deleting-value-or-member' variable when called
interactively.

- `delete': delete the contents unconditionally.
- `mark-or-delete': delete only if the region to be deleted is marked.
  Otherwise, mark the region.
- `mark': mark the region to be deleted unconditionally."
  (interactive
   (list json-par-action-when-deleting-value-or-member))
  (unless action
    (setq action 'delete))
  (json-par-up-forward)
  (json-par-delete-inner action))

(defun json-par-delete-forward-inside-of-parent (&optional action)
  "Delete or mark contents of the parent array/object.

ACTION is one of `delete', `mark-or-delete', or `mark', defaults to `delete'
when called from Lisp program, or the value of
`json-par-action-when-deleting-value-or-member' variable when called
interactively.

- `delete': delete the contents unconditionally.
- `mark-or-delete': delete only if the region to be deleted is marked.
  Otherwise, mark the region.
- `mark': mark the region to be deleted unconditionally."
  (interactive
   (list json-par-action-when-deleting-value-or-member))
  (unless action
    (setq action 'delete))
  (json-par-up-backward)
  (json-par-delete-inner action))

(cl-defun json-par-join-line
    (&optional
     to-next
     &key
     (action-when-deleting-successive-empty-lines 'delete-all)
     (action-when-joining-non-empty-lines 'just-delete))
  "Join two lines.

If TO-NEXT is non-nil, join the current line and the next line.  Otherwise, join
the current line and the previous line.

See `json-par--join-line-backward' and `json-par--join-line-forward' for
details and ACTION-WHEN-DELETING-SUCCESSIVE-EMPTY-LINES and
ACTION-WHEN-JOINING-NON-EMPTY-LINES."
  (interactive
   (list
    current-prefix-arg
    :action-when-deleting-successive-empty-lines
    json-par-action-when-deleting-successive-empty-lines
    :action-when-joining-non-empty-lines
    json-par-action-when-joining-non-empty-lines))
  (if to-next
      (save-excursion
        (end-of-line)
        (json-par--join-line-forward
         t
         t
         action-when-deleting-successive-empty-lines
         action-when-joining-non-empty-lines))
    (save-excursion
      (forward-line 0)
      (json-par--join-line-backward
       t
       t
       action-when-deleting-successive-empty-lines
       action-when-joining-non-empty-lines))))

(defun json-par--join-line-backward (delete-preceding-spaces
                                     delete-following-spaces
                                     action-when-deleting-successive-empty-lines
                                     action-when-joining-non-empty-lines)
  "Join the current line and the previous line.

If DELETE-FOLLOWING-SPACES is non-nil, delete the following spaces.

- If the current line is empty:
  - When ACTION-WHEN-DELETING-SUCCESSIVE-EMPTY-LINES is `delete-all':
    Delete from point to the end of the previous non-empty line.

  - When ACTION-WHEN-DELETING-SUCCESSIVE-EMPTY-LINES is `delete-one':
    Delete from point to the end of the previous line.

  - When ACTION-WHEN-DELETING-SUCCESSIVE-EMPTY-LINES is `leave-one':
    Delete from point to the end of the next line of the previous non-empty
    line.  If the previous line is not empty, delete to the end of that line.

  Then, if DELETE-PRECEDING-SPACES is non-nil, delete the spaces
  before the point.

  Example (`|' is the point):

  `delete-all':
    [

       |
       1
    ]
    ↓
    [|
       1
    ]

  `delete-one':
    [

       |
       1
    ]
    ↓
    [
       |
       1
    ]

  `leave-one':
    [


       |
       1
    ]
    ↓
    [
       |
       1
    ]
    ↓
    [|
       1
    ]


- If the previous line is empty:
  - When ACTION-WHEN-DELETING-SUCCESSIVE-EMPTY-LINES is `delete-all':
    Delete from point to the beginning of the successive empty lines.

  - When ACTION-WHEN-DELETING-SUCCESSIVE-EMPTY-LINES is `delete-one':
    Delete from point until the beginning of the previous line.

  - When ACTION-WHEN-DELETING-SUCCESSIVE-EMPTY-LINES is `leave-one':
    Delete from point to the beginning of the second line of
    the successive empty lines.  If the successive empty lines have only one
    line, delete until the beginning of that line.

  If DELETE-PRECEDING-SPACES is nil, keep the spaces in the first line.

  Example (`|' is the point):

  `delete-all'
    [


       |1
    ]
    ↓
    [
       |1
    ]

  `delete-one'
    [


       |1
    ]
    ↓
    [

       |1
    ]

  `leave-one'
    [



       |1
    ]
    ↓
    [

       |1
    ]
    ↓
    [
       |1
    ]


- If the point is before the first member of array/object, or after the last
  member, convert the current array/object into one-line.  The spaces around the
  point is deleted according to DELETE-PRECEDING-SPACES and
  DELETE-PRECEDING-SPACES.

  Example (`|' is the point):

    [
      |1,
      2,
      3
    ]
    ↓
    [ |1, 2, 3 ]

    [
      1,
      2,
      3
    |]
    ↓
    [ 1, 2, 3| ]

- If the point is between members and each member is on its own line, action
  depends on ACTION-WHEN-JOINING-NON-EMPTY-LINES:

  - `just-delete': just delete the line break.

    Example (`|' is the point):

    [
      1,
    |  2,
      3
    ]
    ↓
    [
      1,|  2,
      3
    ]

  - `delete-line-breaks-between-members': delete all line breaks between members
    (but not before/after members).

    Example (`|' is the point):

    [
      1,
    |  2,
      3
    ]
    ↓
    [
      1,|  2, 3
    ]

  - `delete-line-breaks-inside-brackets': delete all line breaks inside the
    current array/object.

    Example (`|' is the point):

    [
      1,
    |  2,
      3
    ]
    ↓
    [ 1,|  2, 3 ]

  The spaces around the point is deleted according to DELETE-PRECEDING-SPACES
  and DELETE-PRECEDING-SPACES.

- Otherwise, the lines are just joined.  The spaces around the point is deleted
  according to DELETE-PRECEDING-SPACES and DELETE-PRECEDING-SPACES.

After the lines are joined, a space is inserted depending on the characters
around the point."
  (let ((previous-token (save-excursion (json-par-backward-token)))
        (next-token (save-excursion (json-par-forward-token)))
        current-member-index
        current-offset-from-member
        start
        (end (if delete-following-spaces
                 (save-excursion
                   (skip-chars-forward "\s\t")
                   (point))
               (point)))
        (string-like-beginning-position
         (json-par--string-like-beginning-position)))
    (forward-line 0)
    (cond
     ;; Empty line
     ((save-excursion
        (skip-chars-forward "\s\t")
        (eolp))
      (setq start
            (save-excursion
              (cond
               ((eq action-when-deleting-successive-empty-lines 'delete-one)
                (forward-line 0)
                (backward-char))

               ((eq action-when-deleting-successive-empty-lines 'leave-one)
                (if (save-excursion
                      (forward-line -1)
                      (skip-chars-forward "\s\t")
                      (not (eolp)))
                    ;; Only one line is left.
                    ;; Delete it.
                    (progn
                      (forward-line 0)
                      (backward-char))
                  ;; Leave one
                  (skip-chars-backward "\s\t\n")
                  (forward-line)
                  (skip-chars-forward "\s\t")))

               ((eq action-when-deleting-successive-empty-lines 'delete-all)
                (skip-chars-backward "\s\t\n")
                (skip-chars-forward "\s\t")))
              (point)))
      (delete-region start end)
      (when delete-preceding-spaces
        (if (and (save-excursion
                   (skip-chars-backward "\s\t")
                   (bolp))
                 (<= (json-par--indentation-offset) (current-column)))
            (json-par-indent-line)
          (delete-horizontal-space t))))

     ;; The previous line is empty.
     ((save-excursion
        (forward-line -1)
        (skip-chars-forward "\s\t")
        (eolp))
      (setq start
            (save-excursion
              (cond
               ((eq action-when-deleting-successive-empty-lines 'delete-one)
                (backward-char))

               ((eq action-when-deleting-successive-empty-lines 'leave-one)
                (if (save-excursion
                      (forward-line -1)
                      (or
                       (bobp)
                       (progn
                         (forward-line -1)
                         (skip-chars-forward "\s\t")
                         (not (eolp)))))
                    ;; Only one line is left.
                    ;; Delete it.
                    (backward-char)
                  ;; Leave one
                  (skip-chars-backward "\s\t\n")
                  (if (bobp)
                      (forward-line 1)
                    (forward-line 2))
                  (skip-chars-forward "\s\t")))

               ((eq action-when-deleting-successive-empty-lines 'delete-all)
                (skip-chars-backward "\s\t\n")
                (unless (bobp)
                  (forward-line 1))
                (skip-chars-forward "\s\t")))
              (point)))
      (delete-region start end)
      (when delete-preceding-spaces
        (if (and (save-excursion
                   (skip-chars-backward "\s\t")
                   (bolp))
                 (<= (json-par--indentation-offset) (current-column)))
            (json-par-indent-line)
          (delete-horizontal-space t))))

     ;; The current line and the previous line belong to a multiline string or a
     ;; multiline comment.
     (string-like-beginning-position
      (delete-region
       (save-excursion
         (backward-char)
         (when delete-preceding-spaces
           (skip-chars-backward "\s\t"))
         (point))
       end))

     ;; The current line starts with a single-line comment and the previous line
     ;; ends with a single-line comment.
     ((and (looking-at "[\s\t]*//")
           (save-excursion
             (backward-char)
             (json-par--string-like-beginning-position)))
      (setq end (save-excursion
                  (skip-chars-forward "\s\t")
                  (skip-chars-forward "/")
                  (when delete-following-spaces
                    (skip-chars-forward "\s\t"))
                  (point)))
      (setq start
            (save-excursion
              (forward-line -1)
              (when (and (memq action-when-deleting-successive-empty-lines
                               '(leave-one delete-all))
                         (looking-at "^[\s\t]*//+[\s\t]*$"))
                (while (and (looking-at "[\s\t]*//+[\s\t]*$")
                            (zerop (forward-line -1)))
                  t)
                (unless (bobp)
                  (forward-line)
                  (when (eq action-when-deleting-successive-empty-lines
                            'leave-one)
                    (forward-line)
                    (when (json-par--same-line-p (point) end)
                      (forward-line -1)))))
              (end-of-line)
              (when delete-preceding-spaces
                (skip-chars-backward "\s\t"))
              (point)))
      (delete-region start end))

     ;; The previous line ends with a single-line comment.
     ((save-excursion
        (backward-char)
        (json-par--string-like-beginning-position))
      (delete-region (line-beginning-position) end)
      ;; Bring that comment to the end of the this line, then join the lines.
      (let* ((string-like-beginning-position
              (save-excursion
                (backward-char)
                (json-par--string-like-beginning-position)))
             (have-space-before-comment
              (save-excursion
                (goto-char string-like-beginning-position)
                (memq (char-before string-like-beginning-position)
                      '(?\s ?\t ?\n nil))))
             (comment-string
              (delete-and-extract-region
               string-like-beginning-position
               (1- (point)))))
        (end-of-line)
        (when (json-par--string-like-beginning-position)
          (goto-char (json-par--string-like-beginning-position)))
        (when (and have-space-before-comment
                   (not (memq (char-before) '(?\s ?\t ?\n nil))))
          (insert-char ?\s))
        (insert comment-string)
        (delete-region
         (save-excursion
           (when delete-preceding-spaces
             (skip-chars-backward "\s\t"))
           (point))
         (save-excursion
           (skip-chars-forward "/")
           (when delete-following-spaces
             (skip-chars-forward "\s\t"))
           (point)))
        (json-par--fixup-space-after-join-line)
        (forward-line 0)
        (json-par--join-line-backward
         delete-preceding-spaces
         delete-following-spaces
         action-when-deleting-successive-empty-lines
         action-when-joining-non-empty-lines)))

     ;; After the last member.
     ;; [
     ;;   1,
     ;;   2,
     ;;   3
     ;; |]
     ;; ↓
     ;; [ 1, 2, 3| ]
     ((json-par--after-last-member-p next-token)
      (delete-region
       (save-excursion
         (backward-char)
         (when delete-preceding-spaces
           (skip-chars-backward "\s\t"))
         (point))
       end)
      (json-par-up-forward)
      (json-par-oneline)
      (backward-char)
      (json-par--backward-spaces))

     ;; Before the first member.
     ;; [
     ;;   |1,
     ;;   2,
     ;;   3
     ;; ]
     ;; ↓
     ;; [ |1, 2, 3 ]
     ((json-par--before-first-member-p previous-token)
      (delete-region
       (save-excursion
         (backward-char)
         (when delete-preceding-spaces
           (skip-chars-backward "\s\t"))
         (point))
       end)
      (json-par-up-backward)
      (json-par-oneline)
      (forward-char)
      (json-par--forward-spaces))

     ;; Between members and each member is on its own line.
     ;; [
     ;;   1,
     ;;   |2,
     ;;   3
     ;; ]
     ;; ↓
     ;; [
     ;;   1, |2, 3
     ;; ]
     ((and (json-par-token-comma-p previous-token)
           (memq action-when-joining-non-empty-lines
                 '(delete-line-breaks-between-members
                   delete-line-breaks-inside-brackets))
           (not (json-par--multiple-members-on-same-line-p)))
      (delete-region
       (save-excursion
         (backward-char)
         (when delete-preceding-spaces
           (skip-chars-backward "\s\t"))
         (point))
       end)
      (setq current-member-index (json-par--current-member-index))
      (setq current-offset-from-member
            (- (save-excursion (skip-chars-forward "\s\t") (point)) (point)))
      (json-par-up-backward)
      (save-excursion
        (json-par--oneline-after
         0
         (eq action-when-joining-non-empty-lines
             'delete-line-breaks-between-members)))
      (forward-char)
      (json-par--forward-spaces)
      (json-par-goto-index current-member-index)
      (backward-char current-offset-from-member))

     ;; Otherwise.
     (t
      (delete-region
       (save-excursion
         (backward-char)
         (when delete-preceding-spaces
           (skip-chars-backward "\s\t"))
         (point))
       end)))
    (json-par--fixup-space-after-join-line)))

(defun json-par--join-line-forward (delete-preceding-spaces
                                    delete-following-spaces
                                    action-when-deleting-successive-empty-lines
                                    action-when-joining-non-empty-lines)
  "Join the current line and the next line.

If DELETE-PRECEDING-SPACES is non-nil, delete the preceding spaces.

- If the current line is empty:
  - When ACTION-WHEN-DELETING-SUCCESSIVE-EMPTY-LINES is `delete-all':
    Delete from the point to the beginning of the next non-empty line.

  - When ACTION-WHEN-DELETING-SUCCESSIVE-EMPTY-LINES is `delete-one':
    Delete from the point to the beginning of the next line.

  - When ACTION-WHEN-DELETING-SUCCESSIVE-EMPTY-LINES is `leave-one':
    Delete from the point to the end of the previous line of the next non-empty
    line.  If the next line is not empty, delete to the end of that line.

  Then, if DELETE-FOLLOWING-SPACES is non-nil, delete the spaces
  after the point.

  Example (`|' is the point):

  `delete-all':
    [
       1
       |

    ]
    ↓
    [
       1
    |]

  `delete-one':
    [
       1
       |

    ]
    ↓
    [
       1
       |
    ]

  `leave-one':
    [
       1
       |


    ]
    ↓
    [
       1
       |
    ]
    ↓
    [
       1
    |]

- If the next line is empty:
  - When ACTION-WHEN-DELETING-SUCCESSIVE-EMPTY-LINES is `delete-all':
    Delete from the point to the end of the successive empty lines.

  - When ACTION-WHEN-DELETING-SUCCESSIVE-EMPTY-LINES is `delete-one':
    Delete from the point until the end of the next line.

  - When ACTION-WHEN-DELETING-SUCCESSIVE-EMPTY-LINES is `leave-one':
    Delete from the point to the end of the second to last line of the
    successive empty lines.  If the successive empty lines have only one line,
    delete until the end of that line.

  If DELETE-FOLLOWING-SPACES is nil, keep the spaces in the last line.

  Example (`|' is the point):

  `delete-all':
    [
       1|


    ]
    ↓
    [
       1|
    ]

  `delete-one':
    [
       1|


    ]
    ↓
    [
       1|

    ]

  `leave-one':
    [
       1|



    ]
    ↓
    [
       1|

    ]
    ↓
    [
       1|
    ]


- If the point is before the first member of array/object, or after the last
  member, convert the current array/object into one-line.  the spaces around the
  point is deleted according to DELETE-PRECEDING-SPACES and
  DELETE-FOLLOWING-SPACES.

  Example (`|' is the point):

    [|
      1,
      2,
      3
    ]
    ↓
    [ |1, 2, 3 ]

    [
      1,
      2,
      3|
    ]
    ↓
    [ 1, 2, 3| ]

- If the point is between members and each member is on its own line, action
  depends on ACTION-WHEN-JOINING-NON-EMPTY-LINES:

  - `just-delete': just delete the line break.

    Example (`|' is the point):

    [
      1,|
      2,
      3
    ]
    ↓
    [
      1, |2,
      3
    ]

  - `delete-line-breaks-between-members': delete all line breaks between members
    (but not before/after members).

    Example (`|' is the point):

    [
      1,|
      2,
      3
    ]
    ↓
    [
      1, |2, 3
    ]

  - `delete-line-breaks-inside-brackets': delete all line breaks inside the
    current array/object.

    Example (`|' is the point):

    [
      1,|
      2,
      3
    ]
    ↓
    [ 1, |2, 3 ]

  The spaces around the point is deleted according to DELETE-PRECEDING-SPACES
  and DELETE-PRECEDING-SPACES.

- Otherwise, the lines are just joined.  The spaces around the point is deleted
  according to DELETE-PRECEDING-SPACES and DELETE-FOLLOWING-SPACES.

After the lines are joined, a space is inserted depending on the characters
around the point."
  (let ((previous-token (save-excursion (json-par-backward-token)))
        (next-token (save-excursion (json-par-forward-token)))
        current-member-index
        current-offset-from-member
        (start (if delete-preceding-spaces
                   (save-excursion
                     (skip-chars-backward "\s\t")
                     (point))
                 (point)))
        end
        (string-like-beginning-position
         (json-par--string-like-beginning-position)))
    (end-of-line)
    (cond
     ;; Empty line
     ((save-excursion
        (skip-chars-backward "\s\t")
        (bolp))
      (setq end
            (save-excursion
              (cond
               ((eq action-when-deleting-successive-empty-lines 'delete-one)
                (forward-line))

               ((eq action-when-deleting-successive-empty-lines 'leave-one)
                (if (save-excursion
                      (forward-line)
                      (skip-chars-forward "\s\t")
                      (not (eolp)))
                    ;; Only one line is left.
                    ;; Delete it.
                    (forward-line)
                  ;; Leave one
                  (skip-chars-forward "\s\t\n")
                  (forward-line -1)))

               ((eq action-when-deleting-successive-empty-lines 'delete-all)
                (skip-chars-forward "\s\t\n")
                (skip-chars-backward "\s\t")))
              (when delete-following-spaces
                (skip-chars-forward "\s\t"))
              (point)))
      (delete-region start end))

     ;; The next line is empty.
     ((save-excursion
        (forward-line 1)
        (skip-chars-forward "\s\t")
        (eolp))
      (setq end
            (save-excursion
              (cond
               ((eq action-when-deleting-successive-empty-lines 'delete-one)
                (forward-line))

               ((eq action-when-deleting-successive-empty-lines 'leave-one)
                (if (save-excursion
                      (forward-line)
                      (skip-chars-forward "\s\t")
                      (or (eobp)
                          (progn
                            (forward-line)
                            (skip-chars-forward "\s\t")
                            (not (eolp)))))
                    ;; Only one line is left.
                    ;; Delete it.
                    (forward-line)
                  ;; Leave one
                  (skip-chars-forward "\s\t\n")
                  (forward-line -1)
                  (backward-char)
                  (skip-chars-backward "\s\t")))

               ((eq action-when-deleting-successive-empty-lines 'delete-all)
                (skip-chars-forward "\s\t\n")
                (unless (eobp)
                  (forward-line 0)
                  (backward-char))
                (skip-chars-backward "\s\t")))
              (when delete-following-spaces
                (skip-chars-forward "\s\t"))
              (point)))
      (delete-region start end))

     ;; The current line and the next line belong to a multiline string or
     ;; a multiline comment.
     ((save-excursion
        (forward-line)
        (json-par--string-like-beginning-position))
      (delete-region
       start
       (save-excursion
         (forward-line)
         (when delete-following-spaces
           (skip-chars-forward "\s\t"))
         (point))))

     ;; The current line ends with a single-line comment and the next line
     ;; starts with a single-line comment.
     ((and string-like-beginning-position
           (save-excursion
             (forward-line)
             (looking-at "[\s\t]*//")))
      (setq end
            (save-excursion
              (forward-line)
              (when (and (memq action-when-deleting-successive-empty-lines
                               '(leave-one delete-all))
                         (looking-at "[\s\t]*//+[\s\t]*$"))
                (while (and (looking-at "[\s\t]*//+[\s\t]*$")
                            (zerop (forward-line)))
                  t)
                (if (eobp)
                    (forward-line 0)
                  (forward-line -1)
                  (when (eq action-when-deleting-successive-empty-lines
                            'leave-one)
                    (forward-line -1)
                    (when (json-par--same-line-p start (point))
                      (forward-line)))))
              (skip-chars-forward "\s\t")
              (skip-chars-forward "/")
              (when delete-following-spaces
                (skip-chars-forward "\s\t"))
              (point)))
      (delete-region start end))

     ;; The current line ends with a single-line comment.
     (string-like-beginning-position
      (delete-region start (line-end-position))
      (let ((have-space-before-comment
             (memq (char-before string-like-beginning-position)
                   '(?\s ?\t ?\n nil)))
            (comment-string
             (delete-and-extract-region
              string-like-beginning-position
              (line-end-position))))
        (json-par--join-line-forward
         delete-preceding-spaces
         delete-following-spaces
         action-when-deleting-successive-empty-lines
         action-when-joining-non-empty-lines)
        (end-of-line)
        (when (json-par--string-like-beginning-position)
          (goto-char (json-par--string-like-beginning-position)))
        (when (and have-space-before-comment
                   (not (memq (char-before) '(?\s ?\t ?\n nil))))
          (insert-char ?\s))
        (insert comment-string)
        (delete-region
         (point)
         (progn
           (skip-chars-forward "/")
           (when delete-following-spaces
             (skip-chars-forward "\s\t"))
           (point)))))

     ;; Before the first member.
     ;;
     ;; [|
     ;;   1,
     ;;   2,
     ;;   3
     ;; ]
     ;; ↓
     ;; [ |1, 2, 3 ]
     ((json-par--before-first-member-p previous-token)
      (delete-region
       start
       (save-excursion
         (forward-line)
         (when delete-following-spaces
           (skip-chars-forward "\s\t"))
         (point)))
      (json-par-up-backward)
      (json-par-oneline)
      (forward-char)
      (json-par--forward-spaces))

     ;; After the last member.
     ;;
     ;; [
     ;;   1,
     ;;   2,
     ;;   3|
     ;; ]
     ;; ↓
     ;; [ 1, 2, 3| ]
     ((json-par--after-last-member-p next-token)
      (delete-region
       start
       (save-excursion
         (forward-line)
         (when delete-following-spaces
           (skip-chars-forward "\s\t"))
         (point)))
      (json-par-up-forward)
      (json-par-oneline)
      (backward-char)
      (json-par--backward-spaces))

     ;; Between members and each member is on its own line.
     ;;
     ;; [
     ;;   1,
     ;;   2,|
     ;;   3
     ;; ]
     ;; ↓
     ;; [
     ;;   1, 2, |3
     ;; ]
     ((and (json-par-token-comma-p previous-token)
           (memq action-when-joining-non-empty-lines
                 '(delete-line-breaks-between-members
                   delete-line-breaks-inside-brackets))
           (not (json-par--multiple-members-on-same-line-p)))
      (delete-region
       start
       (save-excursion
         (forward-line)
         (when delete-following-spaces
           (skip-chars-forward "\s\t"))
         (point)))
      (setq current-member-index (json-par--current-member-index))
      (setq current-offset-from-member
            (- (save-excursion (skip-chars-forward "\s\t") (point)) (point)))
      (json-par-up-backward)
      (save-excursion
        (json-par--oneline-after
         0
         (eq action-when-joining-non-empty-lines
             'delete-line-breaks-between-members)))
      (forward-char)
      (json-par--forward-spaces)
      (json-par-goto-index current-member-index)
      (backward-char current-offset-from-member))

     ;; Otherwise.
     (t
      (delete-region
       start
       (save-excursion
         (forward-line)
         (when delete-following-spaces
           (skip-chars-forward "\s\t"))
         (point)))))
    (json-par--fixup-space-after-join-line)))

(defun json-par--fixup-space-after-join-line ()
  "Insert a space if needed."
  (if (json-par--string-like-beginning-position)
      (when (and (not (memq (char-before) '(?\s ?\t ?\n nil)))
                 (not (memq (char-after) '(?\s ?\t ?\n nil))))
        (fixup-whitespace))
    (when (and (not (memq (char-before) '(?\s ?\t ?\n nil)))
               (not (memq (char-after) '(?\s ?\t ?\n ?: ?, nil))))
      (insert-char ?\s))))


;;; Keymap

(defvar json-par-delete-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" #'json-par-delete-current-member)
    (define-key map "." #'json-par-delete-current-value-or-key)
    (define-key map "j" #'json-par-delete-forward-member)
    (define-key map "k" #'json-par-delete-backward-member)
    (define-key map "v" #'json-par-delete-object-value)
    (define-key map "h" #'json-par-delete-forward-parent)
    (define-key map "l" #'json-par-delete-backward-parent)
    (define-key map "i" #'json-par-delete-inner)
    (define-key map "a" #'json-par-delete-head-of-member)
    (define-key map "e" #'json-par-delete-object-value)
    (define-key map "A" #'json-par-delete-forward-inside-of-parent)
    (define-key map "E" #'json-par-delete-backward-inside-of-parent)
    (dolist (prefix '("" "C-" "M-" "C-M-"))
      (dolist (key '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
        (define-key map (kbd (concat prefix key))
          #'json-par-prefix-digit-argument))
      (define-key map (kbd (concat prefix "-"))
        #'json-par-prefix-negative-argument))
    (define-key map "\C-u" #'json-par-prefix-universal-argument)
    (define-key map [t] #'json-par-prefix-default)
    map)
  "Temporary keymap for `json-par-delete-prefix-command'.")


;;; Handling key sequence for deleting members

;; Commands for deleting members can be invoked with key sequence.
;;
;; Examples:
;;
;; - Key sequence "d d": delete the current member.
;; - Key sequence "d j": delete the following member.
;;
;; This is implemented with temporary keymap installed by `set-transient-map'
;; via `json-par-prefix-command'.
;;
;; Why not `read-multiple-choice':
;;
;; - I don't like the style of the help message.
;; - The keymap should be customizable.

(defun json-par-delete-prefix-command (arg)
  "Delete or mark various things.

Actually, set a temporary keymap `json-par-delete-prefix-map' to read a target.

If region is active, delete it without reading a target.

ARG is passed to the commands bound in the keymap."
  (interactive "P")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (json-par-prefix-command
     arg
     (when (called-interactively-p 'interactive)
       (if (eq json-par-action-when-deleting-value-or-member 'delete)
           "delete (? for help)%s: "
         "mark (? for help)%s: "))
     (concat
      (if (eq json-par-action-when-deleting-value-or-member 'delete)
          "Delete"
        "Mark")
      " various things.

Action depends on `json-par-action-when-deleting-value-or-member'
(currently, '"
      (prin1-to-string json-par-action-when-deleting-value-or-member)
      ").

Actually, set the following temporary keymap to read target.\n\n"
      (json-par--format-command-help json-par-delete-prefix-map))
     json-par-delete-prefix-map)))


(provide 'json-par-delete)

;;; json-par-delete.el ends here
