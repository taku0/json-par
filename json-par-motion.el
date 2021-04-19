;;; json-par-motion.el --- Moving the point in JSON Par mode -*- lexical-binding: t -*-

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

;; Functions for moving the point in JSON Par mode.

;;; Code:

(require 'json-par-utils)
(require 'json-par-lexer)

(declare-function json-par-delete-head-of-member
                  "json-par-delete"
                  (&optional action))

(declare-function json-par-oneline
                  "json-par-oneline-multiline"
                  (&optional min-level))

;;; Customizations

(defcustom json-par-collapse-when-exit-from-empty-brackets t
  "If non-nil, collapse an empty array/object when exiting from it.

This affects `json-par-up-backward' and `json-par-up-forward'."
  :type 'boolean
  :group 'json-par
  :safe 'booleanp)


;;; forward-sexp-function

(defun json-par-forward-sexp (&optional arg)
  "Move forward a token or a list.

Inside a string or a comment, forward a word.

See `forward-sexp' for ARG.

Signal `scan-error' if it hits a unmatched parenthesis."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (if (< 0 arg)
      (dotimes (_ arg)
        (json-par--forward-sexp-1))
    (dotimes (_ (- arg))
      (json-par--backward-sexp-1))))

(defun json-par--forward-sexp-1 ()
  "Move forward a token or a list.

Inside a string or a comment, forward a word.

Signal `scan-error' if it hits a close parenthesis."
  (let ((string-like-beginning-position
         (json-par--string-like-beginning-position))
        string-like-end-position
        (pos (point))
        token)
    (if string-like-beginning-position
        (progn
          (setq string-like-end-position
                (save-excursion
                  (goto-char string-like-beginning-position)
                  (json-par-token-end
                   (json-par-forward-token-or-list-or-comment))))
          (forward-word)
          (when (< string-like-end-position (point))
            (goto-char string-like-end-position)
            (when (bolp)
              (backward-char)
              (when (<= (point) pos)
                (skip-chars-forward "\s\t\n")
                (json-par--forward-sexp-1)))))
      (setq token (json-par-forward-token-or-list-or-comment))
      (when (bolp)
        (backward-char)
        (when (<= (point) pos)
          (skip-chars-forward "\s\t\n")
          (json-par--forward-sexp-1)))
      (when (json-par-token-close-bracket-p token)
        (goto-char pos)
        (signal 'scan-error
                (list "Unbalanced parentheses"
                      (json-par-token-start token)
                      (json-par-token-end token)))))))

(defun json-par--backward-sexp-1 ()
  "Move backward a token or list.

Inside a string or a comment, forward a word.

Signal `scan-error' if it hits a open parenthesis."
  (let ((string-like-beginning-position
         (json-par--string-like-beginning-position))
        (pos (point))
        token)
    (if string-like-beginning-position
        (progn
          (backward-word)
          (when (< (point) string-like-beginning-position)
            (goto-char string-like-beginning-position)))
      (setq token (json-par-backward-token-or-list-or-comment))
      (when (json-par-token-open-bracket-p token)
        (goto-char pos)
        (signal 'scan-error
                (list "Unbalanced parentheses"
                      (json-par-token-start token)
                      (json-par-token-end token)))))))


;;; Object/array members

(defun json-par--parse-member-forward ()
  "Parse the current member.

Assuming the point is at the beginning of the member.

Return a hash table with the following members:

- :start-of-member, the start position of the member
- :end-of-member, the end position of the member
- :key-token, the key token of a key-value pair, if any
- :colon-token, the colon token of a key-value pair, if any
- :value-token, the value token of member, if any"
  (save-excursion
    (json-par--forward-spaces)
    (let ((done nil)
          (result (make-hash-table :size 5))
          (start-of-member (point))
          end-of-member
          key-token
          colon-token
          value-token
          token
          (json-par--already-out-of-comment t)
          (json-par--already-out-of-atom t))
      (while (progn
               (json-par--forward-spaces)
               (and (not (memq (char-after) '(nil ?\, ?\] ?\) ?})))
                    (not done)))
        (setq token (json-par-forward-token-or-list))
        (cond
         ;; Colon
         ((json-par-token-colon-p token)
          (if (or colon-token value-token)
              (progn
                (goto-char (json-par-token-start token))
                (setq done t))
            (setq colon-token token)))

         ;; Object key
         ((json-par--object-key-p token)
          (if (or key-token colon-token value-token)
              (progn
                (goto-char (json-par-token-start token))
                (setq done t))
            (setq key-token token)))

         ;; Value
         ((or (json-par-token-atom-p token)
              (json-par-token-matching-brackets-p token))
          (if value-token
              (progn
                (goto-char (json-par-token-start token))
                (setq done t))
            (setq value-token token)))))
      (json-par--backward-spaces)
      (setq end-of-member (point))
      (puthash :start-of-member start-of-member result)
      (puthash :end-of-member end-of-member result)
      (puthash :key-token key-token result)
      (puthash :colon-token colon-token result)
      (puthash :value-token value-token result)
      result)))

(defun json-par--parse-member-backward ()
  "Parse the current member.

Assuming the point is at the end of the member.

Return a hash table with the following members:

- :start-of-member, the start position of the member
- :end-of-member, the end position of the member
- :key-token, the key token of a key-value pair, if any
- :colon-token, the colon token of a key-value pair, if any
- :value-token, the value token of member, if any"
  (save-excursion
    (json-par--backward-spaces)
    (let ((done nil)
          (result (make-hash-table :size 5))
          start-of-member
          (end-of-member (point))
          key-token
          colon-token
          value-token
          token
          (json-par--already-out-of-comment t)
          (json-par--already-out-of-atom t))
      (while (progn
               (json-par--backward-spaces)
               (and (not (memq (char-before) '(nil ?\, ?\[ ?\( ?{)))
                    (not done)))
        (setq token (json-par-backward-token-or-list))
        (cond
         ;; Colon
         ((json-par-token-colon-p token)
          (if (or colon-token key-token)
              (progn
                (goto-char (json-par-token-end token))
                (setq done t))
            (setq colon-token token)))

         ;; Object key
         ((json-par--object-key-p token)
          (if key-token
              (progn
                (goto-char (json-par-token-end token))
                (setq done t))
            (setq key-token token)))

         ;; Value
         ((or (json-par-token-atom-p token)
              (json-par-token-matching-brackets-p token))
          (if (or value-token colon-token key-token)
              (progn
                (goto-char (json-par-token-end token))
                (setq done t))
            (setq value-token token)))))
      (json-par--forward-spaces)
      (setq start-of-member (point))
      (puthash :start-of-member start-of-member result)
      (puthash :end-of-member end-of-member result)
      (puthash :key-token key-token result)
      (puthash :colon-token colon-token result)
      (puthash :value-token value-token result)
      result)))

(defun json-par-end-of-member (&optional push-mark)
  "Move the point to the end of the current member, not including a comma.

If PUSH-MARK is non-nil or called interactively, the function is not called
repeatedly, and the region is not active, push a mark first."
  (interactive
   (list
    (not (eq last-command 'json-par-end-of-member))))
  (when (and push-mark (not (region-active-p)))
    (push-mark))
  (json-par--out-comment)
  (json-par--out-atom)
  (goto-char (gethash :end-of-member (json-par--parse-member-forward))))

(defun json-par-beginning-of-member (&optional push-mark)
  "Move the point to the start of the current member.

If PUSH-MARK is non-nil or called interactively, the function is not called
repeatedly, and the region is not active, push a mark first."
  (interactive
   (list
    (not (eq last-command 'json-par-beginning-of-member))))
  (when (and push-mark (not (region-active-p)))
    (push-mark))
  (json-par--out-comment)
  (json-par--out-atom)
  (goto-char (gethash :start-of-member (json-par--parse-member-backward))))

(defun json-par-beginning-of-object-value (&optional push-mark parsed)
  "Move the point to the start of the object value of the current member.

If the point is not in a object, go to the beginning of the member.

If PUSH-MARK is non-nil or called interactively, the function is not called
repeatedly, and the region is not active, push a mark first.

If PARSED is given, it is used instead of calling
`json-par--parse-member-forward'."
  (interactive
   (list
    (not (eq last-command 'json-par-beginning-of-object-value))))
  (when (and push-mark (not (region-active-p)))
    (push-mark))
  (unless parsed
    (json-par-beginning-of-member)
    (setq parsed (json-par--parse-member-forward)))
  (cond
   ((gethash :value-token parsed)
    (goto-char (json-par-token-start (gethash :value-token parsed))))

   ((gethash :colon-token parsed)
    (goto-char (json-par-token-end (gethash :colon-token parsed)))
    (json-par--forward-spaces)
    (when (memq (char-after) '(?\] ?\) ?}))
      (goto-char (json-par-token-end (gethash :colon-token parsed)))
      (skip-chars-forward "\s\t")))

   ((gethash :key-token parsed)
    (goto-char (json-par-token-end (gethash :key-token parsed)))
    (json-par--forward-spaces)
    (when (memq (char-after) '(?\] ?\) ?}))
      (goto-char (json-par-token-end (gethash :key-token parsed)))
      (skip-chars-forward "\s\t")))

   (t
    (goto-char (gethash :end-of-member parsed)))))

(defun json-par-beginning-of-list (&optional push-mark)
  "Move the point before the first member of the current array/object.

If PUSH-MARK is non-nil or called interactively, the function is not called
repeatedly, and the region is not active, push a mark first."
  (interactive
   (list
    (not (eq last-command 'json-par-beginning-of-list))))
  (json-par-up-backward 1 push-mark)
  (json-par-down))

(defun json-par-end-of-list (&optional push-mark)
  "Move the point after last the member of the current array/object.

If PUSH-MARK is non-nil or called interactively, the function is not called
repeatedly, and the region is not active, push a mark first."
  (interactive
   (list
    (not (eq last-command 'json-par-end-of-list))))
  (json-par-up-forward 1 push-mark)
  (json-par-down))

(defun json-par--find-member (p)
  "Find a member satisfying a predicate P.

Move the point to the start of the first member, call P, and if it return nil,
move to the next member and call P until P returns non-nil.

If P returns non-nil, keep the point at the beginning of the member and return
the value returned from P.

Otherwise, move the point to the original position and return nil.

P is called with the index of the member, starting from zero."
  (let ((point-marker (point-marker))
        (parent-token (json-par--parent-token))
        (found nil))
    (goto-char (json-par-token-end parent-token))
    (json-par--forward-spaces)
    (setq found (json-par--find-member-forward p))
    (unless found
      (goto-char point-marker))
    (json-par--free-marker point-marker)
    found))

(defun json-par--find-member-forward (p &optional include-cousin)
  "Find a member satisfying a predicate P after the point.

Move the point to the start of the current member, call P, and if it return nil,
move to the next member and call P until P returns non-nil or all members are
inspected.

If P returns non-nil for some member, keep the point at the beginning of the
member and return the value returned from P.

Otherwise, move the point to the original position and return nil.

P is called with the index of the member relative the starting member, starting
from zero.

If INCLUDE-COUSIN is non-nil, also search cousin members."
  (json-par--find-member-from-here
   p
   (if include-cousin
       #'json-par--goto-beginning-of-next-member-or-cousin
     #'json-par--goto-beginning-of-next-member)))

(defun json-par--find-member-backward (p &optional include-cousin)
  "Find a member satisfying a predicate P before the point.

Move the point to the start of the current member, call P, and if it return nil,
move to the previous member and call P until P returns non-nil or all members
are inspected.

If P returns non-nil for some member, keep the point at the beginning of the
member and return the value returned from P.

Otherwise, move the point to the original position and return nil.

P is called with the index of the member relative the starting member, starting
from zero and increasing.

If INCLUDE-COUSIN is non-nil, also search cousin members."
  (json-par--find-member-from-here
   p
   (if include-cousin
       (lambda ()
         (prog1 (json-par--goto-end-of-previous-member-or-cousin)
           (json-par-beginning-of-member)))
     (lambda ()
       (prog1 (json-par--goto-end-of-previous-member)
         (json-par-beginning-of-member))))))

(defun json-par--goto-beginning-of-next-member-or-cousin ()
  "Move the point to the beginning of the next member.

If the point is on the last member, move to the first member of the
sibling/cousin array/object.

If a member is found, return t.  Return nil otherwise."
  (or (json-par--goto-beginning-of-next-member)
      (let ((depth 1)
            (token (json-par-forward-token)))
        (while (and
                (not (zerop depth))
                (not (json-par-token-outside-of-buffer-p token)))
          (setq token (json-par-forward-token))
          (cond
           ((json-par-token-close-bracket-p token)
            (setq depth (1+ depth)))
           ((json-par-token-open-bracket-p token)
            (setq depth (1- depth))))
          (json-par--forward-spaces)
          (when (memq (char-after) '(?\] ?\) ?}))
            (setq token (json-par-forward-token))
            (setq depth (1+ depth))))
        (zerop depth))))

(defun json-par--goto-end-of-previous-member-or-cousin ()
  "Move the point to the end of the previous member.

If the point is on the first member, move to the last member of the
sibling/cousin array/object.

If a member is found, return t.  Return nil otherwise."
  (or (json-par--goto-end-of-previous-member)
      (let ((depth 1)
            (token (json-par-backward-token)))
        (while (and
                (not (zerop depth))
                (not (json-par-token-outside-of-buffer-p token)))
          (setq token (json-par-backward-token))
          (cond
           ((json-par-token-open-bracket-p token)
            (setq depth (1+ depth)))
           ((json-par-token-close-bracket-p token)
            (setq depth (1- depth))))
          (json-par--backward-spaces)
          (when (memq (char-before) '(?\[ ?\( ?{))
            (setq token (json-par-backward-token))
            (setq depth (1+ depth))))
        (zerop depth))))

(defun json-par--find-member-from-here (p move-next)
  "Find a member satisfying a predicate P from the point.

Move the point to the start of the current member, call P, and if it return nil,
call MOVE-NEXT, and call P until P returns non-nil.

If P returns non-nil, keep the point at the beginning of the member and return
the value returned from P.

Otherwise, move the point to the original position and return nil.

If MOVE-NEXT return nil, it is considered as the end of the list.

P is called with the index of the member relative the starting member, starting
from zero and increasing."
  (json-par-beginning-of-member)
  (let ((point-marker (point-marker))
        (done nil)
        (found nil)
        (i 0)
        old-position
        (json-par--already-out-of-comment t)
        (json-par--already-out-of-atom t))
    (while (not done)
      (setq old-position (point-marker))
      (setq found (funcall p i))
      (setq done found)
      (setq old-position (json-par--free-marker old-position))
      (unless done
        (goto-char old-position)
        (if (funcall move-next)
            (setq i (1+ i))
          (setq done t))))
    (unless found
      (goto-char point-marker))
    (json-par--free-marker point-marker)
    found))

(defun json-par--position-in-member ()
  "Return the position in a member.

Return one of:

- empty-member: the member is empty
- before-member: before a member
- after-member: after a member (before comma)
- before-value: between a colon and an object value
- after-key: between an object key and a colon"
  (save-excursion
    (json-par--out-comment)
    (json-par--out-atom)
    (let* ((beginning-position
            (save-excursion
              (json-par-beginning-of-member)
              (point)))
           (end-position
            (save-excursion
              (goto-char beginning-position)
              (json-par-end-of-member)
              (point)))
           (point-before-spaces
            (save-excursion
              (json-par--backward-spaces)
              (point))))
      (cond
       ((= (save-excursion
             (goto-char beginning-position)
             (json-par--backward-spaces)
             (point))
           (save-excursion
             (goto-char end-position)
             (json-par--backward-spaces)
             (point)))
        'empty-member)

       ((= point-before-spaces
           (save-excursion
             (goto-char beginning-position)
             (json-par--backward-spaces)
             (point)))
        'before-member)

       ((= point-before-spaces
           (save-excursion
             (goto-char end-position)
             (json-par--backward-spaces)
             (point)))
        'after-member)

       ((= point-before-spaces
           (save-excursion
             (json-par-beginning-of-object-value)
             (json-par--backward-spaces)
             (point)))
        'before-value)

       (t 'after-key)))))

(defun json-par--goto-position-in-member (position-in-member)
  "Go to POSITION-IN-MEMBER in the current member.

POSITION-IN-MEMBER is a symbol returned from `json-par--position-in-member'."
  (cond
   ((eq position-in-member 'empty-member)
    nil)

   ((eq position-in-member 'before-member)
    (json-par-beginning-of-member))

   ((eq position-in-member 'after-member)
    (json-par-end-of-member))

   ((eq position-in-member 'before-value)
    (json-par-beginning-of-object-value))

   ((eq position-in-member 'after-key)
    (json-par-beginning-of-object-value)
    (let ((previous-token (save-excursion (json-par-backward-token))))
      (when (json-par-token-colon-p previous-token)
        (goto-char (json-par-token-start previous-token))))
    (json-par--backward-spaces))))

(defun json-par--goto-position-in-parsed-member (position-in-member parsed)
  "Go to POSITION-IN-MEMBER in the PARSED member.

POSITION-IN-MEMBER is a symbol returned from `json-par--position-in-member'.

PARSED is a parsed member returned from `json-par--parse-member-forward' or
`json-par--parse-member-backward'."
  (cond
   ((eq position-in-member 'empty-member)
    nil)

   ((eq position-in-member 'before-member)
    (goto-char (gethash :start-of-member parsed)))

   ((eq position-in-member 'after-member)
    (goto-char (gethash :end-of-member parsed)))

   ((eq position-in-member 'before-value)
    (json-par-beginning-of-object-value nil parsed))

   ((eq position-in-member 'after-key)
    (json-par-beginning-of-object-value nil parsed)
    (let ((previous-token (save-excursion (json-par-backward-token))))
      (when (json-par-token-colon-p previous-token)
        (goto-char (json-par-token-start previous-token))))
    (json-par--backward-spaces))))

(defun json-par-goto-key (key &optional push-mark)
  "Move the point to the beginning of the member with KEY.

If PUSH-MARK is non-nil and the region is not active, push a mark first.

Return non-nil if KEY found.  Otherwise, keep the original position and return
nil."
  (interactive "MGoto key: \np")
  (when (and push-mark (not (region-active-p)))
    (push-mark))
  (let ((pos (point))
        found)
    (json-par--out-comment)
    (json-par--out-atom)
    (setq found (json-par--find-member
                 (lambda (_)
                   (let ((key-token (save-excursion (json-par-forward-token))))
                     (and (json-par-token-string-p key-token)
                          (equal (json-par--read-token key-token) key))))))
    (unless found
      (goto-char pos)
      (when (called-interactively-p 'interactive)
        (warn "key not found")))
    found))

(defun json-par-goto-index (index &optional push-mark)
  "Move the point to the beginning of the member at INDEX.

If PUSH-MARK is non-nil and the region is not active, push a mark first.

Return non-nil if INDEX found.  Otherwise, keep the original position and return
nil."
  (interactive "nGoto index: \np")
  (when (and push-mark (not (region-active-p)))
    (push-mark))
  (let ((pos (point))
        found)
    (json-par--out-comment)
    (json-par--out-atom)
    (setq found (json-par--find-member (lambda (i) (= i index))))
    (unless found
      (goto-char pos)
      (when (called-interactively-p 'interactive)
        (warn "index out of bound")))
    found))

(defun json-par-goto-path (path &optional push-mark)
  "Move the point to the beginning of the member at PATH.

PATH is a list of following elements:

- String: key of member in object
- Number: index of member in object/array
- Symbol `*': first member matching rest of the path

Example:

  When the point is at (*1) below, (json-par-goto-path '(\"a\" * \"b\" 1))
  move the point to (*2).

  {
    (*1) \"a\": [
      { \"a\": 1 },
      { \"b\": [ 2, (*2) 3, 4] },
      { \"c\": 3 }
    ]
  }

If PUSH-MARK is non-nil and the region is not active, push a mark first.

Return non-nil if PATH found.  Otherwise, keep the original position and return
nil."
  (when (and push-mark (not (region-active-p)))
    (push-mark))
  (let ((pos (point))
        (found t)
        step)
    (while (and path found)
      (setq step (car path))
      (setq path (cdr path))
      (cond
       ;; Key
       ((stringp step)
        (setq found (json-par-goto-key step)))

       ;; Index
       ((numberp step)
        (setq found (json-par-goto-index step)))

       ;; Any
       ((eq step '*)
        (setq found (json-par--find-member
                     (lambda (_)
                       (json-par-beginning-of-object-value)
                       (if (memq (char-after) '(?\[ ?{))
                           (progn
                             (forward-char)
                             (json-par-goto-path path))
                         nil))))
        (setq path nil))

       ;; Invalid step
       (t
        (error (concat "invalid step" (prin1-to-string step)))))
      (when path
        (json-par-beginning-of-object-value)
        (if (memq (char-after) '(?\[ ?{))
            (forward-char)
          (setq found nil))))
    (unless found
      (goto-char pos)
      (when (called-interactively-p 'interactive)
        (warn "not found")))
    found))

(defun json-par--multiple-members-on-same-line-around-point-p (count)
  "Return non-nil if lines around the point has multiple members.

Otherwise, return nil.

Check COUNT members around the point.

Examples:

  // Has multiple members on a line:
  [
    1, 2, 3,|
    4, 5, 6
  ]

  // Has single member for each lines:
  [
    1,
    2,
    3,|
    4,
    5,
    6
  ]

This function affects whether a line break is inserted or not when inserting a
comma or a value."
  (let ((positions (list)))
    (save-excursion
      (json-par-beginning-of-member)
      (push (point) positions)
      (dotimes (_ count)
        (json-par-backward-member)
        (when (eq (char-after) ?\,)
          (forward-char))
        (json-par-beginning-of-member)
        (push (point) positions)))
    (save-excursion
      (json-par-beginning-of-member)
      (dotimes (_ count)
        (json-par-forward-member)
        (when (eq (char-before) ?\,)
          (backward-char))
        (json-par-beginning-of-member)
        (push (point) positions)))
    (not (cl-every #'json-par--beginning-of-line-or-list-p positions))))

(defun json-par--multiple-members-on-same-line-p ()
  "Return non-nil if containing object/array has multiple members on a line.

Return nil otherwise.

Examples:

  // Has multiple members on a line:
  [
    1, 2, 3,
    4, 5, 6
  ]

  // Has single member for each lines:
  [
    1,
    2,
    3,
    4,
    5,
    6
  ]

This function affects `json-par--join-line-backward' and
`json-par--join-line-forward'."
  (let ((result nil))
    (save-excursion
      (json-par-up-backward)
      (forward-char)
      (while (and (not result)
                  (progn
                    (json-par--forward-spaces)
                    (not (memq (char-after) '(nil ?\] ?\) ?})))))
        (unless (json-par--beginning-of-line-or-list-p)
          (setq result t))
        (json-par-forward-member)))
    result))

(defun json-par--all-members-on-same-line-after-point-p ()
  "Return non-nil if members after the point are on a line.

Examples (`|' is the point):

  // Return non-nil
  [
    1,
    |2, 3, 4, 5, 6
  ]

  // Return nil
  [
    1,
    |2, 3,
    4, 5, 6
  ]

This function affects `json-par--post-newline'."
  (let ((result t))
    (save-excursion
      (json-par-forward-member)
      (while (and result
                  (progn
                    (json-par--forward-spaces)
                    (not (memq (char-after) '(nil ?\] ?\) ?})))))
        (when (json-par--beginning-of-line-or-list-p)
          (setq result nil))
        (json-par-forward-member)))
    result))

(defun json-par--beginning-of-line-or-list-p (&optional pos)
  "Return non-nil if POS is the beginning of a line or a list except spaces.

Return nil otherwise."
  (unless pos
    (setq pos (point)))
  (save-excursion
    (goto-char pos)
    (json-par--backward-spaces t)
    (or (bolp)
        (memq (char-before) '(?\[ ?\( ?{)))))

(defun json-par--end-of-line-or-list-p (&optional pos)
  "Return non-nil if POS is the end of a line or a list except spaces.

Return nil otherwise."
  (unless pos
    (setq pos (point)))
  (save-excursion
    (goto-char pos)
    (json-par--forward-spaces t)
    (or (eolp)
        (memq (char-after) '(?\] ?\) ?}))
        (looking-at "//"))))

(defun json-par--current-member-index ()
  "Return the position of the current member in the object/array.

The index starts from zero."
  (let ((index 0))
    (save-excursion
      (while (progn
               (json-par-beginning-of-member)
               (json-par--backward-spaces)
               (not (memq (char-before) '(nil ?\[ ?\( ?{))))
        (json-par-backward-member)
        (setq index (1+ index))))
    index))

(defun json-par--before-first-member-p (&optional previous-token)
  "Return non-nil if the point is before the first member of an object/array.

If PREVIOUS-TOKEN is non-nil, assume it is before the point."
  (unless previous-token
    (setq previous-token (save-excursion (json-par-backward-token))))
  (json-par-token-open-bracket-p previous-token))

(defun json-par--before-second-member-p (&optional previous-token)
  "Return non-nil if the point is before the second member of an object/array.

If PREVIOUS-TOKEN is non-nil, assume it is before the point."
  (unless previous-token
    (setq previous-token (save-excursion (json-par-backward-token))))
  (and (json-par-token-comma-p previous-token)
       (save-excursion
         (json-par-backward-member)
         (json-par--backward-spaces)
         (memq (char-before) '(?\[ ?\( ?{)))))

(defun json-par--after-last-member-p (&optional next-token)
  "Return non-nil if the point is after the last member of an object/array.

If NEXT-TOKEN is non-nil, assume it is after the point."
  (unless next-token
    (setq next-token (save-excursion (json-par-forward-token))))
  (json-par-token-close-bracket-p next-token))

;;; Basic movements

(defun json-par-forward-member (&optional arg)
  "Move the point forward to the next member.

With ARG, repeat that times.  If ARG is negative, move backward.

Keep position in a member after movement.

This function treats a key-value pair in an object as one member while
`json-par-forward-sexp' treats it as multiple tokens (key, colon, value, and
optional comma)."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (if (< arg 0)
      (json-par-backward-member (- arg))
    (dotimes (_ arg)
      (json-par--forward-member-1))))

(defun json-par--forward-member-1 ()
  "Move the point forward to the next member."
  (json-par--out-comment)
  (let* ((pos (point))
         ;; If `skip-following-spaces' is non-nil, skip forward following
         ;; spaces after moving.  If it is nil, skip backward.
         skip-following-spaces
         next-token
         previous-token
         just-after-previous-token
         just-before-next-token
         next-is-same-line
         position-in-member
         parsed-next-member)
    (json-par--out-atom)
    (setq next-token (save-excursion (json-par-forward-token)))
    (if (or (json-par-token-close-bracket-p next-token)
            (json-par-token-outside-of-buffer-p next-token))
        (goto-char pos)
      (setq position-in-member
            (json-par--position-in-member))
      (when (eq position-in-member 'empty-member)
        (setq position-in-member 'before-member))
      (setq previous-token (save-excursion (json-par-backward-token)))
      (setq just-after-previous-token (eq (json-par-token-end previous-token)
                                          (point)))
      (setq just-before-next-token (eq (json-par-token-start next-token)
                                       (point)))
      (setq next-is-same-line (json-par--same-line-p
                               (point)
                               (json-par-token-start next-token)))
      (setq skip-following-spaces
            (cond
             ;; After open bracket
             ;;
             ;; [|
             ;;   1,
             ;;   2
             ;; ]
             ;; ↓
             ;; [
             ;;   1,|
             ;;   2
             ;; ]
             ((json-par-token-open-bracket-p previous-token)
              just-before-next-token)

             ;; After ":"
             ;;
             ;; {
             ;;   "a":|   "aaa",
             ;;   "b":   "bbb"
             ;; }
             ;; ↓
             ;; {
             ;;   "a":   "aaa",
             ;;   "b":|   "bbb"
             ;; }
             ;;
             ;; {
             ;;   "a": |  "aaa",
             ;;   "b":   "bbb"
             ;; }
             ;; ↓
             ;; {
             ;;   "a":   "aaa",
             ;;   "b":   |"bbb"
             ;; }
             ((json-par-token-colon-p previous-token)
              (not just-after-previous-token))

             ;; Before ":"
             ((json-par-token-colon-p next-token)
              (not just-after-previous-token))

             ;; Before comma
             ((json-par-token-comma-p next-token)
              (not just-after-previous-token))

             ;; Just before next token (key or value)
             ;;
             ;; [
             ;;   |1,
             ;;   2
             ;; ]
             ;; ↓
             ;; [
             ;;   1,
             ;;   |2
             ;; ]
             (just-before-next-token
              t)

             ;; Just after previous token
             ;;
             ;; [
             ;;   1,|
             ;;   2
             ;; ]
             ;; ↓
             ;; [
             ;;   1,
             ;;   2|
             ;; ]
             (just-after-previous-token
              nil)

             ;; On same line to the next token
             ;;
             ;; [
             ;; |  1,
             ;;   2
             ;; ]
             ;; ↓
             ;; [
             ;;   1,
             ;;   |2
             ;; ]
             (next-is-same-line
              t)

             ;; Otherwise
             ;;
             ;; [
             ;;   1,  |
             ;;   2
             ;; ]
             ;; ↓
             ;; [
             ;;   1,
             ;;   2|
             ;; ]
             ;;
             ;; [
             ;;   1,
             ;;   |
             ;;   2,
             ;;   3
             ;; ]
             ;; ↓
             ;; [
             ;;   1,
             ;;
             ;;   2,|
             ;;   3
             ;; ]
             (t
              nil)))
      (json-par--goto-beginning-of-next-member)
      (setq parsed-next-member (json-par--parse-member-forward))
      (json-par--goto-position-in-parsed-member
       position-in-member
       parsed-next-member)
      (when (and (or (json-par-token-colon-p previous-token)
                     (json-par-token-colon-p next-token))
                 (null (gethash :colon-token parsed-next-member)))
        (setq skip-following-spaces t))
      (json-par--skip-spaces-after-forward-member
       skip-following-spaces
       pos))))

(defun json-par--goto-beginning-of-next-member ()
  "Move the point to the beginning of the next member.

If the point is on the last member, move to the end of the member and return
nil.  Return t otherwise."
  (json-par--forward-spaces)
  (cond
   ((eq (char-after) ?,)
    (forward-char)
    (json-par--forward-spaces)
    t)
   ((memq (char-after) '(?\] ?\) ?} nil))
    nil)
   (t
    (let ((pos (point)))
      ;; For the sake of missing comma
      (json-par-beginning-of-member)
      (json-par-end-of-member)
      (json-par--forward-spaces)
      (when (eq pos (point))
        (json-par-end-of-member)
        (json-par--forward-spaces))
      (when (eq (char-after) ?,)
        (forward-char))
      (json-par--forward-spaces)
      (not (memq (char-after) '(?\] ?\) ?} nil)))))))

(defun json-par--skip-spaces-after-forward-member
    (go-forward original-position)
  "Adjust the point after forwarding a member.

If GO-FORWARD is non-nil, skip spaces forward.  If it hits a close bracket, go
back.
If GO-FORWARD is nil, skip spaces backward but not go beyond ORIGINAL-POSITION."
  (if go-forward
      (progn
        (json-par--forward-spaces)
        (when (memq (char-after) '(?} ?\) ?\]))
          (json-par--skip-spaces-after-forward-member
           nil
           original-position)))
    (json-par--backward-spaces)
    (when (< (point) original-position)
      (goto-char original-position))))

(defun json-par-backward-member (&optional arg)
  "Move the point backward to the previous member.

With ARG, repeat that times.  If ARG is negative, move forward.

Keep position in a member after movement.

This function treats a key-value pair in an object as one member while
`json-par-forward-sexp' treats it as three tokens (key, colon, and value)."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (if (< arg 0)
      (json-par-forward-member (- arg))
    (dotimes (_ arg)
      (json-par--backward-member-1))))

(defun json-par--backward-member-1 ()
  "Move the point backward to the previous member."
  (json-par--out-comment)
  (let ((pos (point))
        ;; If `skip-preceding-spaces' is non-nil, skip backward preceding
        ;; spaces after moving.  If it is nil, skip forward.
        skip-preceding-spaces
        next-token
        previous-token
        just-after-previous-token
        just-before-next-token
        previous-is-same-line
        position-in-member
        parsed-previous-member)
    (json-par--out-atom)
    (setq previous-token (save-excursion (json-par-backward-token)))
    (if (or (json-par-token-open-bracket-p previous-token)
            (json-par-token-outside-of-buffer-p previous-token))
        (goto-char pos)
      (setq position-in-member (json-par--position-in-member))
      (when (eq position-in-member 'empty-member)
        (setq position-in-member 'before-member))
      (setq next-token (save-excursion (json-par-forward-token)))
      (setq just-after-previous-token (eq (json-par-token-end previous-token)
                                          (point)))
      (setq just-before-next-token (eq (json-par-token-start next-token)
                                       (point)))
      (setq previous-is-same-line
            (json-par--same-line-p
             (point)
             (json-par-token-start previous-token)))
      (setq skip-preceding-spaces
            (cond
             ;; Before close bracket
             ;;
             ;; [
             ;;   1  ,
             ;;   2
             ;; |]
             ;; ↓
             ;; [
             ;;   1|  ,
             ;;   2
             ;; ]
             ((json-par-token-close-bracket-p next-token)
              t)

             ;; Before ":"
             ((json-par-token-colon-p next-token)
              (not just-before-next-token))

             ;; After ":"
             ((json-par-token-colon-p previous-token)
              (not just-before-next-token))

             ;; Before ","
             ((json-par-token-comma-p next-token)
              (not just-before-next-token))

             ;; Just after previous token
             ;;
             ;; [
             ;;   1,
             ;;   2,|
             ;;   3
             ;; ]
             ;; ↓
             ;; [
             ;;   1,|
             ;;   2,
             ;;   3
             ;; ]
             (just-after-previous-token
              t)

             ;; Just before next token (key or value)
             ;;
             ;; [
             ;;   1,
             ;;   |2
             ;; ]
             ;; ↓
             ;; [
             ;;   |1,
             ;;   2
             ;; ]
             (just-before-next-token
              nil)

             ;; On same line to the previous token
             ;;
             ;; [
             ;;   1,
             ;;   2,  |
             ;;   3
             ;; ]
             ;; ↓
             ;; [
             ;;   1,|
             ;;   2,
             ;;   3
             ;; ]
             (previous-is-same-line
              t)

             ;; Otherwise
             ;;
             ;; [
             ;;   1,
             ;; |  2
             ;; ]
             ;; ↓
             ;; [
             ;;   |1,
             ;;   2
             ;; ]
             ;;
             ;; [
             ;;   1,
             ;;   2,
             ;;   |
             ;;   3
             ;; ]
             ;; ↓
             ;; [
             ;;   1,
             ;;   |2,
             ;;
             ;;   3
             ;; ]
             (t
              nil)))
      (json-par--goto-end-of-previous-member)
      (setq parsed-previous-member (json-par--parse-member-backward))
      (json-par--goto-position-in-parsed-member
       position-in-member
       parsed-previous-member)
      (json-par--skip-spaces-after-backward-member
       skip-preceding-spaces
       pos))))

(defun json-par--goto-end-of-previous-member ()
  "Move the point to the end of the previous member.

If the point is on the first member, move to the beginning of the member and
return nil.  Return t otherwise."
  (json-par--backward-spaces)
  (cond
   ((eq (char-before) ?,)
    (backward-char)
    (json-par--backward-spaces)
    t)
   ((memq (char-before) '(?\[ ?\( ?{ nil))
    nil)
   (t
    (json-par-beginning-of-member)
    (json-par--backward-spaces)
    (when (eq (char-before) ?,)
      (backward-char))
    (json-par--backward-spaces)
    (not (memq (char-before) '(?\[ ?\( ?{ nil))))))

(defun json-par--skip-spaces-after-backward-member
    (go-backward original-position)
  "Adjust the point after backwarding a member.

If GO-BACKWARD is non-nil, skip spaces backward.  If it hits an open bracket, go
forward.
If GO-BACKWARD is nil, skip spaces forward but not go beyond ORIGINAL-POSITION."
  (if go-backward
      (progn
        (json-par--backward-spaces)
        (when (memq (char-before) '(?{ ?\( ?\[))
          (json-par--skip-spaces-after-backward-member nil original-position)))
    (json-par--forward-spaces)
    (when (< original-position (point))
      (goto-char original-position))))

(defun json-par-up-forward (&optional arg push-mark collapse-if-empty)
  "Move the point to the end of the surrounding brackets.

If the point is inside a string, an number, or a constants, move to the end of
the token instead.

If ARG is given, repeat that times.  If ARG is negative, move backward.
If PUSH-MARK is non-nil or called interactively, and the region is not active,
push a mark first.

If COLLAPSE-IF-EMPTY is non-nil and the brackets is empty, delete all spaces and
line breaks between the brackets.

When called interactively, it defaults to the value of the variable
`json-par-collapse-when-exit-from-empty-brackets'."
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)
    t
    json-par-collapse-when-exit-from-empty-brackets))
  (unless arg
    (setq arg 1))
  (if (< arg 0)
      (json-par-up-backward (- arg) push-mark collapse-if-empty)
    (when (and push-mark (not (region-active-p)))
      (push-mark))
    (json-par--out-comment)
    (let ((json-par--already-out-of-comment t)
          (json-par--already-out-of-atom json-par--already-out-of-atom))
      (dotimes (_ arg)
        (json-par--up-forward-1 collapse-if-empty)
        (setq json-par--already-out-of-atom t)))))

(defun json-par--up-forward-1 (collapse-if-empty)
  "Move the point to the end of the surrounding brackets.

If the point is inside a string, an number, or a constants, move to the end of
the token instead.

If COLLAPSE-IF-EMPTY is non-nil and the brackets is empty, delete all spaces and
line breaks between the brackets."
  (json-par--forward-spaces)
  (let ((current-atom (json-par--current-atom)))
    (if (json-par-token-inside-p current-atom)
        (goto-char (json-par-token-end current-atom))
      (while (progn
               (json-par--forward-spaces)
               (not (memq (char-after) '(nil ?\] ?\) ?}))))
        (json-par-forward-token-or-list))
      (when (memq (char-after) '(?\] ?\) ?}))
        (forward-char)
        (when collapse-if-empty
          (save-excursion
            (backward-char)
            (skip-chars-backward "\s\t\n")
            (when (memq (char-before) '(?\[ ?\( ?{))
              (backward-char)
              (json-par-oneline))))))))

(defun json-par-up-backward (&optional arg push-mark collapse-if-empty)
  "Move the point to the start of the surrounding brackets.

If the point is inside a string, an number, or a constants, move to the start of
the token instead.

If ARG is given, repeat that times.  If ARG is negative, move forward.
If PUSH-MARK is non-nil or called interactively, and the region is not active,
push a mark first.

If COLLAPSE-IF-EMPTY is non-nil and the brackets is empty, delete all spaces and
line breaks between the brackets.

When called interactively, it defaults to the value of the variable
`json-par-collapse-when-exit-from-empty-brackets'."
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)
    t
    json-par-collapse-when-exit-from-empty-brackets))
  (unless arg
    (setq arg 1))
  (if (< arg 0)
      (json-par-up-forward (- arg) push-mark collapse-if-empty)
    (when (and push-mark (not (region-active-p)))
      (push-mark))
    (json-par--out-comment)
    (let ((json-par--already-out-of-comment t)
          (json-par--already-out-of-atom json-par--already-out-of-atom))
      (dotimes (_ arg)
        (json-par--up-backward-1 collapse-if-empty)
        (setq json-par--already-out-of-atom t)))))

(defun json-par--up-backward-1 (collapse-if-empty)
  "Move the point to the start of the surrounding brackets.

If the point is inside a string, an number, or a constants, move to the start of
the token instead.

If COLLAPSE-IF-EMPTY is non-nil and the brackets is empty, delete all spaces and
line breaks between the brackets."
  (json-par--backward-spaces)
  (let ((current-atom (if json-par--already-out-of-atom
                          nil
                        (json-par--current-atom)))
        start)
    (if (json-par-token-inside-p current-atom)
        (goto-char (json-par-token-start current-atom))
      (setq start (nth 1 (syntax-ppss)))
      (if start
          (progn
            (goto-char start)
            (when collapse-if-empty
              (save-excursion
                (forward-char)
                (skip-chars-forward "\s\t\n")
                (when (memq (char-after) '(?\] ?\) ?}))
                  (forward-char)
                  (json-par-oneline)))))
        (goto-char (point-min))))))

(defun json-par--parent-token ()
  "Return the open bracket token surrounding the point.

Assuming the point is not inside a string, an number, or a constants."
  (save-excursion
    (json-par--backward-spaces)
    (if (bobp)
        (json-par-backward-token)
      (json-par-up-backward)
      (let ((parent-token (json-par-forward-token)))
        (if (json-par-token-open-bracket-p parent-token)
            parent-token
          (json-par-backward-token))))))

(defun json-par-down (&optional push-mark)
  "Move the point inside the current value/key.

If the point is before or after a string/bracket, move the point to inside the
string/bracket, then skip spaces unless the string/bracket contains only spaces.

Otherwise, keep the position.

If PUSH-MARK is non-nil or called interactively, the resulting position is not
same to the original position, and the region is not active, push a mark first."
  (interactive "p")
  (let* ((string-like-beginning-position
          (json-par--string-like-beginning-position))
         (current-atom (json-par--current-atom))
         next-token
         previous-token
         previous-comment-region
         next-comment-region
         current-string-like-body-region
         target)
    (unless (json-par-token-inside-p current-atom)
      (setq current-atom nil))
    (when (and (not string-like-beginning-position)
               (not current-atom))
      (setq next-token (save-excursion (json-par-forward-token)))
      (setq previous-token (save-excursion (json-par-backward-token)))
      (setq previous-comment-region (json-par--previous-comment-region))
      (setq next-comment-region (json-par--next-comment-region)))
    (setq target
          (cond
           ;; Inside a string, a comment, or an atom.
           ((or string-like-beginning-position current-atom)
            nil)

           ;; Between a comma, a colon, or an open bracket and a comment.
           ((and (memq (json-par-token-type previous-token)
                       '({ \( \[ \, : outside-of-buffer))
                 next-comment-region)
            (setq current-string-like-body-region
                  (json-par--region-of-string-like-body
                   (car next-comment-region)
                   t))
            (car current-string-like-body-region))

           ;; Between a comment and a comma, a colon, or a close bracket.
           ((and (memq (json-par-token-type next-token)
                       '(} \) \] \, : outside-of-buffer))
                 previous-comment-region)
            (setq current-string-like-body-region
                  (json-par--region-of-string-like-body
                   (car previous-comment-region)
                   t))
            (cdr current-string-like-body-region))

           ;; Between comments.
           ((and previous-comment-region next-comment-region)
            (setq current-string-like-body-region
                  (json-par--region-of-string-like-body
                   (car next-comment-region)
                   t))
            (car current-string-like-body-region))

           ;; Before a string or key.
           ((json-par-token-string-p next-token)
            (save-excursion
              (goto-char (1+ (json-par-token-start next-token)))
              (skip-chars-forward "\s\t\n")
              (when (= (point) (1- (json-par-token-end next-token)))
                (goto-char (1+ (json-par-token-start next-token))))
              (point)))

           ;; After a string or key.
           ((json-par-token-string-p previous-token)
            (save-excursion
              (goto-char (1- (json-par-token-end previous-token)))
              (skip-chars-backward "\s\t\n")
              (when (= (point) (1+ (json-par-token-start previous-token)))
                (goto-char (1- (json-par-token-end previous-token))))
              (point)))

           ;; Before open brackets
           ((json-par-token-open-bracket-p next-token)
            (save-excursion
              (goto-char (json-par-token-end next-token))
              (skip-chars-forward "\s\t\n")
              (when (memq (char-after) '(nil ?\] ?\) ?}))
                (goto-char (json-par-token-end next-token)))
              (point)))

           ;; After close brackets
           ((json-par-token-close-bracket-p previous-token)
            (save-excursion
              (goto-char (json-par-token-start previous-token))
              (skip-chars-backward "\s\t\n")
              (when (memq (char-before) '(nil ?\[ ?\( ?{))
                (goto-char (json-par-token-start previous-token)))
              (point)))))
    (when (and
           push-mark
           target
           (not (eq target (point)))
           (not (region-active-p)))
      (push-mark))
    (when target
      (goto-char target))))

(defun json-par-forward-record (&optional arg)
  "Move the point to the following object/array with the same key/index.

Move the point to the member with the same key/index after the point.  Keep
position in the member.  If such an object/array is not found, keep the original
position.

Search siblings after the point.  If not found, search first cousins after the
point.  If not found, search second cousins after the point, and so on.

With ARG, repeat that times.  If ARG is negative, move backward.

Return ARG minus the count of movement."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (if (< arg 0)
      (json-par-backward-record (- arg))
    (while (and
            (< 0 arg)
            (json-par--find-sibling-record-1 #'json-par--find-member-forward))
      (setq arg (1- arg))))
  (when (and (not (zerop arg))
             (called-interactively-p 'interactive))
    (message "Member not found"))
  arg)

(defun json-par-backward-record (&optional arg)
  "Move the point to the preceding object/array with the same key/index.

Move the point to the member with the same key/index before the point.  Keep
position in the member.  If such an object/array is not found, keep the original
position.

Search siblings before the point.  If not found, search first cousins before the
point.  If not found, search second cousins before the point, and so on.

With ARG, repeat that times.  If ARG is negative, move forward.

Return ARG minus the count of movement."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (if (< arg 0)
      (json-par-forward-record (- arg))
    (while (and
            (< 0 arg)
            (json-par--find-sibling-record-1 #'json-par--find-member-backward))
      (setq arg (1- arg))))
  (when (and (not (zerop arg))
             (called-interactively-p 'interactive))
    (message "Member not found"))
  arg)

(defun json-par--find-sibling-record-1 (find-member)
  "Move the point to the next object/array with the same key/index.

Return non-nil if found.  Otherwise, keep position and return nil.

See `json-par-forward-record' for details.

FIND-MEMBER is either `json-par--find-member-forward' or
`json-par--find-member-backward'."
  (let ((pos (point))
        found
        position-in-member
        key)
    (json-par--out-comment)
    (json-par--out-atom)
    (setq position-in-member (json-par--position-in-member))
    (setq key (or (save-excursion
                    (json-par-beginning-of-member)
                    (json-par--read-object-key-if-any))
                  (json-par--current-member-index)))
    (if (stringp key)
        (setq found
              (funcall find-member
                       (lambda (i)
                         (and (< 0 i)
                              (equal key (json-par--read-object-key-if-any))))
                       t))
      (json-par-up-backward)
      (setq found
            (funcall find-member
                     (lambda (i)
                       (and (< 0 i)
                            (progn
                              (json-par-beginning-of-object-value)
                              (memq (char-after) '(?\[ ?\( ?{)))
                            (progn
                              (forward-char)
                              (json-par-goto-index key))))
                     t)))
    (if found
        (json-par--goto-position-in-parsed-member
         position-in-member
         (json-par--parse-member-forward))
      (goto-char pos))
    found))

(defun json-par-tab (&optional arg)
  "Move the point to the object value if the point is on or after a key.

If the point is on a string, move to the end of the string.

Otherwise, call `indent-for-tab-command' with ARG."
  (interactive "P")
  (let ((current-atom (json-par--current-atom)))
    (when (not (json-par-token-inside-p current-atom))
      (setq current-atom nil))
    (cond
     ((json-par--object-key-p current-atom)
      (json-par-beginning-of-object-value))

     ((json-par-token-string-p current-atom)
      (goto-char (json-par-token-end current-atom)))

     ((json-par--object-key-p (save-excursion (json-par-backward-token)))
      (json-par-beginning-of-object-value))

     (t
      (indent-for-tab-command arg)))))

(defun json-par-mark-head-of-member ()
  "Mark the key of the current member if any, or the value otherwise."
  (interactive)
  (json-par-delete-head-of-member 'mark))

(defun json-par--region-of-string-like-body
    (string-like-beginning-position &optional allow-empty)
  "Return region of contents of a string.

STRING-LIKE-BEGINNING-POSITION is the start of the string.

Return a cons of the start and end positions.

- If the string is empty, return the whole string if ALLOW-EMPTY is nil.
  If ALLOW-EMPTY is non-nil, return the empty region.

- If the string contains only spaces, return the region of the string except
  double quotes.

- Otherwise, return the region of the contents of the string except the leading
  and trailing spaces."
  (save-excursion
    (goto-char string-like-beginning-position)
    (let ((string-like-end-position
           (save-excursion
             (json-par-forward-token-or-list-or-comment)
             (point)))
          start
          end)
      (cond
       ;; String
       ((eq (char-after) ?\")
        (forward-char)
        (skip-chars-forward "\s\t\n")
        (setq start (point))
        (goto-char string-like-end-position)
        (backward-char)
        (skip-chars-backward "\s\t\n")
        (setq end (point))
        (when (<= end start)
          (setq start (1+ string-like-beginning-position))
          (setq end (1- string-like-end-position))
          (when (and (= start end) (not allow-empty))
            (setq start string-like-beginning-position)
            (setq end string-like-end-position))))

       ;; Single-line comment
       ((looking-at "//")
        (setq end (line-end-position))
        (skip-chars-forward "/")
        (cond
         ;; The comment is completely empty (no spaces).
         ((eolp)
          (if allow-empty
              (setq start end)
            (setq start string-like-beginning-position)))

         ;; The comment contains only one space.
         ((looking-at "[\s\t]$")
          (setq start (point)))

         ;; The comment contains two or more space but not other.
         ((looking-at "[\s\t]\\{2,\\}$")
          (setq start (1+ (point))))

         ;; The comment contains non-space characters.
         (t
          (skip-chars-forward "\s\t")
          (setq start (point)))))

       ;; Multiline comment
       (t
        (forward-char)
        (skip-chars-forward "*")
        (cond
         ;; The comment is completely empty (no spaces).
         ((= (1+ (point)) string-like-end-position)
          (if allow-empty
              (progn
                (setq end (- string-like-end-position 2))
                (setq start end))
            (setq start string-like-beginning-position)
            (setq end string-like-end-position)))

         ;; The comment contains only one or two space.
         ((and (looking-at "[\s\t\n]\\{1,2\\}\\*+/")
               (save-excursion
                 (skip-chars-forward "\s\t\n")
                 (skip-chars-forward "*")
                 (= (1+ (point)) string-like-end-position)))
          (setq start (point))
          (skip-chars-forward "\s\t\n")
          (setq end (point)))

         ;; The comment contains three or more space but not other.
         ((save-excursion
            (skip-chars-forward "\s\t\n")
            (skip-chars-forward "*")
            (= (1+ (point)) string-like-end-position))
          (setq start (1+ (point)))
          (skip-chars-forward "\s\t\n")
          (setq end (1- (point))))

         ;; The comment contains non-space characters.
         (t
          (skip-chars-forward "\s\t\n")
          (setq start (point))
          (goto-char string-like-end-position)
          (backward-char)
          (skip-chars-backward "*")
          (skip-chars-backward "\s\t\n")
          (setq end (point))))))
      (cons start end))))

(provide 'json-par-motion)

;;; json-par-motion.el ends here
