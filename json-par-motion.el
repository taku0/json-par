;;; json-par-motion.el --- Moving the point in JSON Par mode -*- lexical-binding: t -*-

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

;; Functions for moving the point in JSON Par mode.

;;; Code:

(require 'json-par-utils)
(require 'json-par-lexer)

(declare-function json-par-oneline
                  "json-par-oneline-multiline"
                  (&optional min-level))

(defvar-local json-par--dwim-function nil)

;;; Customizations

(defcustom json-par-collapse-when-exit-from-empty-brackets t
  "If non-nil, collapse an empty array/object when exiting from it.

This affects `json-par-up-backward' and `json-par-up-forward'."
  :type 'boolean
  :group 'json-par
  :safe #'booleanp)

(defcustom json-par-place-after-down-into-object 'value
  "Target place after invoking `json-par-down' before an object.

- `value': before the value of the key-value pair.
- `member': before the whole key-value pair."
  :type '(choice (const :tag "Before value" value)
                 (const :tag "Before member" member))
  :group 'json-par
  :safe #'symbolp)


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

(defun json-par--parse-member-forward (&optional include-comment)
  "Parse the current member.

Assuming the point is at the beginning of the member.

Return a hash table with the following members:

- :start-of-member, the start position of the member
- :end-of-member, the end position of the member
- :key-token, the key token of a key-value pair, if any
- :colon-token, the colon token of a key-value pair, if any
- :value-token, the value token of member, if any

If INCLUDE-COMMENT is non-nil, start-of-member and end-of-member are placed
before/after comments if any."
  (save-excursion
    (if include-comment
        (skip-chars-forward "\s\t\n")
      (json-par--forward-spaces))
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
      (if include-comment
          (progn
            (skip-chars-backward "\s\t\n")
            (when (let ((json-par--already-out-of-comment nil))
                    (json-par--string-like-beginning-position))
              (forward-line)))
        (json-par--backward-spaces))
      (when (and (eq (char-before) ?:) (memq (char-after) '(?\s ?\t)))
        (forward-char))
      (setq end-of-member (point))
      (when (memq (char-before) '(nil ?\, ?\[ ?\( ?{))
        ;; Empty member
        (json-par--end-of-empty-member)
        (setq start-of-member (point))
        (setq end-of-member (point)))
      (puthash :start-of-member start-of-member result)
      (puthash :end-of-member end-of-member result)
      (puthash :key-token key-token result)
      (puthash :colon-token colon-token result)
      (puthash :value-token value-token result)
      result)))

(defun json-par--end-of-empty-member (&optional prefer-close-bracket)
  "Move the point to the end of empty member.

If the member is followed by a comma, move to just before the comma.

If the member is the last one and consists of multi-lines, move to the end of
second last line.

Otherwise, if PREFER-CLOSE-BRACKET is non-nil, move to just before the close
bracket and move backward one space if exists.

Otherwise, skip spaces backward and move forward one space if exists."
  (skip-chars-forward "\s\t\n")
  (unless (eq (char-after) ?,)
    (skip-chars-backward "\s\t")
    (cond
     ((eq (char-before) ?\n)
      (backward-char))
     (prefer-close-bracket
      (skip-chars-forward "\s\t")
      (when (memq (char-before) '(?\s ?\t))
        (backward-char)))
     ((memq (char-after) '(?\s ?\t))
      (forward-char)))))

(defun json-par--parse-member-backward (&optional include-comment)
  "Parse the current member.

Assuming the point is at the end of the member.

Return a hash table with the following members:

- :start-of-member, the start position of the member
- :end-of-member, the end position of the member
- :key-token, the key token of a key-value pair, if any
- :colon-token, the colon token of a key-value pair, if any
- :value-token, the value token of member, if any

If INCLUDE-COMMENT is non-nil, start-of-member and end-of-member are placed
before/after comments if any."
  (save-excursion
    (if include-comment
        (progn
          (skip-chars-backward "\s\t\n")
          (when (json-par--string-like-beginning-position)
            (forward-line)))
      (json-par--backward-spaces))
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
      (if include-comment
          (skip-chars-forward "\s\t\n")
        (json-par--forward-spaces))
      (setq start-of-member (point))
      (when (memq (char-after) '(nil ?\, ?\] ?\) ?}))
        ;; Empty member
        (json-par--end-of-empty-member)
        (setq start-of-member (point))
        (setq end-of-member (point)))
      (puthash :start-of-member start-of-member result)
      (puthash :end-of-member end-of-member result)
      (puthash :key-token key-token result)
      (puthash :colon-token colon-token result)
      (puthash :value-token value-token result)
      result)))

(defun json-par-end-of-member-point-only (&optional push-mark include-comment)
  "Move the point to the end of the current member, not including a comma.

If PUSH-MARK is non-nil or called interactively, the function is not called
repeatedly, and the region is not active, push a mark first.

If INCLUDE-COMMENT is non-nil and a comment follows the current member,
move to the end of the comment."
  (interactive
   (list
    (not (eq last-command 'json-par-end-of-member))
    nil))
  (when (and push-mark (not (region-active-p)))
    (push-mark))
  (json-par--out-comment)
  (json-par--out-atom)
  (goto-char (gethash :end-of-member
                      (json-par--parse-member-forward include-comment))))

(defun json-par-beginning-of-member-point-only
    (&optional push-mark include-comment)
  "Move the point to the start of the current member.

If PUSH-MARK is non-nil or called interactively, the function is not called
repeatedly, and the region is not active, push a mark first.

If INCLUDE-COMMENT is non-nil and a comment precedes the current member,
move to the beginning of the comment."
  (interactive
   (list
    (not (eq last-command 'json-par-beginning-of-member))
    nil))
  (when (and push-mark (not (region-active-p)))
    (push-mark))
  (json-par--out-comment)
  (json-par--out-atom)
  (goto-char (gethash :start-of-member
                      (json-par--parse-member-backward include-comment))))

(defun json-par-beginning-of-object-value-point-only
    (&optional push-mark parsed include-comment)
  "Move the point to the start of the object value of the current member.

If the point is not in a object, go to the beginning of the member.

If PUSH-MARK is non-nil or called interactively, the function is not called
repeatedly, and the region is not active, push a mark first.

If PARSED is given, it is used instead of calling
`json-par--parse-member-forward'.

If INCLUDE-COMMENT is non-nil and comments precedes the value, move to the start
of the comments."
  (interactive
   (list
    (not (eq last-command 'json-par-beginning-of-object-value))))
  (when (and push-mark (not (region-active-p)))
    (push-mark))
  (unless parsed
    (json-par-beginning-of-member-point-only)
    (setq parsed (json-par--parse-member-forward)))
  (cond
   ((gethash :value-token parsed)
    (goto-char (json-par-token-start (gethash :value-token parsed)))
    (when include-comment
      (json-par--backward-spaces)
      (skip-chars-forward "\s\t\n")))

   ((gethash :colon-token parsed)
    (goto-char (json-par-token-end (gethash :colon-token parsed)))
    (if include-comment
        (skip-chars-forward "\s\t\n")
      (json-par--forward-spaces))
    (when (memq (char-after) '(?\] ?\) ?}))
      (goto-char (json-par-token-end (gethash :colon-token parsed)))
      (skip-chars-forward "\s\t")
      (when (memq (char-after) '(?\] ?\) ?}))
        (goto-char (json-par-token-end (gethash :colon-token parsed)))
        (when (memq (char-after) '(?\s ?\t))
          (forward-char)))))

   ((gethash :key-token parsed)
    (goto-char (json-par-token-end (gethash :key-token parsed)))
    (if include-comment
        (skip-chars-forward "\s\t\n")
      (json-par--forward-spaces))
    (when (memq (char-after) '(?\] ?\) ?}))
      (goto-char (json-par-token-end (gethash :key-token parsed)))
      (skip-chars-forward "\s\t")
      (when (memq (char-after) '(?\] ?\) ?}))
        (goto-char (json-par-token-end (gethash :key-token parsed)))
        (when (memq (char-after) '(?\s ?\t))
          (forward-char)))))

   (t
    (goto-char (gethash :end-of-member parsed))
    (when include-comment
      (json-par--backward-spaces)
      (skip-chars-forward "\s\t\n")
      (goto-char (min (point) (gethash :end-of-member parsed)))))))

(defun json-par-beginning-of-list-point-only
    (&optional push-mark include-comment)
  "Move the point before the first member of the current array/object.

If PUSH-MARK is non-nil or called interactively, the function is not called
repeatedly, and the region is not active, push a mark first.

If INCLUDE-COMMENT is non-nil and comments precedes the first member, move
to the beginning of the comments."
  (interactive
   (list
    (not (eq last-command 'json-par-beginning-of-list))
    nil))
  (json-par-up-backward-point-only 1 push-mark)
  (json-par-down-point-only nil 'member include-comment))

(defun json-par-end-of-list-point-only (&optional push-mark include-comment)
  "Move the point after last the member of the current array/object.

If PUSH-MARK is non-nil or called interactively, the function is not called
repeatedly, and the region is not active, push a mark first.

If INCLUDE-COMMENT is non-nil and comments follows the last member, move
to the end of the comments."
  (interactive
   (list
    (not (eq last-command 'json-par-end-of-list))
    nil))
  (json-par-up-forward-point-only 1 push-mark)
  (json-par-down-point-only nil 'member include-comment))

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

(defun json-par--find-member-forward (p &optional max-cousin-depth)
  "Find a member satisfying a predicate P after the point.

Move the point to the start of the current member, call P, and if it return nil,
move to the next member and call P until P returns non-nil or all members are
inspected.

If P returns non-nil for some member, keep the point at the beginning of the
member and return the value returned from P.

Otherwise, move the point to the original position and return nil.

P is called with the index of the member relative the starting member, starting
from zero.

If MAX-COUSIN-DEPTH is non-nil, also search cousin members, upto
MAX-COUSIN-DEPTHth cousin.  If MAX-COUSIN-DEPTH is t, it is infinite."
  (unless max-cousin-depth
    (setq max-cousin-depth 0))
  (when (eq max-cousin-depth t)
    (setq max-cousin-depth nil))
  (json-par--find-member-from-here
   p
   (lambda ()
     (json-par--goto-beginning-of-next-member-or-cousin nil max-cousin-depth))))

(defun json-par--find-member-backward (p &optional max-cousin-depth)
  "Find a member satisfying a predicate P before the point.

Move the point to the start of the current member, call P, and if it return nil,
move to the previous member and call P until P returns non-nil or all members
are inspected.

If P returns non-nil for some member, keep the point at the beginning of the
member and return the value returned from P.

Otherwise, move the point to the original position and return nil.

P is called with the index of the member relative the starting member, starting
from zero and increasing.

If MAX-COUSIN-DEPTH is non-nil, also search cousin members, upto
MAX-COUSIN-DEPTHth cousin.  If MAX-COUSIN-DEPTH is t, it is infinite."
  (unless max-cousin-depth
    (setq max-cousin-depth 0))
  (when (eq max-cousin-depth t)
    (setq max-cousin-depth nil))
  (json-par--find-member-from-here
   p
   (lambda ()
     (prog1 (json-par--goto-end-of-previous-member-or-cousin
             nil
             max-cousin-depth)
       (json-par-beginning-of-member-point-only)))))

(defun json-par--goto-beginning-of-next-member-or-cousin
    (&optional include-empty max-depth)
  "Move the point to the beginning of the next member.

If the point is on the last member, move to the first member of the
sibling/cousin array/object.

If INCLUDE-EMPTY is non-nil, stop inside an empty brackets with the same depth.

If MAX-DEPTH is non-nil, search only up to MAX-DEPTHth cousin.

If a member is found, return t.  Return nil otherwise."
  (or (json-par--goto-beginning-of-next-member)
      (let ((depth 1)
            (token (json-par-forward-token)))
        (while (and
                (not (zerop depth))
                (or (null max-depth) (<= depth max-depth))
                (not (json-par-token-outside-of-buffer-p token)))
          (setq token (json-par-forward-token))
          (cond
           ((json-par-token-close-bracket-p token)
            (setq depth (1+ depth)))
           ((json-par-token-open-bracket-p token)
            (setq depth (1- depth))))
          (json-par--forward-spaces)
          (when (and (not include-empty)
                     (memq (char-after) '(?\] ?\) ?})))
            (setq token (json-par-forward-token))
            (setq depth (1+ depth))))
        (zerop depth))))

(defun json-par--goto-end-of-previous-member-or-cousin
    (&optional include-empty max-depth)
  "Move the point to the end of the previous member.

If the point is on the first member, move to the last member of the
sibling/cousin array/object.

If INCLUDE-EMPTY is non-nil, stop inside an empty brackets with the same depth.

If MAX-DEPTH is non-nil, search only up to MAX-DEPTHth cousin.

If a member is found, return t.  Return nil otherwise."
  (or (json-par--goto-end-of-previous-member)
      (let ((depth 1)
            (token (json-par-backward-token)))
        (while (and
                (not (zerop depth))
                (or (null max-depth) (<= depth max-depth))
                (not (json-par-token-outside-of-buffer-p token)))
          (setq token (json-par-backward-token))
          (cond
           ((json-par-token-open-bracket-p token)
            (setq depth (1+ depth)))
           ((json-par-token-close-bracket-p token)
            (setq depth (1- depth))))
          (json-par--backward-spaces)
          (when (and (not include-empty)
                     (memq (char-before) '(?\[ ?\( ?{)))
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
  (json-par-beginning-of-member-point-only)
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
              (json-par-beginning-of-member-point-only)
              (point)))
           (end-position
            (save-excursion
              (goto-char beginning-position)
              (json-par-end-of-member-point-only)
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
             (json-par-beginning-of-object-value-point-only)
             (json-par--backward-spaces)
             (point)))
        'before-value)

       (t 'after-key)))))

(defun json-par--fine-position-in-member ()
  "Return the position in a member.

Finer then `json-par--position-in-member'.

Positions and return values:

{(1)  (2)
...,(3)  (4)
\(5)
\(6)  (7)\"key\"(8)  (9)
\(10)
\(11)  (12):(13)  (14)
\(15)
\(16)  (17)\"value\"(18)  (19)
\(20)
\(21)  (22),
...
\(23)  (24)}

- 1: just-after-open-bracket
- 2: after-open-bracket-on-same-line
- 3: just-after-comma
- 4: after-comma-on-same-line
- 5: before-member
- 6: before-member-on-same-line
- 7: just-before-member
- 8: just-after-key
- 9: after-key-on-same-line
- 10: after-key
- 11: before-colon-on-same-line
- 12: just-before-colon
- 13: just-after-colon
- 14: after-colon-on-same-line
- 15: before-value
- 16: before-value-on-same-line
- 17: just-before-value
- 18: just-after-member
- 19: after-member-on-same-line
- 20: after-member
- 21: before-comma-on-same-line
- 22: just-before-comma
- 23: before-close-bracket-on-same-line
- 24: just-before-close-bracket


Priority (from high to low):

- just-before-member
- just-before-value
- just-after-colon
- just-after-member
- just-after-open-bracket
- just-before-close-bracket
- just-after-comma
- just-before-comma
- just-after-key
- just-before-colon
- before-member-on-same-line
- after-open-bracket-on-same-line
- after-comma-on-same-line
- before-member
- after-key-on-same-line
- before-colon-on-same-line
- after-key
- before-value-on-same-line
- after-colon-on-same-line
- before-value
- after-member-on-same-line
- before-close-bracket-on-same-line
- before-comma-on-same-line
- after-member"
  (save-excursion
    (json-par--out-comment)
    (json-par--out-atom)
    (let* ((parsed
            (save-excursion
              (json-par-beginning-of-member-point-only nil t)
              (json-par--parse-member-forward t)))
           (start-of-member (gethash :start-of-member parsed))
           (end-of-member (gethash :end-of-member parsed))
           (key-token (gethash :key-token parsed))
           (colon-token (gethash :colon-token parsed))
           (value-token (gethash :value-token parsed))
           (point-before-spaces-on-same-line
            (save-excursion
              (json-par--backward-spaces t)
              (point)))
           (point-after-spaces-on-same-line
            (save-excursion
              (json-par--forward-spaces t)
              (point)))
           (point-before-spaces
            (save-excursion
              (json-par--backward-spaces)
              (point)))
           (point-after-spaces
            (save-excursion
              (json-par--forward-spaces)
              (point))))
      (cond
       ((or (= (point) start-of-member)
            (cond
             (key-token
              (= (point) (json-par-token-start key-token)))
             (colon-token
              (= (point) (json-par-token-start colon-token)))))
        'just-before-member)

       ((and value-token (= (point) (json-par-token-start value-token)))
        'just-before-value)

       ((and colon-token (= (point) (json-par-token-end colon-token)))
        'just-after-colon)

       ((or (= (point) end-of-member)
            (and value-token
                 (= (point) (json-par-token-end value-token))))
        'just-after-member)

       ((memq (char-before) '(nil ?\[ ?\( ?{))
        'just-after-open-bracket)

       ((memq (char-after) '(nil ?\] ?\) ?}))
        'just-before-close-bracket)

       ((eq (char-before) ?\,)
        'just-after-comma)

       ((eq (char-after) ?\,)
        'just-before-comma)

       ((and key-token (= (point) (json-par-token-end key-token)))
        'just-after-key)

       ((and colon-token (= (point) (json-par-token-start colon-token)))
        'just-before-colon)

       ((= point-before-spaces-on-same-line
           (save-excursion
             (goto-char start-of-member)
             (json-par--backward-spaces t)
             (point)))
        'before-member-on-same-line)

       ((= point-before-spaces-on-same-line
           (save-excursion
             (goto-char start-of-member)
             (json-par--backward-spaces)
             (point)))
        (if (memq (char-before) '(nil ?\[ ?\( ?{))
            'after-open-bracket-on-same-line
          'after-comma-on-same-line))

       ((= point-before-spaces
           (save-excursion
             (goto-char start-of-member)
             (json-par--backward-spaces)
             (point)))
        'before-member)

       ((and key-token
             (= point-before-spaces-on-same-line
                (json-par-token-end key-token)))
        'after-key-on-same-line)

       ((and colon-token
             (= point-after-spaces-on-same-line
                (json-par-token-start colon-token)))
        'before-colon-on-same-line)

       ((or (and key-token
                 (= point-before-spaces
                    (json-par-token-end key-token)))
            (and colon-token
                 (= point-after-spaces
                    (json-par-token-start colon-token))))
        'after-key)

       ((and value-token
             (= point-after-spaces-on-same-line
                (json-par-token-start value-token)))
        'before-value-on-same-line)

       ((and colon-token
             (= point-before-spaces-on-same-line
                (json-par-token-end colon-token)))
        'after-colon-on-same-line)

       ((and value-token
             (= point-after-spaces
                (json-par-token-start value-token)))
        'before-value)

       ((= point-before-spaces-on-same-line
           (save-excursion
             (goto-char end-of-member)
             (json-par--backward-spaces t)
             (point)))
        'after-member-on-same-line)

       ((= point-after-spaces-on-same-line
           (save-excursion
             (goto-char end-of-member)
             (json-par--forward-spaces)
             (point)))
        (if (memq (char-before) '(nil ?\] ?\) ?}))
            'before-close-bracket-on-same-line
          'before-comma-on-same-line))

       (t
        'after-member)))))

(defun json-par--goto-position (position-in-member parsed)
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
    (json-par-beginning-of-object-value-point-only nil parsed))

   ((eq position-in-member 'after-key)
    (json-par-beginning-of-object-value-point-only nil parsed)
    (let ((previous-token (save-excursion (json-par-backward-token))))
      (when (json-par-token-colon-p previous-token)
        (goto-char (json-par-token-start previous-token))))
    (json-par--backward-spaces))))

(defun json-par--goto-fine-position (position-in-member parsed)
  "Go to POSITION-IN-MEMBER in the PARSED member.

POSITION-IN-MEMBER is a symbol returned from
`json-par--fine-position-in-member'.

PARSED is a parsed member returned from `json-par--parse-member-forward' or
`json-par--parse-member-backward'."
  (let ((start-of-member (gethash :start-of-member parsed))
        (end-of-member (gethash :end-of-member parsed))
        (key-token (gethash :key-token parsed))
        (colon-token (gethash :colon-token parsed))
        (value-token (gethash :value-token parsed)))
    (cond
     ((memq position-in-member '(just-after-open-bracket
                                 just-after-comma))
      (goto-char start-of-member)
      (json-par--backward-spaces))

     ((memq position-in-member '(after-open-bracket-on-same-line
                                 after-comma-on-same-line))
      (goto-char start-of-member)
      (json-par--backward-spaces)
      (when (and (memq (char-after) '(?\s ?\t))
                 (save-excursion
                   (skip-chars-forward "\s\t")
                   (not (eq (char-after) ?\n))))
        (forward-char)))

     ((memq position-in-member '(before-member
                                 before-member-on-same-line
                                 just-before-member))
      (goto-char start-of-member))

     ((memq position-in-member '(just-after-key
                                 after-key-on-same-line
                                 after-key))
      (cond
       (key-token
        (goto-char (json-par-token-end key-token)))
       (colon-token
        (goto-char (json-par-token-start colon-token))
        (skip-chars-backward "\s\t\n")
        (when (<= (point) start-of-member)
          (goto-char (json-par-token-start colon-token))))
       (t
        (goto-char start-of-member))))

     ((memq position-in-member '(just-before-colon
                                 before-colon-on-same-line))
      (if colon-token
          (goto-char (json-par-token-start colon-token))
        (goto-char start-of-member)))

     ((memq position-in-member '(just-after-colon
                                 after-colon-on-same-line))
      (if colon-token
          (goto-char (json-par-token-end colon-token))
        (goto-char start-of-member)))

     ((memq position-in-member '(before-value
                                 before-value-on-same-line
                                 just-before-value))
      (cond
       (value-token
        (goto-char (json-par-token-start value-token)))
       (colon-token
        (goto-char (json-par-token-end colon-token))
        (skip-chars-forward "\s\t\n")
        (when (<= (point) end-of-member)
          (goto-char (json-par-token-end colon-token))
          (when (memq (char-after) '(?\s ?\t))
            (forward-char)
            (when (memq (char-after) '(?\] ?\) ?}))
              (backward-char)))))
       (t
        (goto-char start-of-member))))

     ((memq position-in-member '(just-after-member
                                 after-member-on-same-line
                                 after-member))
      (goto-char end-of-member))

     ((memq position-in-member '(before-close-bracket-on-same-line
                                 before-comma-on-same-line))
      (goto-char end-of-member)
      (json-par--forward-spaces)
      (when (and (memq (char-before) '(?\s ?\t))
                 (save-excursion
                   (skip-chars-backward "\s\t")
                   (not (eq (char-before) ?\n))))
        (backward-char)))

     ((memq position-in-member '(just-before-close-bracket
                                 just-before-comma))
      (goto-char end-of-member)
      (json-par--forward-spaces))

     (t
      (error "Unknown position in member %s" position-in-member)))))

(defun json-par-goto-key-point-only (key &optional push-mark)
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
        (message "Key not found")))
    found))

(defun json-par-goto-index-point-only (index &optional push-mark)
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
        (message "Index out of bound")))
    found))

(defun json-par-goto-path-point-only (path &optional push-mark)
  "Move the point to the beginning of the member at PATH.

PATH is a list of following elements:

- String: key of member in object
- Number: index of member in object/array
- Symbol `*': first member matching rest of the path

Example:

  When the point is at (*1) below,
  (json-par-goto-path-point-only \\='(\"a\" * \"b\" 1))
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
        (setq found (json-par-goto-key-point-only step)))

       ;; Index
       ((numberp step)
        (setq found (json-par-goto-index-point-only step)))

       ;; Any
       ((eq step '*)
        (setq found (json-par--find-member
                     (lambda (_)
                       (json-par-beginning-of-object-value-point-only)
                       (if (memq (char-after) '(?\[ ?{))
                           (progn
                             (forward-char)
                             (json-par-goto-path-point-only path))
                         nil))))
        (setq path nil))

       ;; Invalid step
       (t
        (error "Invalid step: %S" step)))
      (when path
        (json-par-beginning-of-object-value-point-only)
        (if (memq (char-after) '(?\[ ?{))
            (forward-char)
          (setq found nil))))
    (unless found
      (goto-char pos)
      (when (called-interactively-p 'interactive)
        (message "Not found")))
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
  (or (json-par--multiple-members-on-same-line-before-point-p count)
      (json-par--multiple-members-on-same-line-after-point-p count)))

(defun json-par--multiple-members-on-same-line-before-point-p (count)
  "Return non-nil if lines before the point has multiple members.

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
      (json-par-beginning-of-member-point-only)
      (json-par--forward-spaces)
      (push (point) positions)
      (dotimes (_ count)
        (when (zerop (json-par-backward-member-point-only))
          (json-par-end-of-member-point-only)
          (json-par-beginning-of-member-point-only)
          (json-par--forward-spaces)
          (push (point) positions))))
    (not (cl-every #'json-par--beginning-of-line-or-list-p positions))))

(defun json-par--multiple-members-on-same-line-after-point-p (count)
  "Return non-nil if lines after the point has multiple members.

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
      (json-par--forward-spaces)
      (when (eq (char-after) ?,)
        (forward-char))
      (json-par-end-of-member-point-only)
      (json-par-beginning-of-member-point-only)
      (json-par--forward-spaces)
      (push (point) positions)
      (dotimes (_ count)
        (when (zerop (json-par-forward-member-point-only))
          (json-par-end-of-member-point-only)
          (json-par-beginning-of-member-point-only)
          (json-par--forward-spaces)
          (push (point) positions))))
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
  (let ((result nil)
        (end-of-list nil))
    (save-excursion
      (json-par-up-backward-point-only)
      (forward-char)
      (while (and (not result)
                  (not end-of-list)
                  (progn
                    (json-par--forward-spaces)
                    (not (memq (char-after) '(nil ?\] ?\) ?})))))
        (unless (json-par--beginning-of-line-or-list-p)
          (setq result t))
        (setq end-of-list (not (zerop (json-par-forward-member-point-only))))))
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
  (let ((result t)
        (end-of-list nil))
    (save-excursion
      (setq end-of-list (not (zerop (json-par-forward-member-point-only))))
      (while (and result
                  (not end-of-list)
                  (progn
                    (json-par--forward-spaces)
                    (not (memq (char-after) '(nil ?\] ?\) ?})))))
        (when (json-par--beginning-of-line-or-list-p)
          (setq result nil))
        (setq end-of-list (not (zerop (json-par-forward-member-point-only))))))
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
               (json-par-beginning-of-member-point-only)
               (json-par--backward-spaces)
               (not (memq (char-before) '(nil ?\[ ?\( ?{))))
        (json-par-backward-member-point-only)
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
         (json-par-backward-member-point-only)
         (json-par--backward-spaces)
         (memq (char-before) '(?\[ ?\( ?{)))))

(defun json-par--after-last-member-p (&optional next-token)
  "Return non-nil if the point is after the last member of an object/array.

If NEXT-TOKEN is non-nil, assume it is after the point."
  (unless next-token
    (setq next-token (save-excursion (json-par-forward-token))))
  (json-par-token-close-bracket-p next-token))

;;; Basic movements

(defun json-par-forward-member-point-only (&optional arg)
  "Move the point forward to the next member.

With ARG, repeat that times.  If ARG is negative, move backward.

Keep position in a member after movement.

This function treats a key-value pair in an object as one member while
`json-par-forward-sexp' treats it as multiple tokens (key, colon, value, and
optional comma).

Return ARG minus the count of movement."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (if (< arg 0)
      (json-par-backward-member-point-only (- arg))
    (while (and (not (zerop arg))
                (json-par--forward-member-1))
      (setq arg (1- arg)))
    (when (not (zerop arg))
      (setq json-par--dwim-function
            (lambda ()
              (when (zerop (json-par-goto-next-cousin-point-only))
                (json-par-forward-member-point-only (1- arg)))))
      (when (called-interactively-p 'interactive)
        (message
         "End of list.  Press %s to move to cousin"
         (substitute-command-keys
          (if (fboundp 'json-par-dwim-if-special)
              "\\[json-par-dwim-if-special]"
            "\\[json-par-dwim]")))))
    arg))

(defun json-par--forward-member-1 (&optional goto-beginning-of-next-member)
  "Move the point forward to the next member.

If the point is on the last member, keep position and return nil.  Return t
otherwise.

If GOTO-BEGINNING-OF-NEXT-MEMBER is given, it is used instead of
`json-par--goto-beginning-of-next-member'."
  (unless goto-beginning-of-next-member
    (setq goto-beginning-of-next-member
          #'json-par--goto-beginning-of-next-member))
  (let* ((pos (point))
         position-in-member)
    (json-par--out-comment)
    (json-par--out-atom)
    (if (save-excursion
          (json-par--forward-spaces)
          (forward-comment 1)
          (eobp))
        (progn
          (goto-char pos)
          nil)
      (setq position-in-member (json-par--fine-position-in-member))
      (if (funcall goto-beginning-of-next-member)
          (progn
            (json-par--goto-fine-position
             position-in-member
             (json-par--parse-member-forward))
            t)
        (goto-char pos)
        nil))))

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
      (json-par-beginning-of-member-point-only)
      (json-par-end-of-member-point-only)
      (json-par--forward-spaces)
      (when (eq pos (point))
        (json-par-end-of-member-point-only)
        (json-par--forward-spaces))
      (when (eq (char-after) ?,)
        (forward-char)
        (json-par--forward-spaces)
        t)))))

(defun json-par-backward-member-point-only (&optional arg)
  "Move the point backward to the previous member.

With ARG, repeat that times.  If ARG is negative, move forward.

Keep position in a member after movement.

This function treats a key-value pair in an object as one member while
`json-par-forward-sexp' treats it as three tokens (key, colon, and value).

Return ARG minus the count of movement."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (if (< arg 0)
      (json-par-forward-member-point-only (- arg))
    (while (and (not (zerop arg))
                (json-par--backward-member-1))
      (setq arg (1- arg)))
    (when (not (zerop arg))
      (setq json-par--dwim-function
            (lambda ()
              (when (zerop (json-par-goto-previous-cousin-point-only))
                (json-par-backward-member-point-only (1- arg)))))
      (when (called-interactively-p 'interactive)
        (message
         "Beginning of list.  Press %s to move to cousin"
         (substitute-command-keys
          (if (fboundp 'json-par-dwim-if-special)
              "\\[json-par-dwim-if-special]"
            "\\[json-par-dwim]")))))
    arg))

(defun json-par--backward-member-1 (&optional goto-end-of-previous-member)
  "Move the point backward to the previous member.

If the point is on the first member, keep position and return nil.  Return t
otherwise.

If GOTO-END-OF-PREVIOUS-MEMBER is given, it is used instead of
`json-par--goto-end-of-previous-member'."
  (unless goto-end-of-previous-member
    (setq goto-end-of-previous-member #'json-par--goto-end-of-previous-member))
  (let ((pos (point))
        position-in-member)
    (json-par--out-comment)
    (json-par--out-atom)
    (if (save-excursion
          (json-par--backward-spaces)
          (bobp))
        (progn
          (goto-char pos)
          nil)
      (setq position-in-member (json-par--fine-position-in-member))
      (if (funcall goto-end-of-previous-member)
          (progn
            (json-par--goto-fine-position
             position-in-member
             (json-par--parse-member-backward))
            t)
        (goto-char pos)
        nil))))

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
    (json-par-beginning-of-member-point-only)
    (json-par--backward-spaces)
    (when (eq (char-before) ?,)
      (backward-char)
      (json-par--backward-spaces)
      t))))

(defun json-par-goto-next-cousin-point-only (&optional arg)
  "Move the point to the first member of the following sibling of the parent.

If the following sibling of the parent is empty, go inside it.

If the parent has no following siblings, move to the 2nd cousin, and so on.

If ARG is given, repeat that times.  If ARG is negative, move backward.

Return ARG minus the count of movement."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (if (< arg 0)
      (json-par-goto-previous-cousin-point-only (- arg))
    (while (and
            (< 0 arg)
            (json-par--goto-next-cousin-1))
      (setq arg (1- arg))))
  (when (and (not (zerop arg))
             (called-interactively-p 'interactive))
    (message "End of list"))
  arg)

(defun json-par--goto-next-cousin-1 ()
  "Move the point to the beginning of the following sibling of the parent.

If the following sibling of the parent is empty, go inside it.

If the parent has no following siblings, move to the 2nd cousin, and so on.

If a nth cousin is found, return t.  Otherwise, keep the position and return
nil."
  (json-par--forward-member-1
   (lambda ()
     (json-par-end-of-list-point-only)
     (json-par--goto-beginning-of-next-member-or-cousin t))))

(defun json-par-goto-previous-cousin-point-only (&optional arg)
  "Move the point to the last member of the preceding sibling of the parent.

If the preceding sibling of the parent is empty, go inside it.

If the parent has no preceding siblings, move to the 2nd cousin, and so on.

If ARG is given, repeat that times.  If ARG is negative, move backward.

Return ARG minus the count of movement."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (if (< arg 0)
      (json-par-goto-next-cousin-point-only (- arg))
    (while (and
            (< 0 arg)
            (json-par--goto-previous-cousin-1))
      (setq arg (1- arg))))
  (when (and (not (zerop arg))
             (called-interactively-p 'interactive))
    (message "Beginning of list"))
  arg)

(defun json-par--goto-previous-cousin-1 ()
  "Move the point to the last member of the preceding sibling of the parent.

If the preceding sibling of the parent is empty, go inside it.

If the parent has no preceding siblings, move to the 2nd cousin, and so on.

If a nth cousin is found, return t.  Otherwise, keep the position and return
nil."
  (json-par--backward-member-1
   (lambda ()
     (json-par-beginning-of-list-point-only)
     (json-par--goto-end-of-previous-member-or-cousin t))))

(defun json-par-up-forward-point-only
    (&optional arg push-mark collapse-if-empty)
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
      (json-par-up-backward-point-only (- arg) push-mark collapse-if-empty)
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

(defun json-par-up-backward-point-only
    (&optional arg push-mark collapse-if-empty)
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
      (json-par-up-forward-point-only (- arg) push-mark collapse-if-empty)
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
    (cond
     ((bobp)
      (json-par-backward-token))
     ((zerop (nth 0 (syntax-ppss)))
      (goto-char (point-min))
      (json-par-backward-token))
     (t
      (json-par-up-backward-point-only)
      (let ((parent-token (json-par-forward-token)))
        (if (json-par-token-open-bracket-p parent-token)
            parent-token
          (json-par-backward-token)))))))

(defun json-par-down-point-only (&optional push-mark place include-comment)
  "Move the point inside the current value/key.

If the point is before or after a string/bracket, move the point to inside the
string/bracket, then skip spaces unless the string/bracket contains only spaces.

If the point is before a square bracket and PLACE is a symbol `value', go before
the value of the first key-value pair.  The default value is `value' when called
from Lisp program, or the value of `json-par-place-after-down-into-object'.

If the point is not before or after a string/bracket, keep the position.

If PUSH-MARK is non-nil or called interactively, the resulting position is not
same to the original position, and the region is not active, push a mark first.

If INCLUDE-COMMENT is non-nil, do not skip comments when going into an
object/array."
  (interactive
   (list
    t
    json-par-place-after-down-into-object
    nil))
  (unless place
    (setq place 'value))
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
              (if (eq place 'value)
                  (json-par-beginning-of-object-value-point-only)
                (json-par-beginning-of-member-point-only nil include-comment))
              (point)))

           ;; After close brackets
           ((json-par-token-close-bracket-p previous-token)
            (save-excursion
              (goto-char (json-par-token-start previous-token))
              (json-par-end-of-member-point-only nil include-comment)
              (point)))))
    (when (and
           push-mark
           target
           (not (eq target (point)))
           (not (region-active-p)))
      (push-mark))
    (when target
      (goto-char target))))

(defun json-par-forward-record-point-only (&optional arg)
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
      (json-par-backward-record-point-only (- arg))
    (while (and
            (< 0 arg)
            (json-par--find-sibling-record-1 #'json-par--find-member-forward))
      (setq arg (1- arg))))
  (when (and (not (zerop arg))
             (called-interactively-p 'interactive))
    (message "Member not found"))
  arg)

(defun json-par-backward-record-point-only (&optional arg)
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
      (json-par-forward-record-point-only (- arg))
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
    (setq position-in-member (json-par--fine-position-in-member))
    (setq key (or (save-excursion
                    (json-par-beginning-of-member-point-only)
                    (json-par--read-object-key-if-any))
                  (json-par--current-member-index)))
    (if (stringp key)
        (setq found
              (funcall find-member
                       (lambda (i)
                         (and (< 0 i)
                              (equal key (json-par--read-object-key-if-any))))
                       t))
      (json-par-up-backward-point-only)
      (setq found
            (funcall find-member
                     (lambda (i)
                       (and (< 0 i)
                            (progn
                              (json-par-beginning-of-object-value-point-only)
                              (memq (char-after) '(?\[ ?\( ?{)))
                            (progn
                              (forward-char)
                              (json-par-goto-index-point-only key))))
                     t)))
    (if found
        (json-par--goto-fine-position
         position-in-member
         (json-par--parse-member-forward))
      (goto-char pos))
    found))

(defun json-par-tab-point-only (&optional arg)
  "Move the point to the object value if the point is on or after a key.

If the point is on a string, move to the end of the string.

Otherwise, call `indent-for-tab-command' with ARG."
  (interactive "P")
  (let ((current-atom (json-par--current-atom)))
    (when (not (json-par-token-inside-p current-atom))
      (setq current-atom nil))
    (cond
     ((json-par--object-key-p current-atom)
      (json-par-beginning-of-object-value-point-only))

     ((json-par-token-string-p current-atom)
      (goto-char (json-par-token-end current-atom)))

     ((json-par--object-key-p (save-excursion (json-par-backward-token)))
      (json-par-beginning-of-object-value-point-only))

     (t
      (indent-for-tab-command arg)))))


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
