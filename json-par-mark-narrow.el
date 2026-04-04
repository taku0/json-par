;;; json-par-mark-narrow.el --- Marking/narrowing in JSON Par mode -*- lexical-binding: t -*-

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

;; Functions for marking or narrowing to things in JSON Par mode.

;;; Code:

(require 'json-par-lexer)
(require 'json-par-motion)
(require 'json-par-delete)

;;; Mark

(defvar-local json-par--region-history '()
  "History of region marked by `json-par-mark-more'.")

(defun json-par-mark-more (&optional arg allow-extend)
  "Mark the current value, or extend the region if active.

If ARG is given, repeat that times.  If the ARG is negative, undo
`json-par-mark-more' that times.  If called with non-numeric prefix argument,
it is converted to -1.

If the region is not active or ALLOW-EXTEND is nil:

- Inside a string, mark a word.

- If a value or key exists around the point, mark it.

- If a value or key is missing but the member is not empty, mark the member.

- If the member is empty and a comma is around the point, mark it (the following
  one is preferred).

- Inside a empty brackets, mark the whole object/array.

Otherwise, extend the region forward if the point is before the mark, or
backward if not.  See `json-par--region-to-extend-backward' or
`json-par--region-to-extend-forward' for details."
  (interactive
   (list
    current-prefix-arg
    (or (eq last-command 'json-par-mark-more)
        (use-region-p))))
  (when (or (consp arg) (eq arg '-))
    (setq arg -1))
  (setq arg (prefix-numeric-value arg))
  (if (< arg 0)
      (json-par-pop-region (- arg))
    (let* ((should-extend (and allow-extend
                               (or (and (mark t)
                                        (/= (point) (mark t)))
                                   (use-region-p))))
           (direction (if should-extend
                          (if (< (mark t) (point)) -1 1)
                        0)))
      (dotimes (_ arg)
        (json-par--mark-more-1 direction should-extend)
        (setq should-extend t)))))

(defun json-par-pop-region (&optional arg)
  "Undo `json-par-mark-more' ARG times.

If the region is not active, call `pop-to-mark-command' instead.

If ARG is negative, call `json-par-mark-more' that times.  If called with
non-numeric prefix argument, it is converted to -1."
  (interactive "P")
  (when (or (consp arg) (eq arg '-))
    (setq arg -1))
  (setq arg (prefix-numeric-value arg))
  (if (< arg 0)
      (json-par-mark-more (- arg))
    (dotimes (_ arg)
      (if (region-active-p)
          (json-par--pop-region-1)
        (pop-to-mark-command)))))

(defun json-par--pop-region-1 ()
  "Undo `json-par-mark-more'."
  (if json-par--region-history
      (let ((region (pop json-par--region-history)))
        (goto-char (nth 0 region))
        (set-mark (nth 1 region))
        (if (nth 2 region)
            (activate-mark)
          (deactivate-mark)))
    (deactivate-mark)))

(defun json-par--mark-more-1 (direction should-extend)
  "Mark the current value, or extend the region if active.

If SHOULD-EXTEND is non-nil, extend the region for DIRECTION.

Otherwise, mark the current value or the key.  See `json-par-mark-more' for
details."
  (unless should-extend
    (setq json-par--region-history '()))
  (let ((history-delete-duplicates t))
    (add-to-history 'json-par--region-history
                    (list (point) (mark t) (region-active-p))))
  (cond
   ((not should-extend)
    (let ((region (json-par--region-of-current-value-or-key-to-mark (point))))
      (if (< (point) (cdr region))
          (progn
            (goto-char (car region))
            (push-mark (cdr region) nil t))
        (goto-char (cdr region))
        (push-mark (car region) nil t))))

   ((< direction 0)
    (let ((region (json-par--region-to-extend-backward (point) (mark t))))
      (goto-char (max (point) (cdr region)))
      (set-mark (car region))))

   (t
    (let ((region (json-par--region-to-extend-forward (point) (mark t))))
      (goto-char (min (point) (car region)))
      (set-mark (cdr region))))))

(defun json-par--region-of-current-value-or-key-to-mark (point)
  "Return a region to mark around POINT when the region is not active.

Return a cons of the start and end positions."
  (save-excursion
    (goto-char point)
    (let* ((string-like-beginning-position
            (json-par--string-like-beginning-position))
           string-like-end-position
           (current-atom (json-par--current-atom))
           next-token
           previous-token
           previous-comment-region
           next-comment-region
           current-string-like-body-region
           start
           end)
      (unless (json-par-token-inside-p current-atom)
        (setq current-atom nil))
      (when (and (not string-like-beginning-position)
                 (not current-atom))
        (setq next-token (save-excursion (json-par-forward-token)))
        (setq previous-token (save-excursion (json-par-backward-token)))
        (setq previous-comment-region (json-par--previous-comment-region))
        (setq next-comment-region (json-par--next-comment-region)))
      (cond
       ;; Inside a string or a comment.
       (string-like-beginning-position
        (setq string-like-end-position
              (save-excursion
                (goto-char string-like-beginning-position)
                (json-par-forward-token-or-list-or-comment)
                (point)))
        (forward-word)
        (if (< (point) string-like-end-position)
            (progn
              (setq start point)
              (setq end (point)))
          (setq current-string-like-body-region
                (json-par--region-of-string-like-body
                 string-like-beginning-position))
          (setq start (car current-string-like-body-region))
          (setq end (cdr current-string-like-body-region))))

       ;; Inside an atom.
       (current-atom
        (setq start (json-par-token-start current-atom))
        (setq end (json-par-token-end current-atom)))

       ;; Between a comma, a colon, or an open bracket and a comment.
       ((and (memq (json-par-token-type previous-token)
                   '({ \( \[ \, : outside-of-buffer))
             next-comment-region)
        (setq start (car next-comment-region))
        (setq end (cdr next-comment-region)))

       ;; Between a comment and a comma or a close bracket.
       ((and (memq (json-par-token-type next-token)
                   '(} \) \] \, outside-of-buffer))
             previous-comment-region)
        (setq start (car previous-comment-region))
        (setq end (cdr previous-comment-region)))

       ;; Between comments.
       ((and previous-comment-region next-comment-region)
        (setq start (car next-comment-region))
        (setq end (cdr next-comment-region)))

       ;; Inside an empty brackets.
       ((and (or (json-par-token-open-bracket-p previous-token)
                 (json-par-token-outside-of-buffer-p previous-token))
             (or (json-par-token-close-bracket-p next-token)
                 (json-par-token-outside-of-buffer-p next-token)))
        (setq start (json-par-token-end previous-token))
        (setq end (json-par-token-start next-token))
        (when (= start end)
          (setq start (json-par-token-start previous-token))
          (setq end (json-par-token-end next-token))))

       ;; Between two commas or other places expecting a value or a key.
       ((and (memq (json-par-token-type previous-token)
                   '({ \( \[ \, : outside-of-buffer))
             (memq (json-par-token-type next-token)
                   '(} \) \] \, : outside-of-buffer)))
        (cond
         ((or (json-par-token-colon-p previous-token)
              (json-par-token-colon-p next-token))
          (setq start (save-excursion
                        (json-par-beginning-of-member-point-only)
                        (point)))
          (setq end (save-excursion
                      (json-par-end-of-member-point-only)
                      (point))))

         ((json-par-token-comma-p next-token)
          (setq start (json-par-token-start next-token))
          (setq end (json-par-token-end next-token)))

         ((json-par-token-comma-p previous-token)
          (setq start (json-par-token-start previous-token))
          (setq end (json-par-token-end previous-token)))))

       ;; Before a key.
       ((json-par--object-key-p next-token)
        (setq start (json-par-token-start next-token))
        (setq end (json-par-token-end next-token)))

       ;; After a key.
       ((json-par--object-key-p previous-token)
        (setq start (json-par-token-start previous-token))
        (setq end (json-par-token-end previous-token)))

       ;; Otherwise.
       (t
        (setq start (save-excursion
                      (json-par-end-of-member-point-only)
                      (json-par-backward-token-or-list)
                      (point)))
        (setq end (save-excursion
                    (json-par-end-of-member-point-only)
                    (point)))))
      (cons start end))))

(defun json-par--region-to-extend-backward (point mark)
  "Return a region to extend the active region backward.

The region is returned as a cons of the start and end positions and it should be
fused with the current active region.

The current region is represented with POINT and MARK.

- Inside a string, return the region of the word before the mark.  If the mark
  is at the beginning of the string, return the region of the whole string
  contents except the leading and trailing spaces.  If the whole contents is
  already marked, include the leading and trailing spaces.  If those spaces are
  already marked, include the double quotes.

- If the mark is middle of a atom, return the region of the whole atom.

- If the mark is after an open bracket or comma, and the member is marked
  partially, return the region of the whole member, not including commas.

- If the mark if after an open bracket, return the region of all members of the
  object/array except leading and trailing spaces.  If all members are already
  marked, include the spaces.  If the spaces are already marked, include the
  brackets.

- If the point is after a colon, return the region of the whole member.

- Otherwise, return the region of the next member."
  (save-excursion
    (goto-char point)
    (let (string-like-beginning-position
          current-atom
          previous-token
          mark-is-after-comma
          extended-region
          start
          end)
      (goto-char mark)
      (setq string-like-beginning-position
            (json-par--string-like-beginning-position))
      (setq current-atom (json-par--current-atom))
      (unless (json-par-token-inside-p current-atom)
        (setq current-atom nil))
      (when (and (not string-like-beginning-position)
                 (not current-atom))
        (setq previous-token
              (save-excursion (json-par-backward-token-or-list-or-comment)))
        (setq mark-is-after-comma
              (save-excursion
                (json-par-token-comma-p (json-par-backward-token)))))
      (cond
       ;; Inside a string or a comment.
       (string-like-beginning-position
        (backward-word)
        (if (< (point) string-like-beginning-position)
            (progn
              (setq extended-region
                    (json-par--extended-region-inside-string-like
                     mark
                     point
                     string-like-beginning-position))
              (setq start (car extended-region))
              (setq end (cdr extended-region)))
          (setq start (point))
          (setq end point)))

       ;; Inside an atom.
       (current-atom
        (setq start (json-par-token-start current-atom))
        (setq end (json-par-token-end current-atom)))

       ;; After an open bracket or comma, and the member is marked partially.
       ((and (or (json-par-token-open-bracket-p previous-token)
                 (json-par-token-comma-p previous-token)
                 (json-par-token-outside-of-buffer-p previous-token))
             (or (save-excursion
                   (goto-char point)
                   (json-par-up-backward-point-only)
                   (< (json-par-token-start previous-token) (point)))
                 (and (save-excursion
                        (goto-char point)
                        (json-par-end-of-member-point-only)
                        (json-par--forward-spaces)
                        (skip-chars-backward "\s\t\n")
                        (< point (point)))
                      (save-excursion
                        (goto-char point)
                        (json-par-beginning-of-member-point-only)
                        (json-par--backward-spaces)
                        (skip-chars-forward "\s\t\n")
                        (< (point) point)))))
        (setq start (point))
        (setq end (save-excursion
                    (json-par-end-of-member-point-only)
                    (while (< (point) point)
                      (json-par-forward-member-point-only)
                      (json-par--forward-spaces))
                    (json-par--forward-spaces)
                    (skip-chars-backward "\s\t\n")
                    (point))))

       ;; Beginning of the buffer or after an open bracket.
       ((or (json-par-token-outside-of-buffer-p previous-token)
            (json-par-token-open-bracket-p previous-token))
        (setq extended-region
              (json-par--extended-region-inside-brackets mark point))
        (setq start (car extended-region))
        (setq end (cdr extended-region)))

       ;; After a colon.
       ((json-par-token-colon-p previous-token)
        (setq start (save-excursion
                      (json-par-beginning-of-member-point-only)
                      (point)))
        (setq end (save-excursion
                    (json-par-end-of-member-point-only)
                    (point))))

       ;; One or more members are marked.  Case 1.
       ;; Mark one more member.
       (mark-is-after-comma
        (setq end (point))
        (json-par-backward-token)
        (json-par-beginning-of-member-point-only)
        (setq start (point)))

       ;; One or more members are marked.  Case 2.
       ;; Mark one more member.
       (t
        (setq end (point))
        (json-par-beginning-of-member-point-only)
        (setq start (point))))
      (cons start end))))

(defun json-par--region-to-extend-forward (point mark)
  "Return a region to extend the active region forward.

The region is returned as a cons of the start and end positions and it should be
fused with the current active region.

The current region is represented with POINT and MARK.

- Inside a string, return the region of the word after the mark.  If the mark
  is at the end of the string, return the region of the whole string contents
  except the leading and trailing spaces.  If the whole contents is already
  marked, include the leading and trailing spaces.  If those spaces are already
  marked, include the double quotes.

- If the mark is middle of a atom, return the region of the whole atom.

- If the mark is before a close bracket or comma, and the member is marked
  partially, return the region of the whole member, not including commas.

- If the mark if before a close bracket, return the region of all members of the
  object/array except leading and trailing spaces.  If all members are already
  marked, include the spaces.  If the spaces are already marked, include the
  brackets.

- If the point is before a colon, return the region of the whole member.

- Otherwise, return the region of the next member."
  (save-excursion
    (goto-char point)
    (let* (string-like-beginning-position
           string-like-end-position
           current-atom
           next-token
           mark-is-before-comma
           extended-region
           start
           end)
      (goto-char mark)
      (setq string-like-beginning-position
            (json-par--string-like-beginning-position))
      (setq current-atom (json-par--current-atom))
      (unless (json-par-token-inside-p current-atom)
        (setq current-atom nil))
      (when (and (not string-like-beginning-position)
                 (not current-atom))
        (setq next-token
              (save-excursion (json-par-forward-token-or-list-or-comment)))
        (setq mark-is-before-comma
              (save-excursion
                (json-par-token-comma-p (json-par-forward-token)))))
      (cond
       ;; Inside a string or a comment.
       (string-like-beginning-position
        (setq string-like-end-position
              (save-excursion
                (goto-char string-like-beginning-position)
                (json-par-forward-token-or-list-or-comment)
                (point)))
        (forward-word)
        (if (< string-like-end-position (point))
            (progn
              (setq extended-region
                    (json-par--extended-region-inside-string-like
                     point
                     mark
                     string-like-beginning-position))
              (setq start (car extended-region))
              (setq end (cdr extended-region)))
          (setq start point)
          (setq end (point))))

       ;; Inside an atom.
       (current-atom
        (setq start (json-par-token-start current-atom))
        (setq end (json-par-token-end current-atom)))

       ;; Before close bracket or comma and member is marked partially.
       ((and (or (json-par-token-close-bracket-p next-token)
                 (json-par-token-comma-p next-token)
                 (json-par-token-outside-of-buffer-p next-token))
             (or
              (save-excursion
                (goto-char point)
                (json-par-up-forward-point-only)
                (< (point) (json-par-token-end next-token)))
              (and (save-excursion
                     (goto-char point)
                     (json-par-beginning-of-member-point-only)
                     (json-par--backward-spaces)
                     (skip-chars-forward "\s\t\n")
                     (< (point) point))
                   (save-excursion
                     (goto-char point)
                     (json-par-end-of-member-point-only)
                     (json-par--forward-spaces)
                     (skip-chars-backward "\s\t\n")
                     (< point (point))))))
        (setq end (point))
        (setq start (save-excursion
                      (json-par-beginning-of-member-point-only)
                      (while (< point (point))
                        (json-par-backward-member-point-only)
                        (json-par--backward-spaces))
                      (json-par--backward-spaces)
                      (skip-chars-forward "\s\t\n")
                      (point))))

       ;; End of the buffer or before close bracket.
       ((or (json-par-token-outside-of-buffer-p next-token)
            (json-par-token-close-bracket-p next-token))
        (setq extended-region
              (json-par--extended-region-inside-brackets point mark))
        (setq start (car extended-region))
        (setq end (cdr extended-region)))

       ;; Before colon.
       ((json-par-token-colon-p next-token)
        (setq start (save-excursion
                      (json-par-beginning-of-member-point-only)
                      (point)))
        (setq end (save-excursion
                    (json-par-end-of-member-point-only)
                    (point))))

       ;; One or more members are marked.  Case 1.
       ;; Mark one more.
       (mark-is-before-comma
        (setq start (point))
        (json-par-forward-token)
        (json-par-end-of-member-point-only)
        (setq end (point)))

       ;; One or more members are marked.  Case 2.
       ;; Mark one more.
       (t
        (setq start (point))
        (json-par-end-of-member-point-only)
        (setq end (point))))
      (cons start end))))

(defun json-par--extended-region-inside-string-like
    (original-region-start original-region-end string-like-beginning-position)
  "Return the region of the contents of the string to extend.

Assuming the current region is between ORIGINAL-REGION-START and
ORIGINAL-REGION-END.

STRING-LIKE-BEGINNING-POSITION is the start of the string.

See `json-par--region-to-extend-forward' for details"
  (let* ((string-like-region
          (json-par--region-of-string-like-body
           string-like-beginning-position))
         (start (car string-like-region))
         (end (cdr string-like-region))
         string-like-end-position)
    (when (<= original-region-start start end original-region-end)
      (save-excursion
        (goto-char start)
        (skip-chars-backward "\s\t\n")
        (setq start (max string-like-beginning-position (point)))
        (setq string-like-end-position
              (save-excursion
                (goto-char string-like-beginning-position)
                (json-par-forward-token-or-list-or-comment)
                (point)))
        (goto-char end)
        (skip-chars-forward "\s\t\n")
        (unless (json-par--string-like-beginning-position)
          (forward-line 0)
          (backward-char))
        (setq end (min string-like-end-position (point)))
        (when (<= original-region-start start end original-region-end)
          (setq start string-like-beginning-position)
          (setq end string-like-end-position))))
    (cons start end)))

(defun json-par--extended-region-inside-brackets
    (original-region-start original-region-end)
  "Return the region of the members of the object/array to extend.

Assuming the current region is between ORIGINAL-REGION-START and
ORIGINAL-REGION-END.

See `json-par--region-to-extend-forward' for details"
  (let* (token
         (start (save-excursion
                  (json-par-up-backward-point-only)
                  (skip-chars-forward "\s\t\n")
                  (setq token (save-excursion (json-par-forward-token)))
                  (when (json-par-token-open-bracket-p token)
                    (goto-char (json-par-token-end token))
                    (skip-chars-forward "\s\t\n"))
                  (point)))
         (end (save-excursion
                (json-par-up-forward-point-only)
                (skip-chars-backward "\s\t\n")
                (setq token (save-excursion (json-par-backward-token)))
                (when (json-par-token-close-bracket-p token)
                  (goto-char (json-par-token-start token))
                  (skip-chars-backward "\s\t\n"))
                (point))))
    (when (<= original-region-start start end original-region-end)
      (setq start (save-excursion
                    (goto-char start)
                    (skip-chars-backward "\s\t\n")
                    (point)))
      (setq end (save-excursion
                  (goto-char end)
                  (skip-chars-forward "\s\t\n")
                  (point)))
      (when (<= original-region-start start end original-region-end)
        (setq start (save-excursion
                      (goto-char start)
                      (json-par-up-backward-point-only)
                      (point)))
        (setq end (save-excursion
                    (goto-char end)
                    (json-par-up-forward-point-only)
                    (point)))))
    (cons start end)))

;;; Interactive mark

(defmacro json-par--save-region-history-if-moved (&rest body)
  "Add current region to history if region changed after evaluating BODY.

Region is added to `json-par--region-history'.

Return the last value of BODY."
  (declare (debug (body)))
  (let ((current-region (make-symbol "current-region")))
    `(let ((,current-region (list (point) (mark t) (region-active-p))))
       (prog1 (progn ,@body)
         (when (and (region-active-p)
                    (not (equal ,current-region (list (point) (mark t) t))))
           (add-to-history 'json-par--region-history ,current-region nil t))))))

(defun json-par-end-of-member (&optional push-mark)
  "Move the point to the end of the current member, not including a comma.

If region is active, move mark to the beginning of the member.

If PUSH-MARK is non-nil or called interactively, the function is not called
repeatedly, and the region is not active, push a mark first."
  (interactive
   (list
    (not (eq last-command 'json-par-end-of-member))))
  (json-par--save-region-history-if-moved
   (json-par-end-of-member-point-only push-mark)
   (when (region-active-p)
     (json-par-end-of-member-point-only nil t)
     (set-mark (save-excursion
                 (json-par-beginning-of-member-point-only nil t)
                 (point))))))

(defun json-par-beginning-of-member (&optional push-mark)
  "Move the point to the start of the current member.

If the region is active, move mark to the end of the member.

If PUSH-MARK is non-nil or called interactively, the function is not called
repeatedly, and the region is not active, push a mark first."
  (interactive
   (list
    (not (eq last-command 'json-par-beginning-of-member))))
  (json-par--save-region-history-if-moved
   (json-par-beginning-of-member-point-only push-mark)
   (when (region-active-p)
     (json-par-beginning-of-member-point-only nil t)
     (set-mark (save-excursion
                 (json-par-end-of-member-point-only nil t)
                 (point))))))

(defun json-par-beginning-of-object-value (&optional push-mark parsed)
  "Move the point to the start of the object value of the current member.

If the point is not in a object, go to the beginning of the member.

If the region is active, move mark to the end of the member.

If PUSH-MARK is non-nil or called interactively, the function is not called
repeatedly, and the region is not active, push a mark first.

If PARSED is given, it is used instead of calling
`json-par--parse-member-forward'."
  (interactive
   (list
    (not (eq last-command 'json-par-beginning-of-object-value))))
  (json-par--save-region-history-if-moved
   (json-par-beginning-of-object-value-point-only push-mark parsed t)
   (when (region-active-p)
     (set-mark (save-excursion
                 (json-par-end-of-member-point-only nil t)
                 (point))))))

(defun json-par-beginning-of-list (&optional push-mark)
  "Move the point before the first member of the current array/object.

If the region is active, move mark to the end of the member where mark is.

If PUSH-MARK is non-nil or called interactively, the function is not called
repeatedly, and the region is not active, push a mark first."
  (interactive
   (list
    (not (eq last-command 'json-par-beginning-of-list))))
  (json-par--save-region-history-if-moved
   (json-par-beginning-of-list-point-only push-mark t)
   (when (region-active-p)
     (set-mark (save-excursion
                 (goto-char (mark t))
                 (json-par-end-of-member-point-only nil t)
                 (point)))
     (when (and (= (point) (mark t))
                (save-excursion
                  (skip-chars-forward "\s\t\n")
                  (not (eq (char-after) ?\,))))
       (skip-chars-forward "\s\t\n")
       (set-mark (point))
       (skip-chars-backward "\s\t\n")))))

(defun json-par-end-of-list (&optional push-mark)
  "Move the point after last the member of the current array/object.

If the region is active, move mark to the beginning of the member where mark is.

If PUSH-MARK is non-nil or called interactively, the function is not called
repeatedly, and the region is not active, push a mark first."
  (interactive
   (list
    (not (eq last-command 'json-par-end-of-list))))
  (json-par--save-region-history-if-moved
   (json-par-end-of-list-point-only push-mark t)
   (when (region-active-p)
     (set-mark (save-excursion
                 (goto-char (mark t))
                 (json-par-beginning-of-member-point-only nil t)
                 (point)))
     (when (and (= (point) (mark t))
                (save-excursion
                  (skip-chars-backward "\s\t\n")
                  (not (eq (char-after) ?\,))))
       (skip-chars-backward "\s\t\n")
       (set-mark (point))
       (skip-chars-forward "\s\t\n")))))

(defun json-par-goto-key (key &optional push-mark)
  "Move the point to the beginning of the member with KEY.

If the region is active, move mark to the end of the member.

If PUSH-MARK is non-nil and the region is not active, push a mark first.

Return non-nil if KEY found.  Otherwise, keep the original position and return
nil."
  (interactive "MGoto key: \np")
  (json-par--save-region-history-if-moved
   (prog1 (json-par-goto-key-point-only key push-mark)
     (when (region-active-p)
       (json-par-beginning-of-member-point-only nil t)
       (set-mark (save-excursion
                   (json-par-end-of-member-point-only nil t)
                   (point)))))))

(defun json-par-goto-index (index &optional push-mark)
  "Move the point to the beginning of the member at INDEX.

If the region is active, move mark to the end of the member.

If PUSH-MARK is non-nil and the region is not active, push a mark first.

Return non-nil if INDEX found.  Otherwise, keep the original position and return
nil."
  (interactive "nGoto index: \np")
  (json-par--save-region-history-if-moved
   (prog1 (json-par-goto-index-point-only index push-mark)
     (when (region-active-p)
       (json-par-beginning-of-member-point-only nil t)
       (set-mark (save-excursion
                   (json-par-end-of-member-point-only nil t)
                   (point)))))))

(defun json-par-goto-path (path &optional push-mark)
  "Move the point to the beginning of the member at PATH.

PATH is a list of following elements:

- String: key of member in object
- Number: index of member in object/array
- Symbol `*': first member matching rest of the path

Example:

  When the point is at (*1) below, (json-par-goto-path \\='(\"a\" * \"b\" 1))
  move the point to (*2).

  {
    (*1) \"a\": [
      { \"a\": 1 },
      { \"b\": [ 2, (*2) 3, 4] },
      { \"c\": 3 }
    ]
  }

If the region is active, move mark to the end of the member.

If PUSH-MARK is non-nil and the region is not active, push a mark first.

Return non-nil if PATH found.  Otherwise, keep the original position and return
nil."
  (json-par--save-region-history-if-moved
   (prog1 (json-par-goto-path-point-only path push-mark)
     (when (region-active-p)
       (json-par-beginning-of-member-point-only nil t)
       (set-mark (save-excursion
                   (json-par-end-of-member-point-only nil t)
                   (point)))))))

(defun json-par--adjust-region-to-member-boundary ()
  "Place point and mark to member boundary."
  (let ((mark-is-before-point (< (mark t) (point))))
    (if mark-is-before-point
        (json-par-end-of-member-point-only nil t)
      (json-par-beginning-of-member-point-only nil t))
    (set-mark (save-excursion
                (goto-char (mark t))
                (if mark-is-before-point
                    (json-par-beginning-of-member-point-only nil t)
                  (json-par-end-of-member-point-only nil t))
                (point)))
    ;; Is empty array/object?
    (when (and (= (point) (mark t))
               (save-excursion
                 (skip-chars-backward "\s\t\n")
                 (memq (char-before) '(nil ?\[ ?\( ?{)))
               (save-excursion
                 (skip-chars-forward "\s\t\n")
                 (memq (char-after) '(nil ?\] ?\) ?}))))
      (if mark-is-before-point
          (progn
            (skip-chars-backward "\s\t\n")
            (set-mark (point))
            (skip-chars-forward "\s\t\n"))
        (skip-chars-forward "\s\t\n")
        (set-mark (point))
        (skip-chars-backward "\s\t\n")))))

(defun json-par-forward-member (&optional arg)
  "Move the point forward to the next member.

With ARG, repeat that times.  If ARG is negative, move backward.

If the region is active, place point to the end of the member after movement
and place mark to the beginning of the member where mark is.

If the region is not active, keep position in a member after movement.

This function treats a key-value pair in an object as one member while
`json-par-forward-sexp' treats it as multiple tokens (key, colon, value, and
optional comma).

Return ARG minus the count of movement."
  (interactive "p")
  (json-par--save-region-history-if-moved
   (prog1 (json-par-forward-member-point-only arg)
     (when (region-active-p)
       (json-par--adjust-region-to-member-boundary)))))

(defun json-par-backward-member (&optional arg)
  "Move the point backward to the previous member.

With ARG, repeat that times.  If ARG is negative, move forward.

If the region is active, place point to the beginning of the member after
movement and place mark to the end of the member where mark is.

If the region is not active, keep position in a member after movement.

This function treats a key-value pair in an object as one member while
`json-par-forward-sexp' treats it as three tokens (key, colon, and value).

Return ARG minus the count of movement."
  (interactive "p")
  (json-par--save-region-history-if-moved
   (prog1 (json-par-backward-member-point-only arg)
     (when (region-active-p)
       (json-par--adjust-region-to-member-boundary)))))

(defun json-par-goto-next-cousin (&optional arg)
  "Move the point to the first member of the following sibling of the parent.

If the following sibling of the parent is empty, go inside it.

If the parent has no following siblings, move to the 2nd cousin, and so on.

If ARG is given, repeat that times.  If ARG is negative, move backward.

If the region is active, mark the member after movement.

Return ARG minus the count of movement."
  (interactive "p")
  (json-par--save-region-history-if-moved
   (let ((mark-is-before-point (and (region-active-p) (< (mark t) (point)))))
     (prog1 (json-par-goto-next-cousin-point-only arg)
       (when (region-active-p)
         (if mark-is-before-point
             (progn
               (json-par-beginning-of-member-point-only nil t)
               (set-mark (point))
               (json-par-end-of-member-point-only nil t))
           (progn
             (json-par-end-of-member-point-only nil t)
             (set-mark (point))
             (json-par-beginning-of-member-point-only nil t))))))))

(defun json-par-goto-previous-cousin (&optional arg)
  "Move the point to the last member of the preceding sibling of the parent.

If the preceding sibling of the parent is empty, go inside it.

If the parent has no preceding siblings, move to the 2nd cousin, and so on.

If ARG is given, repeat that times.  If ARG is negative, move backward.

If the region is active, mark the member after movement.

Return ARG minus the count of movement."
  (interactive "p")
  (json-par--save-region-history-if-moved
   (let ((mark-is-before-point (and (region-active-p) (< (mark t) (point)))))
     (prog1 (json-par-goto-previous-cousin-point-only arg)
       (when (region-active-p)
         (if mark-is-before-point
             (progn
               (json-par-beginning-of-member-point-only nil t)
               (set-mark (point))
               (json-par-end-of-member-point-only nil t))
           (progn
             (json-par-end-of-member-point-only nil t)
             (set-mark (point))
             (json-par-beginning-of-member-point-only nil t))))))))

(defun json-par-up-forward (&optional arg push-mark collapse-if-empty)
  "Move the point to the end of the surrounding brackets.

If the point is inside a string, an number, or a constants, move to the end of
the token instead.

If ARG is given, repeat that times.  If ARG is negative, move backward.

If the region is active, mark the key or value after movement.

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
  (json-par--save-region-history-if-moved
   (json-par-up-forward-point-only arg push-mark collapse-if-empty)
   (when (region-active-p)
     (set-mark (save-excursion
                 (json-par-backward-token-or-list)
                 (point))))))

(defun json-par-up-backward (&optional arg push-mark collapse-if-empty)
  "Move the point to the start of the surrounding brackets.

If the point is inside a string, an number, or a constants, move to the start of
the token instead.

If ARG is given, repeat that times.  If ARG is negative, move forward.

If the region is active, mark the key or value after movement.

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
  (json-par--save-region-history-if-moved
   (json-par-up-backward-point-only arg push-mark collapse-if-empty)
   (when (region-active-p)
     (set-mark (save-excursion
                 (json-par-forward-token-or-list)
                 (point))))))

(defun json-par-down (&optional push-mark place)
  "Move the point inside the current value/key.

If the point is before or after a string/bracket, move the point to inside the
string/bracket, then skip spaces unless the string/bracket contains only spaces.

If the point is before a square bracket and PLACE is a symbol `value', go before
the value of the first key-value pair.  The default value is `value' when called
from Lisp program, or the value of `json-par-place-after-down-into-object'.

If the point is not before or after a string/bracket, keep the position.

If the region is active, mark contents of the current value/key.

If PUSH-MARK is non-nil or called interactively, the resulting position is not
same to the original position, and the region is not active, push a mark first."
  (interactive
   (list
    t
    json-par-place-after-down-into-object))
  (unless place
    (setq place 'value))
  (json-par--save-region-history-if-moved
   (let ((pos (point))
         region
         string-like-beginning-position)
     (json-par-down-point-only push-mark place)
     (when (and (region-active-p) (/= pos (point)))
       (setq string-like-beginning-position
             (json-par--string-like-beginning-position))
       (setq region (cond
                     (string-like-beginning-position
                      (json-par--region-of-string-like-body
                       string-like-beginning-position
                       t))

                     ((json-par-token-close-bracket-p
                       (save-excursion (json-par-forward-token)))
                      (json-par-end-of-member-point-only nil t)
                      (cons (save-excursion
                              (json-par-beginning-of-list-point-only nil t)
                              (point))
                            (point)))

                     (t
                      (json-par-beginning-of-member-point-only nil t)
                      (cons (point)
                            (save-excursion
                              (json-par-end-of-list-point-only nil t)
                              (point))))))
       (if (= (point) (car region))
           (set-mark (cdr region))
         (set-mark (car region)))))))

(defun json-par-forward-record (&optional arg)
  "Move the point to the following object/array with the same key/index.

Move the point to the member with the same key/index after the point.  Keep
position in the member.  If such an object/array is not found, keep the original
position.

Search siblings after the point.  If not found, search first cousins after the
point.  If not found, search second cousins after the point, and so on.

With ARG, repeat that times.  If ARG is negative, move backward.

If the region is active, mark the member after movement.

Return ARG minus the count of movement."
  (interactive "p")
  (json-par--save-region-history-if-moved
   (let ((mark-is-before-point (and (region-active-p) (< (mark t) (point)))))
     (prog1 (json-par-forward-record-point-only arg)
       (when (region-active-p)
         (if mark-is-before-point
             (progn
               (json-par-beginning-of-member-point-only nil t)
               (set-mark (point))
               (json-par-end-of-member-point-only nil t))
           (progn
             (json-par-end-of-member-point-only nil t)
             (set-mark (point))
             (json-par-beginning-of-member-point-only nil t))))))))

(defun json-par-backward-record (&optional arg)
  "Move the point to the preceding object/array with the same key/index.

Move the point to the member with the same key/index before the point.  Keep
position in the member.  If such an object/array is not found, keep the original
position.

Search siblings before the point.  If not found, search first cousins before the
point.  If not found, search second cousins before the point, and so on.

With ARG, repeat that times.  If ARG is negative, move forward.

If the region is active, mark the member after movement.

Return ARG minus the count of movement."
  (interactive "p")
  (json-par--save-region-history-if-moved
   (let ((mark-is-before-point (and (region-active-p) (< (mark t) (point)))))
     (prog1 (json-par-backward-record-point-only arg)
       (when (region-active-p)
         (if mark-is-before-point
             (progn
               (json-par-beginning-of-member-point-only nil t)
               (set-mark (point))
               (json-par-end-of-member-point-only nil t))
           (progn
             (json-par-end-of-member-point-only nil t)
             (set-mark (point))
             (json-par-beginning-of-member-point-only nil t))))))))

(defun json-par-tab (&optional arg)
  "Move the point to the object value if the point is on or after a key.

If the point is on a string, move to the end of the string.

Otherwise, call `indent-for-tab-command' with ARG.

If the region is active, mark the key or value after movement."
  (interactive "P")
  (json-par--save-region-history-if-moved
   (unless (and
            (region-active-p)
            (save-excursion
              (skip-chars-backward "\s\t")
              (bolp)))
     (json-par-tab-point-only arg))
   (when (region-active-p)
     (setq deactivate-mark nil)
     (cond
      ;; Empty member
      ((and (save-excursion
              (skip-chars-forward "\s\t\n")
              (memq (char-after) '(nil ?\, ?\] ?\) ?})))
            (save-excursion
              (skip-chars-backward "\s\t\n")
              (memq (char-before) '(nil ?\, ?\[ ?\( ?{))))
       (set-mark (point))
       (json-par--adjust-region-to-member-boundary))

      ;; Before value
      ((save-excursion
         (json-par--backward-spaces)
         (eq (char-before) ?:))
       (json-par-beginning-of-object-value-point-only nil nil t)
       (set-mark (save-excursion
                   (json-par-end-of-member-point-only nil t)
                   (when (save-excursion
                           (json-par--backward-spaces)
                           (eq (char-before) ?:))
                     ;; Empty value
                     (json-par--end-of-empty-member t))
                   (point))))

      ;; After member
      ((save-excursion
         (json-par--forward-spaces)
         (memq (char-after) '(nil ?\, ?\] ?\) ?})))
       (json-par-end-of-member-point-only nil t)
       (set-mark (save-excursion
                   (json-par-beginning-of-object-value-point-only nil nil t)
                   (point))))

      ;; Before key
      ((and (eq (char-after) ?\")
            (save-excursion
              (forward-char)
              (json-par--skip-string)
              (json-par--forward-spaces)
              (eq (char-after) ?:)))
       (json-par-beginning-of-member-point-only nil t)
       (set-mark (save-excursion
                   (json-par-forward-token)
                   (json-par--forward-spaces)
                   (skip-chars-backward "\s\t\n")
                   ;; In single-line comment?
                   (when (json-par--string-like-beginning-position)
                     (forward-line))
                   (point)))
       (when (and (eq (char-after) ?\")
                  (eq (char-before (mark t)) ?\"))
         (forward-char)
         (set-mark (1- (mark t)))))

      ;; Before member
      (t
       (json-par-beginning-of-member-point-only nil t)
       (set-mark (save-excursion
                   (json-par-end-of-member-point-only nil t)
                   (point))))))))

(defun json-par-mark-head-of-member ()
  "Mark the key of the current member.

If not inside an object, mark the whole member.

If the current member don't have a key, insert an empty key and place point and
mark inside the key."
  (interactive)
  (json-par--save-region-history-if-moved
   (let* ((pos (point))
          (parsed (progn
                    (json-par-beginning-of-member-point-only nil t)
                    (json-par--parse-member-forward)))
          (key-token (gethash :key-token parsed))
          (colon-token (gethash :colon-token parsed))
          (value-token (gethash :value-token parsed))
          (parent-token (json-par--parent-token))
          (inside-object (json-par-token-open-curly-bracket-p parent-token)))
     (cond
      (key-token
       (goto-char (json-par-token-start key-token))
       (json-par--backward-spaces)
       (skip-chars-forward "\s\t\n")
       (set-mark (save-excursion
                   (goto-char (json-par-token-end key-token))
                   (json-par--forward-spaces)
                   (skip-chars-backward "\s\t\n")
                   (when (json-par--string-like-beginning-position)
                     (forward-line))
                   (point)))
       (when (and (eq (char-after) ?\")
                  (eq (char-before (mark t)) ?\"))
         (forward-char)
         (set-mark (1- (mark t))))
       (when (<= (json-par-token-end key-token) pos)
         (exchange-point-and-mark)))

      (colon-token
       (goto-char (json-par-token-start colon-token))
       (json-par-insert-double-quotes)
       (set-mark (point)))

      (inside-object
       (when value-token
         (goto-char (json-par-token-start value-token)))
       (json-par-insert-double-quotes)
       (set-mark (point)))

      (t
       (set-mark (point))
       (json-par--adjust-region-to-member-boundary))))))


;;; Narrow

(defvar-local json-par--narrow-direction 0
  "Direction to extend the narrowed area when `json-par-narrow' is repeated.")

(defvar-local json-par--restriction-history '()
  "History of the narrowed areas by `json-par-narrow'.")

(defun json-par-narrow (&optional arg allow-extend)
  "Narrow to the current value, or extend the narrowed area if repeated.

If ARG is given, repeat that times.  If the ARG is negative, undo
`json-par-narrow' that times.  If called with non-numeric prefix argument,
it is converted to -1.

See `json-par-mark-more' for ALLOW-EXTEND and what region narrowed to or
extend."
  (interactive
   (list
    current-prefix-arg
    (eq last-command 'json-par-narrow)))
  (when (or (consp arg) (eq arg '-))
    (setq arg -1))
  (setq arg (prefix-numeric-value arg))
  (if (< arg 0)
      (json-par-pop-restriction (- arg))
    (let* ((should-extend allow-extend)
           (direction
            (cond (should-extend json-par--narrow-direction)
                  ((region-active-p) (if (< (mark t) (point)) -1 1))
                  (t 0))))
      (setq json-par--narrow-direction direction)
      (dotimes (_ arg)
        (json-par--narrow-1 direction should-extend)
        (setq should-extend t))))
  (recenter))

(defun json-par-pop-restriction (&optional arg)
  "Undo `json-par-narrow' ARG times.

If ARG is negative, call `json-par-narrow' that times.  If called with
non-numeric prefix argument, it is converted to -1."
  (interactive "P\np")
  (when (or (consp arg) (eq arg '-))
    (setq arg -1))
  (setq arg (prefix-numeric-value arg))
  (if (< arg 0)
      (json-par-narrow (- arg))
    (dotimes (_ arg)
      (json-par--pop-restriction-1))))

(defun json-par--pop-restriction-1 ()
  "Undo `json-par-narrow'."
  (if json-par--restriction-history
      (let ((region (pop json-par--restriction-history)))
        (narrow-to-region (nth 0 region) (nth 1 region)))
    (widen)))

(defun json-par--narrow-1 (direction should-extend)
  "Narrow to the current value, or extend the narrowed area if narrowed.

If SHOULD-EXTEND is non-nil, extend the narrowed area for DIRECTION.

Otherwise, narrow to the current value or the key.  See `json-par-narrow' for
details."
  (let ((start (point-min))
        (end (point-max))
        region
        new-start
        new-end)
    (widen)
    (unless should-extend
      (setq json-par--restriction-history '()))
    (let ((history-delete-duplicates t))
      (add-to-history 'json-par--restriction-history (list start end)))
    (cond
     ((not should-extend)
      (setq region
            (if (region-active-p)
                (cons (min (point) (mark t)) (max (point) (mark t)))
              (json-par--region-of-current-value-or-key-to-mark (point))))
      (setq json-par--narrow-direction
            (if (< (point) (cdr region)) 1 -1)))

     ((< direction 0)
      (setq region (json-par--region-to-extend-backward end start)))

     (t
      (setq region (json-par--region-to-extend-forward start end))))
    (setq new-start (if should-extend
                        (min start (car region) (cdr region))
                      (min (car region) (cdr region))))
    (setq new-end (if should-extend
                      (max end (car region) (cdr region))
                    (max (car region) (cdr region))))
    (when (and (not (json-par--same-line-p new-start new-end))
               (save-excursion
                 (goto-char new-start)
                 (skip-chars-backward "\s\t")
                 (bolp)))
      (setq new-start
            (save-excursion
              (goto-char new-start)
              (line-beginning-position))))
    (narrow-to-region new-start new-end)))


(provide 'json-par-mark-narrow)

;;; json-par-mark-narrow.el ends here
