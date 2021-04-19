;;; json-par-mark-narrow.el --- Marking/narrowing in JSON Par mode -*- lexical-binding: t -*-

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

;; Functions for marking or narrowing to things in JSON Par mode.

;;; Code:

(require 'json-par-lexer)
(require 'json-par-motion)

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
    (eq last-command 'json-par-mark-more)))
  (when (consp arg)
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

If ARG is negative, call `json-par-mark-more' that times.  If called with
non-numeric prefix argument, it is converted to -1."
  (interactive "P")
  (when (consp arg)
    (setq arg -1))
  (setq arg (prefix-numeric-value arg))
  (if (< arg 0)
      (json-par-mark-more (- arg))
    (dotimes (_ arg)
      (json-par--pop-region-1))))

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
                        (json-par-beginning-of-member)
                        (point)))
          (setq end (save-excursion
                      (json-par-end-of-member)
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
                      (json-par-end-of-member)
                      (json-par-backward-token-or-list)
                      (point)))
        (setq end
              (save-excursion
                (json-par-end-of-member)
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

- If one or more whole members including commas are marked, return the region of
  the preceding member and comma.

- Otherwise, return the region of the preceding token or object/array."
  (save-excursion
    (goto-char point)
    (let (string-like-beginning-position
          current-atom
          previous-token
          point-is-after-comma
          mark-is-before-comma
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
        (setq point-is-after-comma
              (save-excursion
                (goto-char point)
                (json-par-token-comma-p (json-par-backward-token))))
        (setq mark-is-before-comma
              (save-excursion
                (json-par-token-comma-p (json-par-forward-token))))
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
                   (json-par-up-backward)
                   (< (json-par-token-start previous-token) (point)))
                 (and (save-excursion
                        (goto-char point)
                        (json-par-end-of-member)
                        (json-par--forward-spaces)
                        (skip-chars-backward "\s\t\n")
                        (< point (point)))
                      (save-excursion
                        (goto-char point)
                        (json-par-beginning-of-member)
                        (json-par--backward-spaces)
                        (skip-chars-forward "\s\t\n")
                        (< (point) point)))))
        (setq start (point))
        (setq end (save-excursion
                    (json-par-end-of-member)
                    (while (< (point) point)
                      (json-par-forward-member)
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
                      (json-par-beginning-of-member)
                      (point)))
        (setq end (save-excursion
                    (json-par-end-of-member)
                    (point))))

       ;; One or more member is marked.
       ((or (and mark-is-before-comma (not point-is-after-comma))
            (and mark-is-after-comma point-is-after-comma))
        (setq end (point))
        (json-par-backward-member)
        (setq start (point)))

       ;; Otherwise.
       (t
        (setq start (json-par-token-start previous-token))
        (setq end (json-par-token-end previous-token))))
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

- If one or more whole members including commas are marked, return the region of
  the following member and comma.

- Otherwise, return the region of the following token or object/array."
  (save-excursion
    (goto-char point)
    (let* (string-like-beginning-position
           string-like-end-position
           current-atom
           next-token
           point-is-before-comma
           mark-is-before-comma
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
        (setq next-token
              (save-excursion (json-par-forward-token-or-list-or-comment)))
        (setq point-is-before-comma
              (save-excursion
                (goto-char point)
                (json-par-token-comma-p (json-par-forward-token))))
        (setq mark-is-before-comma
              (save-excursion
                (json-par-token-comma-p (json-par-forward-token))))
        (setq mark-is-after-comma
              (save-excursion
                (json-par-token-comma-p (json-par-backward-token)))))
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
                (json-par-up-forward)
                (< (point) (json-par-token-end next-token)))
              (and (save-excursion
                     (goto-char point)
                     (json-par-beginning-of-member)
                     (json-par--backward-spaces)
                     (skip-chars-forward "\s\t\n")
                     (< (point) point))
                   (save-excursion
                     (goto-char point)
                     (json-par-end-of-member)
                     (json-par--forward-spaces)
                     (skip-chars-backward "\s\t\n")
                     (< point (point))))))
        (setq end (point))
        (setq start (save-excursion
                      (json-par-beginning-of-member)
                      (while (< point (point))
                        (json-par-backward-member)
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
                      (json-par-beginning-of-member)
                      (point)))
        (setq end (save-excursion
                    (json-par-end-of-member)
                    (point))))

       ;; Member is marked.
       ((or (and mark-is-after-comma (not point-is-before-comma))
            (and mark-is-before-comma point-is-before-comma))
        (setq start (point))
        (json-par-forward-member)
        (setq end (point)))

       ;; Otherwise.
       (t
        (setq start (json-par-token-start next-token))
        (setq end (json-par-token-end next-token))))
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
                  (json-par-up-backward)
                  (skip-chars-forward "\s\t\n")
                  (setq token (save-excursion (json-par-forward-token)))
                  (when (json-par-token-open-bracket-p token)
                    (goto-char (json-par-token-end token))
                    (skip-chars-forward "\s\t\n"))
                  (point)))
         (end (save-excursion
                (json-par-up-forward)
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
                      (json-par-up-backward)
                      (point)))
        (setq end (save-excursion
                    (goto-char end)
                    (json-par-up-forward)
                    (point)))))
    (cons start end)))


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
  (when (consp arg)
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
  (when (consp arg)
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
