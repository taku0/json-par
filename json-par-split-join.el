;;; json-par-split-join.el --- Splitting/joining in JSON Par mode -*- lexical-binding: t -*-

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

;; Functions for splitting or joining strings, object, or arrays.

;;; Code:

(require 'json-par-lexer)
(require 'json-par-motion)
(require 'json-par-insert)
(require 'json-par-indent)

(defun json-par-split ()
  "Split a string, object, or array at the point.

A comma and other required tokens are inserted.

- Splitting a string:

  If the point is on the middle of an escape sequence, split at the end of the
  sequence.

  The string can be a value or a key.

  Examples (`|' is the point):

    {
      \"aaa\": \"b|bb\"
    }
    ↓
    {
      \"aaa\": \"b\",
      \"\": \"|bb\"
    }

    {
      \"a|aa\": \"bbb\"
    }
    ↓
    {
      \"a\": ,
      \"|aa\": \"bbb\"
    }

- Splitting an object/array:

  It is assumed the object/array is a member of an array.

  If the point is at the middle of a member, go to the beginning of the member
  before splitting."
  (interactive)
  (let ((current-atom (json-par--current-atom))
        (string-like-beginning-position
         (json-par--string-like-beginning-position))
        (parent-token (json-par--parent-token)))
    (cond
     ((and (json-par-token-inside-p current-atom)
           (json-par-token-string-p current-atom))
      (json-par--split-string current-atom))

     (string-like-beginning-position
      (json-par--split-comment string-like-beginning-position))

     ((or (json-par-token-open-square-bracket-p parent-token)
          (json-par-token-open-curly-bracket-p parent-token))
      (json-par--split-array-or-object parent-token)))))

(defun json-par--split-string (string-token)
  "Split a string STRING-TOKEN at the point.

See `json-par-split' for details."
  (let ((escape-sequence (json-par--escape-sequence-under-point)))
    (when escape-sequence
      (goto-char (cdr escape-sequence))))
  (let* ((is-key (json-par--object-key-p string-token))
         (end-position (save-excursion
                         (json-par-end-of-member)
                         (point)))
         (end-marker
          (let ((marker (copy-marker end-position)))
            (set-marker-insertion-type marker t)
            marker))
         ;; String that contains a comma, spaces, and a empty key with a colon
         ;; (if the string is a object value) to be inserted between the two
         ;; split strings.
         ;; We build this before splitting the string to get better line break
         ;; behavior.
         (comma-and-spaces
          (save-excursion
            (goto-char end-position)
            (json-par-insert-comma)
            (unless is-key
              (json-par--insert-value "0" t)
              (delete-char -1))
            ;; `json-par-insert-comma' may insert extra spaces after the point.
            ;; Delete it.
            (delete-region (point) (json-par--free-marker end-marker))
            (delete-and-extract-region end-position (point)))))
    (insert-char ?\")
    (when is-key
      (insert ": "))
    (insert comma-and-spaces)
    (save-excursion
      (insert-char ?\"))))

(defun json-par--escape-sequence-under-point ()
  "Return region of an escape sequence under the point.

Return a cons of the start and end positions.

If the point is not on an escape sequence, return nil."
  (cond
   ((and (json-par--escaped-p) (not (eq (char-after) ?u)))
    (cons (1- (point))
          (1+ (point))))

   ((save-excursion
      (backward-char 5)
      (looking-at ".\\{,4\\}\\(\\\\u[0-9a-fA-F]\\{,4\\}\\)"))
    (cons (match-beginning 1)
          (match-end 1)))))

(defun json-par--split-comment (beginning-position)
  "Split a comment at BEGINNING-POSITION."
  (let* ((pos (point))
         (end-position
          (save-excursion
            (goto-char beginning-position)
            (forward-comment 1)
            (point)))
         (single-line
          (save-excursion
            (goto-char beginning-position)
            (looking-at "//")))
         (prefix-and-suffix
          (if single-line
              (json-par--prefix-and-suffix-of-single-line-comment
               beginning-position
               end-position
               pos)
            (json-par--prefix-and-suffix-of-multiline-comment
             beginning-position
             end-position
             pos)))
         (prefix (car prefix-and-suffix))
         (suffix (cdr prefix-and-suffix))
         before-prefix-position)
    (when (= (point) (1+ beginning-position))
      (forward-char))
    (when (and (not single-line)
               (= (point) (1- end-position)))
      (backward-char))
    (insert suffix)
    (newline)
    (setq before-prefix-position (point))
    (insert prefix)
    (save-excursion
      (goto-char before-prefix-position)
      (json-par-indent-line))))

(defun json-par--prefix-and-suffix-of-multiline-comment
    (beginning-position end-position &optional point)
  "Return the prefix and suffix string of a multiline comment.

The comment starts at BEGINNING-POSITION and end at END-POSITION.

Return a cons of strings.

A prefix is the first slash, following asterisks, and spaces including line
breaks.  A suffix is the last slash, preceding asterisks, and spaces including
line breaks.

If the comment contains only spaces, those spaces are part of the prefix rather
than the suffix.  If the comment contains only asterisks, those asterisks are
part of the prefix except the last asterisk.

If POINT is given, neither the prefix nor the suffix go beyond POINT."
  (let* ((prefix-end-position
          (save-excursion
            (goto-char beginning-position)
            (looking-at "/[*]+[\s\t\n*]*")
            (max
             (+ beginning-position 2)
             (min
              (match-end 0)
              (- end-position 2)
              (or point (point-max))))))
         (prefix (buffer-substring beginning-position prefix-end-position))
         (suffix (buffer-substring
                  (save-excursion
                    (goto-char end-position)
                    (backward-char)
                    (skip-chars-backward "*\s\t\n")
                    (max
                     (point)
                     prefix-end-position
                     (or point (point-min))))
                  end-position)))
    (cons prefix suffix)))

(defun json-par--prefix-and-suffix-of-single-line-comment
    (beginning-position end-position &optional point)
  "Return the prefix and suffix string of a single line comment.

The comment starts at BEGINNING-POSITION and end at END-POSITION.

Return a cons of strings.

A prefix is the starting slashes and following spaces.  A suffix is always an
empty string.

If POINT is given, the prefix doesn't go beyond POINT."
  (ignore end-position)
  (let ((prefix
         (buffer-substring
          beginning-position
          (save-excursion
            (goto-char beginning-position)
            (looking-at "/+[\s\t]*")
            (max (+ beginning-position 2)
                 (min (match-end 0) (or point (point-max)))))))
        (suffix ""))
    (cons prefix suffix)))

(defun json-par--split-array-or-object (parent-token)
  "Split an array/object at point.

The array/object start at PARENT-TOKEN.

See `json-par-split' for details."
  (let* ((position-in-member (json-par--position-in-member))
         (parent-is-array (json-par-token-open-square-bracket-p parent-token))
         (open-bracket (if parent-is-array ?\[ ?\{))
         (close-bracket (if parent-is-array ?\] ?\}))
         (insert-newline-around-bracket
          (save-excursion
            (goto-char (json-par-token-end parent-token))
            (<= (line-end-position)
                (progn
                  (skip-chars-forward "\s\t\n")
                  (point)))))
         (whole-list-token
          (save-excursion
            (goto-char (json-par-token-start parent-token))
            (json-par-forward-token-or-list)))
         (end-position
          (if (json-par-token-matching-brackets-p whole-list-token)
              (json-par-token-end whole-list-token)
            (point-max)))
         (end-marker
          (let ((marker (copy-marker end-position)))
            (set-marker-insertion-type marker t)
            marker))
         ;; String that contains a comma, spaces, and a empty key with a colon
         ;; (if the array/object is a object value) to be inserted between the
         ;; two split array/object.
         ;; We build this before splitting the array/object to get better line
         ;; break behavior.
         (comma-and-spaces
          (save-excursion
            (goto-char end-position)
            (json-par-insert-comma)
            (json-par--insert-value "0" t)
            (delete-char -1)
            ;; `json-par-insert-comma' may insert extra spaces after the point.
            ;; Delete it.
            (delete-region (point) (json-par--free-marker end-marker))
            (delete-and-extract-region end-position (point)))))
    (json-par--out-atom)
    (unless (or (eq position-in-member 'after-member)
                (eq position-in-member 'before-member))
      (json-par-beginning-of-member))
    (save-excursion
      (json-par--backward-spaces)
      (when (eq (char-before) ?\,)
        (delete-char -1)))
    (save-excursion
      (json-par--forward-spaces)
      (when (eq (char-after) ?\,)
        (delete-char 1)))
    (insert-char close-bracket)
    (save-excursion
      (backward-char)
      (if insert-newline-around-bracket
          (if (save-excursion
                (skip-chars-backward "\s\t")
                (bolp))
              (json-par-indent-line)
            (newline-and-indent))
        (unless (memq (char-before) '(?\s ?\t ?\n ?\[ nil))
          (insert-char ?\s))))
    (save-excursion
      (insert-char open-bracket)
      (if insert-newline-around-bracket
          (unless (save-excursion
                    (skip-chars-forward "\s\t")
                    (eolp))
            (newline-and-indent))
        (unless (memq (char-after) '(?\s ?\t ?\n ?\] nil))
          (insert-char ?\s))))
    (insert comma-and-spaces)))

(defun json-par-join ()
  "Join two strings or arrays/objects.

A comma between them, if any, is ignored.

When joining a string and other atoms (numbers or constants), other atoms are
converted to a string first.

When joining an array/object and an atom, the atom is appended/prepended to the
array/object.

When joining an array and an object, the object is appended/prepended to the
array.

If neither of tokens around the point is a string, array, nor object, do
nothing."
  (interactive)
  (json-par--out-comment)
  (json-par--out-atom)
  (let ((previous-token
         (save-excursion (json-par-backward-token-or-list-or-comment)))
        (next-token
         (save-excursion (json-par-forward-token-or-list-or-comment))))
    (unless (and (json-par-token-comment-p previous-token)
                 (json-par-token-comment-p next-token))
      (setq previous-token (save-excursion (json-par-backward-token)))
      (setq next-token (save-excursion (json-par-forward-token))))
    (if (json-par-token-comma-p previous-token)
        (save-excursion
          (goto-char (json-par-token-start previous-token))
          (setq previous-token (json-par-backward-token)))
      (when (json-par-token-comma-p next-token)
        (save-excursion
          (goto-char (json-par-token-end next-token))
          (setq next-token (json-par-forward-token)))))
    (cond
     ;; Beginning of buffer/end of buffer
     ((or (json-par-token-outside-of-buffer-p previous-token)
          (json-par-token-outside-of-buffer-p next-token))
      nil)

     ;; Between comments
     ((and (json-par-token-comment-p previous-token)
           (json-par-token-comment-p next-token))
      (json-par--join-comments previous-token next-token))

     ;; Between string and atom
     ((or (and (json-par-token-string-p previous-token)
               (json-par-token-atom-p next-token))
          (and (json-par-token-atom-p previous-token)
               (json-par-token-string-p next-token)))
      (json-par--join-strings previous-token next-token))

     ;; Between array and value
     ((or (json-par-token-close-square-bracket-p previous-token)
          (json-par-token-open-square-bracket-p next-token))
      (json-par--join-arrays previous-token next-token))

     ;; Between objects
     ((or (json-par-token-close-curly-bracket-p previous-token)
          (json-par-token-open-curly-bracket-p next-token))
      (json-par--join-objects previous-token next-token))

     ;; Otherwise
     (t nil))))

(defun json-par--join-comments (previous-token next-token)
  "Join comments PREVIOUS-TOKEN and NEXT-TOKEN."
  (if (and (json-par-token-single-line-comment-p previous-token)
           (json-par-token-single-line-comment-p next-token))
      (progn
        (goto-char (json-par-token-start next-token))
        (skip-chars-forward "/")
        (skip-chars-forward "\s\t")
        (delete-region (1- (json-par-token-end previous-token)) (point)))
    (let ((end-of-previous-token (json-par-token-end previous-token))
          beginning-of-next-comment-body)
      (when (json-par-token-single-line-comment-p next-token)
        (json-par--convert-single-line-comment-to-multiline-comment
         next-token
         (json-par--prefix-and-suffix-of-multiline-comment
          (json-par-token-start previous-token)
          (json-par-token-end previous-token))))
      (goto-char (json-par-token-start next-token))
      (forward-char)
      (skip-chars-forward "*\s\t\n")
      (when (and (eq (char-before) ?*) (eq (char-after) ?/))
        (backward-char))
      (setq beginning-of-next-comment-body (point-marker))
      (when (json-par-token-single-line-comment-p previous-token)
        (setq end-of-previous-token
              (json-par--convert-single-line-comment-to-multiline-comment
               previous-token
               (json-par--prefix-and-suffix-of-multiline-comment
                (json-par-token-start next-token)
                (json-par-token-end next-token)))))
      (goto-char end-of-previous-token)
      (backward-char)
      (skip-chars-backward "*")
      (skip-chars-backward "\s\t\n")
      (when (and (eq (char-before) ?/) (eq (char-after) ?*))
        (skip-chars-forward "*")
        (backward-char))
      (delete-region
       (point)
       (json-par--free-marker beginning-of-next-comment-body)))))

(defun json-par--convert-single-line-comment-to-multiline-comment
    (token prefix-and-suffix)
  "Convert a single line comment TOKEN with multiline comment.

PREFIX-AND-SUFFIX is a pair of prefix string and suffix string, which is
returned from `json-par--prefix-and-suffix-of-multiline-comment'.

Return the end position of the converted comment."
  (save-excursion
    (goto-char (json-par-token-start token))
    (looking-at "/*")
    (replace-match (car prefix-and-suffix))
    (end-of-line)
    (insert (cdr prefix-and-suffix))
    (point)))

(defun json-par--join-strings (previous-token next-token)
  "Join strings PREVIOUS-TOKEN and NEXT-TOKEN.

See `json-par-join' for details."
  (setq next-token
        (json-par--delete-comma-between-tokens previous-token next-token))
  (setq previous-token
        (json-par--move-comments-between-tokens previous-token next-token))
  (goto-char (json-par-token-end previous-token))
  (unless (json-par-token-string-p previous-token)
    (json-par-stringify-region (json-par-token-start previous-token)
                               (json-par-token-end previous-token))
    (setq previous-token (save-excursion (json-par-backward-token)))
    (setq next-token (save-excursion (json-par-forward-token))))
  (unless (json-par-token-string-p next-token)
    (json-par-stringify-region (json-par-token-start next-token)
                               (json-par-token-end next-token))
    (setq next-token (save-excursion (json-par-forward-token))))
  (delete-region
   (1- (json-par-token-end previous-token))
   (1+ (json-par-token-start next-token))))

(defun json-par--join-arrays (previous-token next-token)
  "Join arrays PREVIOUS-TOKEN and NEXT-TOKEN.

See `json-par-join' for details."
  (setq next-token
        (json-par--delete-comma-between-tokens previous-token next-token))
  (goto-char (json-par-token-end previous-token))
  (setq previous-token
        (save-excursion
          (goto-char (json-par-token-end previous-token))
          (json-par-backward-token-or-list)))
  (setq next-token
        (save-excursion
          (goto-char (json-par-token-start next-token))
          (json-par-forward-token-or-list)))
  (setq previous-token
        (json-par--move-comments-between-tokens previous-token next-token))
  (unless (json-par-token-matching-square-brackets-p previous-token)
    (json-par--wrap-region-with-brackets
     "["
     "]"
     (json-par-token-start previous-token)
     (json-par-token-end previous-token)
     'one-line)
    (setq previous-token (save-excursion (json-par-backward-token-or-list)))
    (setq next-token (save-excursion (json-par-forward-token-or-list))))
  (unless (json-par-token-matching-square-brackets-p next-token)
    (json-par--wrap-region-with-brackets
     "["
     "]"
     (json-par-token-start next-token)
     (json-par-token-end next-token)
     'one-line)
    (setq next-token (save-excursion (json-par-forward-token-or-list))))
  (goto-char (json-par-token-start next-token))
  (forward-char)
  (skip-chars-forward "\s\t\n")
  (unless (or (json-par-token-close-square-bracket-p
               (save-excursion (json-par-forward-token)))
              (json-par-token-open-square-bracket-p
               (save-excursion
                 (goto-char (json-par-token-end previous-token))
                 (backward-char)
                 (json-par-backward-token))))
    (json-par-insert-comma))
  (delete-region
   (save-excursion
     (goto-char (json-par-token-end previous-token))
     (backward-char)
     (skip-chars-backward "\s\t\n")
     (point))
   (point))
  (save-excursion
    (goto-char (json-par-token-start previous-token))
    (setq previous-token (json-par-forward-token-or-list))
    (json-par--fixup-spaces-at-start-and-end-of-list
     (json-par-token-start previous-token)
     (json-par-token-end previous-token)))
  (skip-chars-forward "\s\t\n")
  (when (eq (char-after) ?\])
    (skip-chars-backward "\s\t\n")))

(defun json-par--fixup-spaces-at-start-and-end-of-list (start end)
  "Insert spaces and/or line breaks at start/end of an array/object.

The array/object starts at START and ends at END.

If spaces follows the open bracket but not precedes the close bracket, the
spaces are copied before the close bracket.  Likewise, if spaces precedes the
close bracket but not follows the open bracket, the spaces are copied after the
open bracket."
  (save-excursion
    (let ((preceding-spaces
           (save-excursion
             (goto-char start)
             (forward-char)
             (buffer-substring
              (point)
              (progn
                (skip-chars-forward "\s\t\n")
                (point)))))
          (following-spaces
           (save-excursion
             (goto-char end)
             (backward-char)
             (buffer-substring
              (save-excursion
                (skip-chars-backward "\s\t\n")
                (point))
              (point)))))
      (when (string= following-spaces "")
        (goto-char end)
        (backward-char)
        (insert preceding-spaces)
        (when (save-excursion
                (skip-chars-backward "\s\t")
                (bolp))
          (json-par-indent-line)))
      (when (string= preceding-spaces "")
        (goto-char start)
        (forward-char)
        (insert-before-markers following-spaces)
        (when (save-excursion
                (skip-chars-backward "\s\t")
                (bolp))
          (json-par-indent-line))))))

(defun json-par--join-objects (previous-token next-token)
  "Join objects PREVIOUS-TOKEN and NEXT-TOKEN.

See `json-par-join' for details."
  ;; FIXME: almost identical to `json-par--join-arrays'.
  (setq next-token
        (json-par--delete-comma-between-tokens previous-token next-token))
  (goto-char (json-par-token-end previous-token))
  (setq previous-token
        (save-excursion
          (goto-char (json-par-token-end previous-token))
          (json-par-backward-token-or-list)))
  (setq next-token
        (save-excursion
          (goto-char (json-par-token-start next-token))
          (json-par-forward-token-or-list)))
  (setq previous-token
        (json-par--move-comments-between-tokens previous-token next-token))
  (unless (json-par-token-matching-curly-brackets-p previous-token)
    (json-par--wrap-region-with-brackets
     "{"
     "}"
     (json-par-token-start previous-token)
     (json-par-token-end previous-token)
     'one-line)
    (json-par--prepend-empty-keys-to-values-if-missing
     (json-par-token-start previous-token))
    (setq previous-token (save-excursion (json-par-backward-token-or-list)))
    (setq next-token (save-excursion (json-par-forward-token-or-list))))
  (unless (json-par-token-matching-curly-brackets-p next-token)
    (json-par--wrap-region-with-brackets
     "{"
     "}"
     (json-par-token-start next-token)
     (json-par-token-end next-token)
     'one-line)
    (json-par--prepend-empty-keys-to-values-if-missing
     (json-par-token-start next-token))
    (setq next-token (save-excursion (json-par-forward-token-or-list))))
  (goto-char (json-par-token-start next-token))
  (forward-char)
  (skip-chars-forward "\s\t\n")
  (unless (or (json-par-token-close-curly-bracket-p
               (save-excursion (json-par-forward-token)))
              (json-par-token-open-curly-bracket-p
               (save-excursion
                 (goto-char (json-par-token-end previous-token))
                 (backward-char)
                 (json-par-backward-token))))
    (json-par-insert-comma))
  (delete-region
   (save-excursion
     (goto-char (json-par-token-end previous-token))
     (backward-char)
     (skip-chars-backward "\s\t\n")
     (point))
   (point))
  (save-excursion
    (goto-char (json-par-token-start previous-token))
    (setq previous-token (json-par-forward-token-or-list))
    (json-par--fixup-spaces-at-start-and-end-of-list
     (json-par-token-start previous-token)
     (json-par-token-end previous-token)))
  (skip-chars-forward "\s\t\n")
  (when (eq (char-after) ?})
    (skip-chars-backward "\s\t\n")))

(defun json-par--delete-comma-between-tokens (previous-token next-token)
  "Delete a comma between PREVIOUS-TOKEN and NEXT-TOKEN if any.

Return NEXT-TOKEN with new position."
  (save-excursion
    (goto-char (json-par-token-end previous-token))
    (let ((token (json-par-forward-token)))
      (if (and (json-par-token-comma-p token)
               (not (eq (json-par-token-start token)
                        (json-par-token-start next-token))))
          (progn
            (delete-region
             (json-par-token-start token)
             (json-par-token-end token))
            (json-par-forward-token))
        next-token))))

(defun json-par--move-comments-between-tokens (previous-token next-token)
  "Move comments between PREVIOUS-TOKEN and NEXT-TOKEN before PREVIOUS-TOKEN."
  (save-excursion
    (transpose-regions
     (save-excursion
       (skip-chars-forward "\s\t\n")
       (point))
     (json-par-token-start next-token)
     (json-par-token-start previous-token)
     (json-par-token-start previous-token))
    (goto-char (json-par-token-start next-token))
    (json-par-backward-token-or-list)))

(provide 'json-par-split-join)

;;; json-par-split-join.el ends here
