;;; json-par-fixup.el --- Fixing JSON in JSON Par mode -*- lexical-binding: t -*-

;; Copyright (C) 2023 taku0
;;
;; Author: taku0 <mxxouy6x3m_github@tatapa.org>
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

;;; Code:

(require 'json-par-lexer)
(require 'json-par-delete)
(require 'json-par-insert)

(defvar-local json-par--changed-region-start nil)
(defvar-local json-par--changed-region-end nil)
(defvar-local json-par--inhibit-modification-hooks nil)
(defvar-local json-par--inhibit-fixup-tick nil
  "Suppress fixing if this variable equal to `buffer-chars-modified-tick'.")

(defun json-par--reset-changed-region ()
  "Clear the markers of the changed region."
  (when json-par--changed-region-start
    (set-marker json-par--changed-region-start nil))
  (when json-par--changed-region-end
    (set-marker json-par--changed-region-end nil)))

(defun json-par--update-changed-region (start end _length)
  "Update the markers of the changed region.

Intended for `after-change-functions'.

See `after-change-functions' for START, END, and _LENGTH."
  (unless json-par--inhibit-modification-hooks
    (if json-par--changed-region-start
        (if (marker-position json-par--changed-region-start)
            (set-marker json-par--changed-region-start
                        (min json-par--changed-region-start
                             start))
          (set-marker json-par--changed-region-start start))
      (setq json-par--changed-region-start (copy-marker start)))
    (if json-par--changed-region-end
        (if (marker-position json-par--changed-region-end)
            (set-marker json-par--changed-region-end
                        (max json-par--changed-region-end
                             end))
          (set-marker json-par--changed-region-end end))
      (setq json-par--changed-region-end (copy-marker end)))))

(defun json-par--fixup-changed-region ()
  "Fixup changed region.

Remove redundant commas.
Insert missing commas.
Quote unquoted keys.
Insert empty keys if missing."
  (when (and (not (memq this-command '(undo undo-only undo-redo)))
             (not (eq json-par--inhibit-fixup-tick
                      (buffer-chars-modified-tick))))
    (json-par--clean-up-protection-markers)
    (when (and json-par--changed-region-start
               json-par--changed-region-end
               (marker-position json-par--changed-region-start)
               (marker-position json-par--changed-region-end))
      (json-par-fixup-region json-par--changed-region-start
                             json-par--changed-region-end)))
  (json-par--reset-changed-region))

(defun json-par--clean-up-protection-markers ()
  "Clean up protection markers if protected region contain some value."
  (let ((protection-markers json-par--protection-markers)
        (changed nil))
    (setq json-par--protection-markers
          (cl-remove-if-not
           (lambda (marker)
             (let (token valid)
               (setq valid
                     (and (or (null (char-before marker))
                              (memq (char-after marker) '(?\[ ?\( ?{ ?,)))
                          (save-excursion
                            (goto-char marker)
                            (when (memq (char-after marker) '(?\[ ?\( ?{ ?,))
                              (forward-char))
                            (setq token (json-par-forward-token))
                            (or (json-par-token-comma-p token)
                                (json-par-token-close-bracket-p token)
                                (json-par-token-outside-of-buffer-p token)))))
               (unless valid
                 (push (list
                        'apply
                        (lambda (position)
                          (move-marker marker position))
                        (marker-position marker))
                       buffer-undo-list)
                 (move-marker marker nil)
                 (setq changed t))
               valid))
           json-par--protection-markers))
    (when changed
      (json-par--record-protection-markers-to-undo-list protection-markers))))

(defun json-par-fixup-region (start end)
  "Fixup region (START END).

Remove redundant commas.
Insert missing commas.
Quote unquoted keys.
Insert empty keys if missing.
Remove spaces inside empty array/object.

Redundant commas are not deleted if the comma is inserted by
`json-par-insert-comma' and the point is on the empty member resulted from the
`json-par-insert-comma' invocation.

Regions between top-level values are not modified."
  (interactive "r")
  (save-match-data
    (save-excursion
      (goto-char start)
      (json-par--out-comment)
      (setq start (point))
      (json-par--out-atom)
      (when (/= start (point))
        (json-par-backward-token))
      (setq start (point)))
    (save-excursion
      (goto-char end)
      (json-par--out-comment)
      (json-par--out-atom)
      (setq end (point-marker)))
    (let* ((json-par--inhibit-modification-hooks t)
           following-token
           current-token
           preceding-token
           newline-after-preceding-colon
           newline-before-following-colon
           only-one-value-between-colons
           depth)
      (save-excursion
        (goto-char end)
        (setq following-token (json-par-forward-token))
        (goto-char end)
        (setq depth (nth 0 (syntax-ppss)))
        (while (progn
                 (setq current-token (json-par-backward-token))
                 (and
                  (<= start (json-par-token-start following-token))
                  (or
                   (not (json-par-token-outside-of-buffer-p following-token))
                   (not (json-par-token-outside-of-buffer-p current-token)))))
          (cond
           ;; Between top-level values.
           ((zerop depth)
            (setq following-token current-token))

           ;; Before an object key.
           ((json-par--object-key-p following-token t)
            ;; Case 1:
            ;; {
            ;;   "a": "b",
            ;;   "following-token": "b"
            ;; }
            ;; OK.
            ;;
            ;; Case 2:
            ;; {
            ;;   "following-token": "b"
            ;; }
            ;; OK.
            ;;
            ;; Case 3:
            ;; {
            ;;   "a": "current-token"
            ;;   "following-token": "b"
            ;; }
            ;; Insert a comma after the current token.
            ;;
            ;; Case 4:
            ;; {
            ;;   "a":
            ;;   "following-token": "b"
            ;; }
            ;; Insert a comma after the current token.
            ;;
            ;; Case 5:
            ;; {
            ;;   "a": "following-token"
            ;;   : "b"
            ;; }
            ;; Insert a comma and an empty key after the following token.
            ;;
            ;; Case 6:
            ;; {
            ;;   "a": "following-token": "b"
            ;; }
            ;; Insert a comma after the current token.
            (cond
             ;; Case 1 or 2.
             ((or (json-par-token-comma-p current-token)
                  (json-par-token-open-bracket-p current-token)
                  (json-par-token-outside-of-buffer-p current-token))
              (setq following-token current-token))

             ;; Case 3, 4, or 6.
             ((save-excursion
                (goto-char (json-par-token-end following-token))
                (json-par--forward-spaces t)
                (eq (char-after) ?:))
              (goto-char (json-par-token-end current-token))
              (json-par-insert-comma))

             ;; Case 5.
             (t
              (goto-char (json-par-token-end following-token))
              (json-par-insert-comma))))

           ;; Before a value.
           ((or (json-par-token-string-p following-token)
                (json-par-token-number-p following-token)
                (json-par-token-constant-p following-token)
                (json-par-token-other-p following-token)
                (json-par-token-open-bracket-p following-token))
            (if (or (json-par-token-string-p current-token)
                    (json-par-token-number-p current-token)
                    (json-par-token-constant-p current-token)
                    (json-par-token-close-bracket-p current-token)
                    (json-par-token-other-p current-token))
                ;; A comma is missing.  Insert it.
                (progn
                  (goto-char (json-par-token-end current-token))
                  (json-par-insert-comma))
              ;; Otherwise, go ahead.
              (setq following-token current-token)))

           ;; Before a colon.
           ((json-par-token-colon-p following-token)
            ;; Case 1:
            ;; {
            ;;   "a": "b",
            ;;   "current-token": "b"
            ;; }
            ;; OK.
            ;;
            ;; Case 2:
            ;; {
            ;;   "a": "b",
            ;;   : "b"
            ;; }
            ;; Insert an empty key before the following token.
            ;;
            ;; Case 3:
            ;; {
            ;;   "a":
            ;;   "current-token": "b"
            ;; }
            ;; This case will be handled by the next iteration.  Keep as is.
            ;;
            ;; Case 4:
            ;; {
            ;;   "a": "current-token"
            ;;   : "b"
            ;; }
            ;; Insert an empty key before the following token.  A comma will be
            ;; inserted in the next iteration.
            ;;
            ;; Case 5:
            ;; {
            ;;   "a": "current-token" : "b"
            ;; }
            ;; This case will be handled by next iteration.  Keep as is.
            ;;
            ;; Case 6:
            ;; {
            ;;   "a":
            ;;   : "b"
            ;; }
            ;; Insert an empty key before the following token.  A comma will be
            ;; inserted in the next iteration.
            ;;
            ;;
            ;; For the folloing cases, assuming the key is not quoted.  Quote
            ;; tokens before the colon.  The key must not contain line breaks,
            ;; colons, commas, or double quotes.
            ;;
            ;; Case 7:
            ;; {
            ;;   "a": [1] [2]
            ;;   [3] [4]: "b"
            ;; }
            ;; Quote "[3] [4]".  A comma will be inserted in the next iteration.
            ;;
            ;; Case 8:
            ;; {
            ;;   "a": [1] [2], [3] [4]: "b"
            ;; }
            ;; Quote "[3] [4]".
            ;;
            ;; Case 9:
            ;; {
            ;;   "a": 1, [2
            ;;   3] 4: "b"
            ;; }
            ;; Quote "4"
            ;;
            ;; Case 10:
            ;; {
            ;;   "a": 1, [2
            ;;   3 4]: "b"
            ;; }
            ;; Insert an empty key before the following token.
            ;;
            ;;
            ;; If tokens can be the key of the current member or the value of
            ;; the previous member, check newlines around the tokens.
            ;;
            ;; Case 11:
            ;; {
            ;;   "a": 1 2
            ;;   : "b"
            ;; }
            ;; Insert an empty key before the following token.
            ;;
            ;; Case 12:
            ;; {
            ;;   "a":
            ;;   1 2: "b"
            ;; }
            ;; Quote "1 2".
            ;;
            ;; Case 13:
            ;; {
            ;;   "a":
            ;;   1
            ;;   2
            ;;   : "b"
            ;; }
            ;; Quote "2".
            ;;
            ;; Case 14:
            ;; {
            ;;   "a":
            ;;   1
            ;;   : "b"
            ;; }
            ;; Insert an empty key before the following token.
            ;;
            ;; Case 15:
            ;; {
            ;;   "a": 1 : "b"
            ;; }
            ;; Insert a comma after the current.
            ;;
            ;; Case 16:
            ;; {
            ;;   "a": 1 2 3 : "b"
            ;; }
            ;; Quote "2 3".
            (cond
             ;; Case 1, 3, or 5.
             ((and (json-par-token-string-p current-token)
                   (or (json-par--same-line-p
                        (json-par-token-end current-token)
                        (json-par-token-start following-token))
                       (save-excursion
                         (json-par--backward-spaces)
                         (memq (char-before) '(nil ?\[ ?\( ?{ ?,)))))
              (setq following-token current-token))

             ;; Case 2.
             ((or (json-par-token-comma-p current-token)
                  (json-par-token-open-bracket-p current-token)
                  (json-par-token-outside-of-buffer-p current-token))
              (goto-char (json-par-token-start following-token))
              (json-par--insert-key "\"\"" current-token following-token))

             ;; Case 4 or 6.
             ((or (json-par-token-string-p current-token)
                  (json-par-token-colon-p current-token))
              (goto-char (json-par-token-start following-token))
              (json-par--insert-key "\"\"" current-token following-token))

             ;; Other cases.
             (t
              (goto-char (json-par-token-end current-token))
              (setq preceding-token (json-par-backward-token-or-list))
              (while (and (json-par--same-line-p
                           (point)
                           (json-par-token-start current-token))
                          (not (json-par-token-string-p preceding-token))
                          (not (json-par-token-colon-p preceding-token))
                          (not (json-par-token-comma-p preceding-token))
                          (not (json-par-token-open-bracket-p preceding-token))
                          (not (json-par-token-outside-of-buffer-p
                                preceding-token)))
                (setq preceding-token (json-par-backward-token-or-list)))
              (cond
               ((json-par-token-outside-of-buffer-p preceding-token)
                t)
               ((json-par-token-open-bracket-p preceding-token)
                (json-par-forward-token))
               (t
                (json-par-forward-token-or-list)))
              (json-par--forward-spaces)
              (cond
               ;; No tokens to quote.  Insert an empty key.
               ((eq (char-after) ?:)
                (json-par--insert-key "\"\"" current-token following-token))

               ;; Candidate key is between colons.
               ((save-excursion
                  (json-par--backward-spaces)
                  (eq (char-before) ?:))
                (setq newline-after-preceding-colon
                      (save-excursion
                        (json-par--backward-spaces t)
                        (not (eq (char-before) ?:))))
                (setq newline-before-following-colon
                      (save-excursion
                        (goto-char (json-par-token-start following-token))
                        (json-par--backward-spaces t)
                        (bolp)))
                (setq only-one-value-between-colons
                      (save-excursion
                        (json-par-forward-token-or-list)
                        (json-par--forward-spaces)
                        (eq (char-after) ?:)))
                (cond
                 ((and (not newline-after-preceding-colon)
                       newline-before-following-colon)
                  (json-par--insert-key "\"\"" current-token following-token))
                 ((and newline-after-preceding-colon
                       (not newline-before-following-colon))
                  (json-par-stringify-region
                   (point)
                   (json-par-token-end current-token))
                  (setq following-token
                        (save-excursion (json-par-forward-token))))
                 (only-one-value-between-colons
                  (json-par--insert-key "\"\"" current-token following-token))

                 (t
                  (json-par-forward-token-or-list)
                  (json-par--forward-spaces)
                  (json-par-stringify-region
                   (point)
                   (json-par-token-end current-token))
                  (setq following-token
                        (save-excursion (json-par-forward-token))))))

               ;; Otherwise, quote the tokens.
               (t
                (json-par-stringify-region
                 (point)
                 (json-par-token-end current-token))
                (setq following-token
                      (save-excursion (json-par-forward-token))))))))

           ;; Before a close bracket or the end of the buffer.
           ((or (json-par-token-close-bracket-p following-token)
                (json-par-token-outside-of-buffer-p following-token))
            (cond
             ;; The current token is a redundant comma.  Remove it.
             ((json-par-token-comma-p current-token)
              (json-par--delete-redundant-comma current-token))

             ;; Empty array/object.  Remove spaces inside it.
             ((and (json-par-token-open-bracket-p current-token)
                   (json-par-token-close-bracket-p following-token)
                   (save-excursion
                     (goto-char (json-par-token-end current-token))
                     (skip-chars-forward "\s\t")
                     (eq (point) (json-par-token-start following-token))))
              (delete-region (json-par-token-end current-token)
                             (json-par-token-start following-token))
              (setq following-token current-token))

             ;; Otherwise, go ahead.
             (t
              (setq following-token current-token))))

           ;; Before a comma
           ((json-par-token-comma-p following-token)
            (if (or (json-par-token-comma-p current-token)
                    (json-par-token-open-bracket-p current-token)
                    (json-par-token-outside-of-buffer-p current-token))
                ;; The following token is a redundant comma.  Remove it.
                (progn
                  (json-par--delete-redundant-comma following-token)
                  (setq following-token current-token))
              ;; Otherwise, go ahead.
              (setq following-token current-token)))

           (t
            (error "Unkown token type: %s"
                   (json-par-token-type following-token))))
          (cond
           ((json-par-token-open-bracket-p current-token)
            (setq depth (1- depth)))
           ((json-par-token-close-bracket-p current-token)
            (setq depth (1+ depth))))))
      (json-par--fixup-region-insert-newlines-if-needed start end)
      (json-par--free-marker end))))

(defun json-par--delete-redundant-comma (comma)
  "Delete redundant COMMA token.

Spaces and line breaks around the comma is adjusted."
  (let* ((next-token (save-excursion
                       (goto-char (json-par-token-end comma))
                       (json-par-forward-token)))
         (previous-token (save-excursion
                           (goto-char (json-par-token-start comma))
                           (json-par-backward-token)))
         (is-last-member
          (or (json-par-token-close-bracket-p next-token)
              (json-par-token-outside-of-buffer-p next-token)))
         (is-first-member
          (or (json-par-token-open-bracket-p previous-token)
              (json-par-token-outside-of-buffer-p previous-token)))
         (is-first-member-of-line
          (json-par--beginning-of-line-or-list-p (json-par-token-start comma)))
         (is-last-member-of-line
          (json-par--end-of-line-or-list-p (json-par-token-end comma)))
         (is-protected (cl-some
                        (lambda (marker)
                          (or (= marker (json-par-token-start comma))
                              (save-excursion
                                (goto-char marker)
                                (forward-char)
                                (json-par--forward-spaces)
                                (= (point) (json-par-token-start comma)))))
                        json-par--protection-markers))
         start
         end)
    (cond
     ;; The comma is protected.
     (is-protected
      nil)

     ;; The comma is the only content of an array/object.
     ((and is-first-member is-last-member)
      (delete-region (json-par-token-end previous-token)
                     (json-par-token-start next-token)))

     ;; The comma is at the end of an array/object.
     (is-last-member
      (setq end (save-excursion
                  (goto-char (json-par-token-start next-token))
                  (json-par--backward-spaces t)
                  (if (bolp)
                      (1- (point))
                    (goto-char (json-par-token-start next-token))
                    (when (memq (char-before) '(?\s ?\t))
                      (backward-char))
                    (point))))
      (delete-region (json-par-token-end previous-token) end))

     ;; The comma is at the start of an array/object.
     (is-first-member
      (setq start (save-excursion
                    (goto-char (json-par-token-end previous-token))
                    (json-par--forward-spaces t)
                    (if (or (eolp)
                            (and (eq (char-after) ?/)
                                 (eq (char-after (1+ (point))) ?/)))
                        (progn
                          (forward-line)
                          (json-par--forward-spaces t)
                          (point))
                      (goto-char (json-par-token-end previous-token))
                      (when (memq (char-after) '(?\s ?\t))
                        (forward-char))
                      (point))))
      (delete-region start (json-par-token-start next-token)))

     ;; The comma is the only content of a line.
     ((and is-first-member-of-line is-last-member-of-line)
      (delete-region (json-par-token-start comma)
                     (json-par-token-start next-token)))

     ;; The comma is at the end of a line
     (is-last-member-of-line
      (delete-region (json-par-token-end previous-token)
                     (json-par-token-end comma)))

     ;; The comma is at the start of a line
     (is-first-member-of-line
      (delete-region (json-par-token-start comma)
                     (json-par-token-start next-token)))

     ;; Otherwise
     (t
      (delete-region (json-par-token-start comma)
                     (json-par-token-start next-token))))))

(defun json-par--fixup-region-insert-newlines-if-needed (start end)
  "Insert newlines before/after the modified region (START END) if needed.

Insert newlines if and only if the containing array/object have line breaks
after each members around the region."
  (save-excursion
    (goto-char start)
    (json-par--forward-spaces)
    (when (and (eq (char-after) ?,)
               (< start end))
      (forward-char)
      (json-par--forward-spaces)
      (setq start (point))))
  (unless (eq (save-excursion
                (goto-char start)
                (json-par--backward-spaces)
                (point))
              (save-excursion
                (goto-char end)
                (json-par--backward-spaces)
                (point)))
    (let (parent-token-of-start
          parent-token-of-end
          multiple-members-on-current-line
          multiple-members-on-same-line-before-point
          multiple-members-on-same-line-after-point
          insert-newline-before-start
          insert-newline-after-end)
      (save-excursion
        (goto-char start)
        (setq parent-token-of-start (json-par--parent-token))
        (goto-char end)
        (setq parent-token-of-end (json-par--parent-token))
        (setq multiple-members-on-current-line
              (and
               (json-par--same-line-p start end)
               (progn
                 (goto-char start)
                 (json-par--backward-spaces t)
                 (not (bolp)))
               (progn
                 (goto-char end)
                 (json-par--forward-spaces t)
                 (not (eolp)))))
        (setq multiple-members-on-same-line-before-point
              (progn
                (goto-char start)
                (json-par--backward-spaces)
                (when (eq (char-before) ?,)
                  (backward-char))
                (json-par--multiple-members-on-same-line-before-point-p 3)))
        (setq multiple-members-on-same-line-after-point
              (progn
                (goto-char end)
                (json-par--forward-spaces)
                (when (json-par--same-line-p (point) end)
                  (or (zerop (json-par-forward-member))
                      (progn
                        (json-par-end-of-member)
                        (json-par--forward-spaces))))
                (and (not (memq (char-after) '(?\] ?\) ?} nil)))
                     (json-par--multiple-members-on-same-line-after-point-p
                      3))))
        (if (eq (json-par-token-start parent-token-of-start)
                (json-par-token-start parent-token-of-end))
            (progn
              (setq insert-newline-before-start
                    (and (not (json-par--same-line-p
                               (json-par-token-start parent-token-of-start)
                               start))
                         (not multiple-members-on-current-line)
                         (not multiple-members-on-same-line-before-point)
                         (not multiple-members-on-same-line-after-point)))
              (setq insert-newline-after-end insert-newline-before-start))
          (setq insert-newline-before-start
                (and (not (json-par--same-line-p
                           (json-par-token-start parent-token-of-start)
                           start))
                     (not multiple-members-on-current-line)
                     (not multiple-members-on-same-line-before-point)))
          (setq insert-newline-after-end
                (and (progn
                       ;; Current line doesn't contain the closing bracket.
                       ;; Note that `json-par-up-forward' is much slower than
                       ;; `json-par-up-backward' or `json-par--parent-token'.
                       (end-of-line)
                       (eq (json-par-token-start (json-par--parent-token))
                           (json-par-token-start parent-token-of-end)))
                     (not multiple-members-on-current-line)
                     (not multiple-members-on-same-line-after-point))))
        (when insert-newline-after-end
          (goto-char end)
          (json-par--backward-spaces t)
          (when (eq (char-before) ?,)
            (json-par--forward-spaces t)
            (when (eq (char-after) ?,)
              (forward-char)
              (json-par--forward-spaces t))
            (unless (eolp)
              (newline-and-indent))))
        (when insert-newline-before-start
          (goto-char start)
          (json-par--forward-spaces t)
          (unless (or (eolp)
                      (and (eq (char-after) ?/)
                           (eq (char-after (1+ (point))) ?/)))
            (json-par--backward-spaces t)
            (when (eq (char-before) ?,)
              (newline-and-indent))))))))

;; TODO
(defun json-par--add-fixup-advice (f)
  "Add `json-par--fixup-advice' around F."
  (advice-add f :around #'json-par--fixup-advice))

(defun json-par--fixup-advice (oldfun &rest args)
  "Record changed region and fixup the region after calling OLDFUN.

This advice is no-op if `json-par--update-changed-region' is installed as a
`after-change-functions'.

ARGS are passed to the OLDFUN."
  (let ((hook-installed
         (memq #'json-par--update-changed-region after-change-functions)))
    (unless hook-installed
      (add-hook 'after-change-functions
                #'json-par--update-changed-region
                nil
                t))
    (unwind-protect
        (prog1 (apply oldfun args)
          (unless hook-installed
            (json-par--fixup-changed-region)))
      (unless hook-installed
        (remove-hook
         'after-change-functions #'json-par--update-changed-region t)))))

(provide 'json-par-fixup)

;;; json-par-fixup.el ends here
