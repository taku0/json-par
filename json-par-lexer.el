;;; json-par-lexer.el --- Lexer for JSON Par mode -*- lexical-binding: t -*-

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

;; Routines for JSON tokens.

;;; Code:

(require 'json)

;; Token is a tuple consists of:
;;
;; - Token type
;; - Start position (inclusive)
;; - End position (exclusive)
;;
;; Token types is one of the following symbols:
;;
;; - string
;; - number
;; - constant (true, false, null, and other symbols matches "[-a-zA-Z_]+")
;; - [
;; - ]
;; - {
;; - }
;; - (
;; - )
;; - ,
;; - :
;; - other (other characters)
;; - outside-of-buffer (the beginning/end of the buffer)
;;
;; `json-par-forward-token-or-list' and `json-par-backward-token-or-list' also
;; return tokens with the following types, which represents balancing brackets:
;;
;; - ()
;; - {}
;; - []
;;
;; `json-par-forward-token-or-list-or-comment' and
;; `json-par-backward-token-or-list-or-comment' also return tokens with the
;; following type.
;;
;; - comment

(defun json-par-token (type start end)
  "Construct and return a token.

TYPE is the type of the token such as `string' or {.
START is the start position of the token.
END is the point after the token."
  (list type start end))

(defun json-par-token-type (token)
  "Return the type of TOKEN."
  (nth 0 token))

(defun json-par-token-start (token)
  "Return the start position of TOKEN."
  (nth 1 token))

(defun json-par-token-end (token)
  "Return the end position of TOKEN."
  (nth 2 token))

(defun json-par-token-text (token)
  "Return the text of TOKEN with properties.

Assuming the token is still where it is at the time of creation."
  (buffer-substring (json-par-token-start token)
                    (json-par-token-end token)))

(defun json-par-token-text-no-properties (token)
  "Return the text of TOKEN without properties..

Assuming the token is still where it is at the time of creation."
  (buffer-substring-no-properties (json-par-token-start token)
                                  (json-par-token-end token)))

(defun json-par-token-inside-p (token &optional pos)
  "Return non-nil if POS is strictly inside TOKEN.

Return nil otherwise, including when the POS is just before or after the TOKEN.

If POS is nil, the point is used."
  (and
   token
   (< (json-par-token-start token)
      (or pos (point))
      (json-par-token-end token))))

(defun json-par-token-length (token)
  "Return the length of TOKEN."
  (- (json-par-token-end token) (json-par-token-start token)))

(defun json-par-token-empty-string-p (token)
  "Return the end position of TOKEN.

Return nil otherwise."
  (and (json-par-token-string-p token)
       (= (json-par-token-length token) 2)))

(defun json-par-token-string-p (token)
  "Return non-nil if TOKEN is a string.

Return nil otherwise."
  (eq (json-par-token-type token) 'string))

(defun json-par-token-number-p (token)
  "Return non-nil if TOKEN is a number.

Return nil otherwise."
  (eq (json-par-token-type token) 'number))

(defun json-par-token-constant-p (token)
  "Return non-nil if TOKEN is a constant.

Return nil otherwise."
  (eq (json-par-token-type token) 'constant))

(defun json-par-token-other-p (token)
  "Return non-nil if TOKEN is an unknown token.

Return nil otherwise."
  (eq (json-par-token-type token) 'other))

(defun json-par-token-comma-p (token)
  "Return non-nil if TOKEN is a comma.

Return nil otherwise."
  (eq (json-par-token-type token) '\,))

(defun json-par-token-colon-p (token)
  "Return non-nil if TOKEN is a colon.

Return nil otherwise."
  (eq (json-par-token-type token) ':))

(defun json-par-token-outside-of-buffer-p (token)
  "Return non-nil if TOKEN is an outside of buffer.

Return nil otherwise."
  (eq (json-par-token-type token) 'outside-of-buffer))

(defun json-par-token-open-curly-bracket-p (token)
  "Return non-nil if TOKEN is an open curly bracket.

Return nil otherwise."
  (eq (json-par-token-type token) '{))

(defun json-par-token-close-curly-bracket-p (token)
  "Return non-nil if TOKEN is a close curly bracket.

Return nil otherwise."
  (eq (json-par-token-type token) '}))

(defun json-par-token-matching-curly-brackets-p (token)
  "Return non-nil if TOKEN is a balanced curly brackets.

Return nil otherwise."
  (eq (json-par-token-type token) '{}))

(defun json-par-token-open-square-bracket-p (token)
  "Return non-nil if TOKEN is an open square bracket.

Return nil otherwise."
  (eq (json-par-token-type token) '\[))

(defun json-par-token-close-square-bracket-p (token)
  "Return non-nil if TOKEN is a close square bracket.

Return nil otherwise."
  (eq (json-par-token-type token) '\]))

(defun json-par-token-matching-square-brackets-p (token)
  "Return non-nil if TOKEN is a balanced square brackets.

Return nil otherwise."
  (eq (json-par-token-type token) '\[\]))

(defun json-par-token-open-parenthesis-p (token)
  "Return non-nil if TOKEN is an open parenthesis.

Return nil otherwise."
  (eq (json-par-token-type token) '\())

(defun json-par-token-close-parenthesis-p (token)
  "Return non-nil if TOKEN is a close parenthesis.

Return nil otherwise."
  (eq (json-par-token-type token) '\)))

(defun json-par-token-matching-parentheses-p (token)
  "Return non-nil if TOKEN is a balanced parentheses.

Return nil otherwise."
  (eq (json-par-token-type token) '\(\)))

(defun json-par-token-comment-p (token)
  "Return non-nil if TOKEN is a comment.

Return nil otherwise.

It may be a single-line comment or multiple comment.

This kind of token is returned only from
`json-par-forward-token-or-list-or-comment'` or
`json-par-backward-token-or-list-or-comment'"
  (eq (json-par-token-type token) 'comment))

(defun json-par-token-single-line-comment-p (token)
  "Return non-nil if TOKEN is a single-line comment.

Return nil otherwise.

This kind of token is returned only from
`json-par-forward-token-or-list-or-comment'` or
`json-par-backward-token-or-list-or-comment'"
  (and (json-par-token-comment-p token)
       (save-excursion
         (goto-char (json-par-token-start token))
         (looking-at "//"))))

(defun json-par-token-multiline-comment-p (token)
  "Return non-nil if TOKEN is a multiline comment.

Return nil otherwise.

This kind of token is returned only from
`json-par-forward-token-or-list-or-comment'` or
`json-par-backward-token-or-list-or-comment'"
  (and (json-par-token-comment-p token)
       (save-excursion
         (goto-char (json-par-token-start token))
         (looking-at "/\\*"))))

(defun json-par-token-open-bracket-p (token)
  "Return non-nil if TOKEN is an open bracket.

It may be a curly bracket, a square bracket, or a parenthesis.

Return nil otherwise."
  (memq (json-par-token-type token) '(\[ \( {)))

(defun json-par-token-close-bracket-p (token)
  "Return non-nil if TOKEN is a close bracket.

It may be a curly bracket, a square bracket, or a parenthesis.

Return nil otherwise."
  (memq (json-par-token-type token) '(\] \) })))

(defun json-par-token-matching-brackets-p (token)
  "Return non-nil if TOKEN is a balanced brackets.

It may be curly brackets, square brackets, or parentheses.

Return nil otherwise."
  (memq (json-par-token-type token) '(\[\] \(\) {})))

(defun json-par-token-atom-p (token)
  "Return non-nil if TOKEN is an atom.

Atom is one of the following tokens:

- string
- number
- constant (true, false, null, or other alphanumeric (invalid) token)
- other (invalid token)

Return nil otherwise."
  (memq (json-par-token-type token) '(string number constant other)))

(defun json-par-token-one-line-p (token)
  "Return non-nil if TOKEN has no line breaks in it.

Return nil otherwise."
  (json-par--same-line-p
   (json-par-token-start token)
   (json-par-token-end token)))

(defun json-par--same-line-p (start end)
  "Return non-nil if START and END are on the same line.

Return nil otherwise."
  (= (save-excursion
       (goto-char start)
       (line-beginning-position))
     (save-excursion
       (goto-char end)
       (line-beginning-position))))

(defun json-par-forward-token ()
  "Move the point forward to the end of the next token.

Return a token object.  If no more tokens available, return a token with
type `outside-of-buffer'."
  (json-par--out-comment)
  (json-par--forward-spaces)
  (cond
   ;; Outside of buffer
   ((eobp)
    (json-par-token 'outside-of-buffer (point) (point)))

   ;; Separators and parentheses
   ((memq (char-after) '(?\, ?\{ ?\} ?\( ?\) ?\[ ?\] ?:))
    (forward-char)
    (json-par-token (intern (string (char-before))) (1- (point)) (point)))

   ;; String
   ((eq (char-after) ?\")
    (let ((pos-after-spaces (point)))
      (condition-case nil
          (goto-char (scan-sexps (point) 1))
        (scan-error (goto-char (point-max))))
      (json-par-token 'string pos-after-spaces (point))))

   ;; Number or constants
   ((looking-at "[-+0-9.a-zA-Z_]+")
    (let ((end-pos (match-end 0)))
      (prog1
          (json-par-token
           (cond
            ((and (looking-at "[-+]?[0-9]*\\.?[0-9]*\\(?:[eE][-+]?[0-9]*\\)?")
                  (eq (match-end 0) end-pos))
             'number)
            ((and (looking-at "[a-zA-Z_][0-9a-zA-Z_]*")
                  (eq (match-end 0) end-pos))
             'constant)
            (t 'other))
           (point)
           end-pos)
        (goto-char end-pos))))

   ;; Other
   (t
    (forward-char)
    (json-par-token 'other (1- (point)) (point)))))

(defun json-par-backward-token ()
  "Move the point forward to the beginning of the previous token.

Return a token object.  If no more tokens available, return a token with
type `outside-of-buffer'."
  (json-par--out-comment)
  (json-par--backward-spaces)
  (cond
   ;; Outside of buffer
   ((bobp)
    (json-par-token 'outside-of-buffer (point) (point)))

   ;; Separators and parentheses
   ((memq (char-before) '(?\, ?\{ ?\} ?\( ?\) ?\[ ?\] ?:))
    (backward-char)
    (json-par-token (intern (string (char-after))) (point) (1+ (point))))

   ;; String
   ((eq (char-before) ?\")
    (let ((pos-before-spaces (point)))
      (backward-char)
      (when (json-par--string-like-beginning-position)
        (goto-char (json-par--string-like-beginning-position)))
      (json-par-token 'string (point) pos-before-spaces)))

   ;; Number or constants
   ((save-excursion
      (backward-char)
      (looking-at "[-+0-9.a-zA-Z_]"))
    (let ((pos-before-spaces (point)))
      (skip-chars-backward "-+0-9a-z.A-Z_")
      (json-par-token
       (cond
        ((and (looking-at "[-+]?[0-9]*\\.?[0-9]*\\(?:[eE][-+]?[0-9]*\\)?")
              (eq (match-end 0) pos-before-spaces)
              (< 0 (- (match-end 0) (match-beginning 0))))
         'number)
        ((and (looking-at "[a-zA-Z_][0-9a-zA-Z_]*")
              (eq (match-end 0) pos-before-spaces))
         'constant)
        (t
         'other))
       (point)
       pos-before-spaces)))

   ;; Other
   (t
    (backward-char)
    (json-par-token 'other (point) (1+ (point))))))

(defun json-par-backward-token-or-list ()
  "Move the point to the beginning of the previous token or list.

Return the token skipped."
  (let* ((previous-token (json-par-backward-token))
         (previous-type (json-par-token-type previous-token))
         (previous-start (json-par-token-start previous-token))
         (previous-end (json-par-token-end previous-token)))
    (cond
     ;; List
     ((memq previous-type '(} ?\) \]))
      (condition-case nil
          (progn
            ;; `backward-list' is 10x slower than this and takes a few seconds
            ;; for 10K+ lines of JSON if `parse-sexp-lookup-properties' is
            ;; non-nil.  This is a lightweight alternative for it.  If
            ;; `backward-list' becomes fast enough, replace this with
            ;; `backward-list'.
            (goto-char (nth 1 (syntax-ppss)))
            (json-par-token
             (assoc-default previous-type '((} . {})
                                            (\) . \(\))
                                            (\] . \[\])))
             (point)
             previous-end))
        (scan-error
         (goto-char previous-start)
         previous-token)))

     ;; Other token
     (t previous-token))))

(defun json-par-forward-token-or-list ()
  "Move the point to the end of the next token or list.

Return the token skipped."
  (let* ((next-token (json-par-forward-token))
         (next-type (json-par-token-type next-token))
         (next-start (json-par-token-start next-token))
         (next-end (json-par-token-end next-token)))
    (cond
     ;; List
     ((memq next-type '({ \( \[))
      (goto-char next-start)
      (condition-case nil
          (progn
            (forward-list)
            (json-par-token
             (assoc-default next-type '(({ . {})
                                        (\( . \(\))
                                        (\[ . \[\])))
             next-start
             (point)))
        (scan-error
         (goto-char next-end)
         next-token)))

     ;; Other token
     (t next-token))))

(defun json-par-backward-token-or-list-or-comment ()
  "Move the point to the beginning of the previous token, list, or comment.

Return the token skipped."
  (let ((comment-region (json-par--previous-comment-region)))
    (if comment-region
        (progn
          (goto-char (car comment-region))
          (json-par-token 'comment (car comment-region) (cdr comment-region)))
      (json-par-backward-token-or-list))))

(defun json-par-forward-token-or-list-or-comment ()
  "Move the point to the end of the next token, list or comment.

Return the token skipped."
  (let ((comment-region (json-par--next-comment-region)))
    (if comment-region
        (progn
          (goto-char (cdr comment-region))
          (json-par-token 'comment (car comment-region) (cdr comment-region)))
      (json-par-forward-token-or-list))))

(defun json-par-backward-token-or-comment ()
  "Move the point to the beginning of the previous token or comment.

Return the token skipped."
  (let ((comment-region (json-par--previous-comment-region)))
    (if comment-region
        (progn
          (goto-char (car comment-region))
          (json-par-token 'comment (car comment-region) (cdr comment-region)))
      (json-par-backward-token))))

(defun json-par-forward-token-or-comment ()
  "Move the point to the end of the next token or comment.

Return the token skipped."
  (let ((comment-region (json-par--next-comment-region)))
    (if comment-region
        (progn
          (goto-char (cdr comment-region))
          (json-par-token 'comment (car comment-region) (cdr comment-region)))
      (json-par-forward-token))))


(defun json-par--string-like-beginning-position (&optional parser-state)
  "Return the beginning position of a string or a comment at the point.

If PARSER-STATE is given, it is used instead of (syntax-ppss).
If PARSER-STATE is a number or a marker, use that position for (syntax-ppss)."
  (save-excursion
    (when (number-or-marker-p parser-state)
      (goto-char parser-state))
    (when (or (null parser-state) (number-or-marker-p parser-state))
      (setq parser-state (save-excursion (syntax-ppss parser-state))))
    (cond
     ;; inside a string
     ((nth 3 parser-state)
      (nth 8 parser-state))

     ;; inside a comment, JSON has no comments though
     ((nth 4 parser-state)
      (nth 8 parser-state))

     ;; between //, JSON has no comments though
     ((and (eq (char-before) ?/)
           (eq (char-after) ?/)
           (save-excursion
             (backward-char)
             (not (nth 4 (syntax-ppss)))))
      (1- (point)))

     ;; between /*, JSON has no comments though
     ((and (eq (char-before) ?/)
           (eq (char-after) ?*)
           (save-excursion
             (backward-char)
             (not (nth 4 (syntax-ppss)))))
      (1- (point))))))

(defun json-par--next-comment-region ()
  "Return region of the next comment.

Return a pair (START . END) where START and END are the start position and end
position of the comment.

Assuming the point is not in a comment.

If the point is not before a comment, return nil."
  (save-excursion
    (skip-chars-forward "\s\t\n")
    (when (save-excursion (forward-comment 1))
      (cons
       (point)
       (progn (forward-comment 1) (point))))))

(defun json-par--previous-comment-region ()
  "Return region of the prevision comment.

Return a pair (START . END) where START and END are the start position and end
position of the comment.

If the point is not after a comment, return nil."
  (save-excursion
    (skip-chars-backward "\s\t\n")
    (cond ((json-par--string-like-beginning-position)
           (cons
            (json-par--string-like-beginning-position)
            (progn
              (skip-chars-forward "\s\t")
              (when (eq (char-after) ?\n)
                (forward-char))
              (point))))

          ((forward-comment -1)
           (cons (point)
                 (progn
                   (forward-comment 1)
                   (point))))

          (t nil))))

(defun json-par--forward-spaces (&optional keep-line)
  "Skip whitespaces, newlines and comments forward.

If KEEP-LINE is non-nil, don't skip newlines except inside comments."
  (if keep-line
      (progn
        (skip-chars-forward "\s\t")
        (while (and (eq (char-after) ?/)
                    (eq (char-after (1+ (point))) ?*)
                    (forward-comment 1))
          (skip-chars-forward "\s\t")))
    (forward-comment (point-max))))

(defun json-par--backward-spaces (&optional keep-line)
  "Skip whitespaces, newlines and comments backward.

If KEEP-LINE is non-nil, don't skip newlines except inside comments."
  (if keep-line
      (progn
        (skip-chars-backward "\s\t")
        (while (and (eq (char-before) ?/)
                    (eq (char-before (1- (point))) ?*)
                    (forward-comment -1))
          (skip-chars-backward "\s\t")))
    (forward-comment (- (point)))))

(defvar-local json-par--already-out-of-comment nil
  "Non-nil if the point is already out of comment.

`json-par--out-comment' is often a bottleneck, so using this may improve
performance.")

(defun json-par--out-comment (&optional parser-state)
  "Move before a comment if the point is inside a comment.

If PARSER-STATE is given, it is used instead of (syntax-ppss).
If PARSER-STATE is a number or a marker, use that position for (syntax-ppss)."
  (unless json-par--already-out-of-comment
    (let ((string-like-beginning-position
           (json-par--string-like-beginning-position parser-state)))
      (when (and
             string-like-beginning-position
             (not (eq (char-after string-like-beginning-position) ?\")))
        (goto-char string-like-beginning-position)))))

(defun json-par--current-atom (&optional parser-state)
  "Return an atom token under or just after/before the point.

An atom is a string, a number, a constant, or an unknown token.
If PARSER-STATE is given, it is used instead of (syntax-ppss).
If PARSER-STATE is a number or a marker, use that position for (syntax-ppss)."
  (save-excursion
    (let ((string-like-beginning-position
           (json-par--string-like-beginning-position parser-state)))
      (cond
       ;; inside a string
       ((and string-like-beginning-position
             (eq (char-after string-like-beginning-position) ?\"))
        (goto-char (json-par--string-like-beginning-position parser-state))
        (json-par-forward-token))

       ;; inside a comment, JSON has no comments though
       (string-like-beginning-position
        nil)

       ;; maybe inside/before/after a string, a number, or a constant
       (t
        (let ((previous-token (save-excursion (json-par-backward-token)))
              next-token)
          (if (and
               (json-par-token-atom-p previous-token)
               (eq (json-par-token-end previous-token) (point)))
              (progn
                (goto-char (json-par-token-start previous-token))
                (json-par-forward-token))
            (setq next-token (save-excursion (json-par-forward-token)))
            (when (and
                   (json-par-token-atom-p next-token)
                   (eq (json-par-token-start next-token) (point)))
              (goto-char (json-par-token-end next-token))
              (json-par-backward-token)))))))))

(defvar-local json-par--already-out-of-atom nil
  "Non-nil if the point is already out of comment.

`json-par--out-atom' is often a bottleneck, so using this may improve
performance.")

(defun json-par--out-atom (&optional skip-comma)
  "Move after an atom if the point is inside an atom.

When SKIP-COMMA is non-nil, skip following comma if any."
  (unless json-par--already-out-of-atom
    (let ((current-atom (json-par--current-atom)))
      (when (json-par-token-inside-p current-atom)
        (goto-char (json-par-token-end current-atom))
        (when (and skip-comma
                   (save-excursion
                     (json-par--forward-spaces)
                     (eq (char-after) ?\,)))
          (json-par--forward-spaces)
          (forward-char))))))

(defun json-par--object-key-p (token &optional right-associative)
  "Return non-nil if TOKEN is a object key.

A token is a object key if and only if:

- it is a string,
- it is before colon, and
- if RIGHT-ASSOCIATIVE is non-nil, then it is not after colon.

Examples:

  // \"a\" is a object key
  { \"a\": 1 }

  // \"b\" is not a object key
  { \"a\": \"b\" }

  // \"b\" is not a object key if RIGHT-ASSOCIATIVE is nil
  { \"a\": \"b\": }
  // this is treated as:
  { \"a\": \"b\", : }
  // If RIGHT-ASSOCIATIVE is non-nil, it is treated as the following,
  // so \"b\" is a object key.
  { \"a\": , \"b\": }

This function affects where a comma is inserted by `json-par-insert-comma'."
  (and
   (json-par-token-string-p token)
   (save-excursion
     (goto-char (json-par-token-end token))
     (json-par--forward-spaces)
     (eq (char-after) ?:))
   (or right-associative
       (save-excursion
         (goto-char (json-par-token-start token))
         (json-par--backward-spaces)
         (not (eq (char-before) ?:))))))

(defun json-par--read-token (token)
  "Parse TOKEN and return its value.

Assuming the token is still where it is at the time of creation."
  (save-excursion
    (goto-char (json-par-token-start token))
    (json-read)))

(defun json-par--read-object-key-if-any ()
  "Parse and return the following token if it is an object key.

Return nil otherwise."
  (let ((token (save-excursion (json-par-forward-token))))
    (if (json-par--object-key-p token)
        (json-par--read-token token)
      nil)))


(provide 'json-par-lexer)

;;; json-par-lexer.el ends here
