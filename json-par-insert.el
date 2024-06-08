;;; json-par.el --- Inserting values in JSON Par mode -*- lexical-binding: t -*-

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

;; Functions for inserting values in JSON Par mode.

;;; Code:

(require 'cl-lib)
(require 'json-par-lexer)
(require 'json-par-motion)
(require 'json-par-indent)

(defvar json-par--fixup-adviced-functions nil
  "Functions to be adviced with `json-par--fixup-advice'.")

(defvar-local json-par--inhibit-fixup-tick nil
  "Suppress fixing if this variable equal to `buffer-chars-modified-tick'.")

;;; Customizations

(defcustom json-par-action-when-inserting-double-quotes-at-end 'exit
  "Action for inserting a double quote before a closing double quote.

- `insert': insert an escaped double quote.
- `exit': move the point after the string."
  :type '(choice (const :tag "Insert escaped double quotes" insert)
                 (const :tag "Exit from the string" exit))
  :group 'json-par
  :safe #'symbolp)

(defcustom json-par-default-brackets-style 'multiline
  "Whether or not to insert line breaks when inserting an empty array or object.

This value is used only if the style cannot be guessed from the context.

- `one-line': no line breaks.
  Example (`|' is the point): [|]

- `multiline': insert line breaks and indentation.
  Example:
  [
    |
  ]"
  :type '(choice (const :tag "Single line" one-line)
                 (const :tag "Multiline" multiline))
  :group 'json-par
  :safe #'symbolp)


;;; Functions

(defun json-par--insert-value
    (value &optional place-point-before-comma omit-key)
  "Insert VALUE before or after the current member.

If the point is on or after the value of the current member, insert the value
after the current member.  Otherwise, insert the value before the current
member.  If the point is where a value is expected but missing, insert to there.

Insert commas, spaces, and line breaks if needed.

If the point is inside an object, the current member lacks a key, and OMIT-KEY
is nil, insert an empty key and a colon before the value.

Move the point after the value.  If PLACE-POINT-BEFORE-COMMA is nil and a comma
is inserted after the value, move the point after the comma.

Examples (`|' is the point):

  // After a value
  {
    \"a\": \"aaa\"|,
    \"b\": \"bbb\"
  }
  ↓ (json-par--insert-value \"true\")
  {
    \"a\": \"aaa\",
    \"\": true,|
    \"b\": \"bbb\"
  }

  // Before a member (\"b\")
  {
    \"a\": \"aaa\",|
    \"b\": \"bbb\"
  }
  ↓ (json-par--insert-value \"true\")
  {
    \"a\": \"aaa\",
    \"\": true,|
    \"b\": \"bbb\"
  }

  // Before a value
  {
    \"a\": |\"aaa\",
    \"b\": \"bbb\"
  }
  ↓ (json-par--insert-value \"true\")
  {
    \"\": true,|
    \"a\": \"aaa\",
    \"b\": \"bbb\"
  }"
  (json-par--out-comment)
  (json-par--out-atom t)
  (let* ((next-token (save-excursion (json-par-forward-token)))
         (previous-token (save-excursion (json-par-backward-token)))
         (parent-token (json-par--parent-token))
         (point-is-just-after-previous-token
          (eq (json-par-token-end previous-token) (point)))
         (inside-object (json-par-token-open-curly-bracket-p parent-token))
         (one-line
          (<= (save-excursion (goto-char (json-par-token-start next-token))
                              (line-beginning-position))
              (json-par-token-end parent-token))))
    (cond
     ;; Inside an empty brackets, or between two commas or other places
     ;; expecting a value (but not before a colon)
     ((or (and (memq (json-par-token-type previous-token)
                     '({ \( \[ \, : outside-of-buffer))
               (memq (json-par-token-type next-token)
                     '(} \) \] \, outside-of-buffer)))
          (json-par-token-outside-of-buffer-p parent-token))
      (when (and point-is-just-after-previous-token
                 (not (json-par-token-outside-of-buffer-p previous-token)))
        (if (memq (char-after) '(?\s ?\t))
            (forward-char)
          (insert-char ?\s)))
      (when (and inside-object
                 (not (json-par-token-colon-p previous-token))
                 (not omit-key))
        (insert "\"\": "))
      (insert value)
      (when (memq (char-after) '(?} ?\) ?\]))
        (if one-line
            (save-excursion (insert-char ?\s))
          (json-par--open-line-and-indent-both))))

     ;; Between a colon and the next key or another colon (comma missing)
     ((and (eq (json-par-token-type previous-token) ':)
           (or (eq (json-par-token-type next-token) ':)
               (json-par--object-key-p next-token t)))
      (when (and point-is-just-after-previous-token
                 (not (json-par-token-outside-of-buffer-p previous-token)))
        (if (memq (char-after) '(?\s ?\t))
            (forward-char)
          (insert-char ?\s)))
      (insert value)
      (unless (memq (char-after) '(?\s ?\t ?\n nil))
        (insert-char ?\s))
      (json-par-insert-comma)
      (when place-point-before-comma
        (json-par--backward-spaces)
        (backward-char)))

     ;; Otherwise
     (t
      (when (json-par-token-comma-p next-token)
        (setq place-point-before-comma t))
      (while (not (json-par-insert-comma))
        t)
      (json-par--insert-value value place-point-before-comma omit-key)
      (when (and (not place-point-before-comma) (eq (char-after) ?\,))
        (forward-char))))))

(defun json-par-insert-true ()
  "Insert a `true' before or after the current member.

See `json-par--insert-value' for details."
  (interactive)
  (json-par--insert-value "true"))

(push #'json-par-insert-true json-par--fixup-adviced-functions)

(defun json-par-insert-false ()
  "Insert a `false' before or after the current member.

See `json-par--insert-value' for details."
  (interactive)
  (json-par--insert-value "false"))

(push #'json-par-insert-false json-par--fixup-adviced-functions)

(defun json-par-insert-null ()
  "Insert a `null' before or after the current member.

See `json-par--insert-value' for details."
  (interactive)
  (json-par--insert-value "null"))

(push #'json-par-insert-null json-par--fixup-adviced-functions)

(defun json-par-insert-self-as-value ()
  "Insert a char invoked this command before or after the current member.

See `json-par--insert-value' for details."
  (interactive)
  (json-par--insert-value (string last-command-event)))

(push #'json-par-insert-self-as-value json-par--fixup-adviced-functions)

(defun json-par-insert-self-as-number ()
  "Insert a char invoked this command before or after the current member.

See `json-par--insert-value' for details.

This function doesn't move the point after a comma, if any, after the number."
  (interactive)
  (json-par--insert-value (string last-command-event) t))

(push #'json-par-insert-self-as-number json-par--fixup-adviced-functions)

(defun json-par-insert-brackets (open close default-brackets-style)
  "Insert a pair of brackets OPEN and CLOSE before or after the current member.

See `json-par--insert-value' for details.

If the region is active, wrap it with the brackets instead.

See `json-par--wrap-region-with-brackets' for details.

Return a list (FINAL-START FINAL-END) where FINAL-START and FINAL-END are the
start and end of the brackets.

DEFAULT-BRACKETS-STYLE affects whether or not to insert line breaks when
inserting an empty array or object.  See `json-par-default-brackets-style' for
details."
  (if (use-region-p)
      (json-par--wrap-region-with-brackets
       open
       close
       (region-beginning)
       (region-end)
       default-brackets-style)
    (json-par--insert-value (concat open close) t)
    (let* ((should-break-line
            (json-par--should-break-line-after-inserting-brackets
             (- (point) (length close) (length open))
             (point)
             default-brackets-style))
           start
           end)
      (backward-char (length close))
      (setq start (- (point) (length open)))
      (setq end (copy-marker (+ (point) (length close))))
      (when should-break-line
        (newline)
        (json-par--open-line-and-indent-both))
      (list start (json-par--free-marker end)))))

(defun json-par--wrap-region-with-brackets
    (open close start end default-brackets-style)
  "Wrap region from START toe END with brackets OPEN and CLOSE.

Insert spaces and line breaks if needed.

If the point is inside an object, insert an empty key and a colon before the
brackets.

Indent the region after wrapping.

Return a list (FINAL-START FINAL-END) where FINAL-START and FINAL-END are the
start and end of the wrapped brackets."
  (setq end (copy-marker end))
  (let* ((point-marker (point-marker))
         (parent-token (json-par--parent-token))
         (inside-object (json-par-token-open-curly-bracket-p parent-token)))
    (goto-char end)
    (unless (memq (char-before) '(?\s ?\t ?\n nil))
      (insert-before-markers " "))
    (insert-before-markers close)
    (goto-char start)
    (insert open)
    (unless (memq (char-after) '(?\s ?\t ?\n nil))
      (insert-char ?\s))
    (goto-char start)
    (when (and inside-object
               (save-excursion
                 (json-par--backward-spaces)
                 (not (eq (char-before) ?:))))
      (setq start (copy-marker start))
      (set-marker-insertion-type start t)
      (set-marker-insertion-type point-marker t)
      (json-par--insert-key-or-value "\"\"")
      (set-marker-insertion-type point-marker nil)
      (setq start (json-par--free-marker start)))
    (when (json-par--should-break-line-after-inserting-brackets
           start
           end
           default-brackets-style)
      (goto-char start)
      (forward-char (length open))
      (skip-chars-forward "\s\t")
      (unless (eolp)
        (skip-chars-backward "\s\t")
        (insert-char ?\n))
      (goto-char end)
      (backward-char (length close))
      (skip-chars-backward "\s\t")
      (unless (bolp)
        (insert-char ?\n)))
    (goto-char point-marker)
    (json-par--free-marker point-marker)
    (json-par-indent-region start end)
    (list start (json-par--free-marker end))))

(defun json-par--should-break-line-after-inserting-brackets
    (start end default-brackets-style)
  "Return non-nil if we should break line between brackets just inserted.

START and END is the start and end of the brackets.

Use DEFAULT-BRACKETS-STYLE if no idea.  See `json-par-default-brackets-style'
for details."
  (save-excursion
    (goto-char end)
    (let* (token
           (previous-value (save-excursion
                             (json-par-beginning-of-member)
                             (setq token (json-par-backward-token-or-list))
                             (when (json-par-token-comma-p token)
                               (setq token (json-par-backward-token-or-list)))
                             token))
           (next-value (save-excursion
                         (setq token (json-par-forward-token-or-list))
                         (when (json-par-token-comma-p token)
                           (json-par-end-of-member)
                           (setq token (json-par-backward-token-or-list)))
                         token))
           (previous-is-brackets
            (json-par-token-matching-brackets-p previous-value))
           (next-is-brackets
            (json-par-token-matching-brackets-p next-value))
           (previous-is-multiline-brackets
            (and previous-is-brackets
                 (not (json-par-token-one-line-p previous-value))))
           (next-is-multiline-brackets
            (and next-is-brackets
                 (not (json-par-token-one-line-p next-value)))))
      (or previous-is-multiline-brackets
          next-is-multiline-brackets
          (not (json-par--same-line-p start end))
          (and
           ;; If both the previous and next is one-line brackets, this should be
           ;; one-line too.
           (not previous-is-brackets)
           (not next-is-brackets)
           ;; If the parent is one-line, this should be one-line too.
           (or
            (json-par-token-outside-of-buffer-p previous-value)
            (/= (save-excursion
                  (json-par-up-backward)
                  (line-beginning-position))
                (save-excursion
                  (json-par-up-forward)
                  (line-beginning-position))))
           (not (eq default-brackets-style 'one-line)))))))

(defun json-par-insert-square-brackets (&optional default-brackets-style)
  "Insert a pair of square brackets before or after the current member.

See `json-par--insert-value' for details.

If the region is active, wrap it with the brackets instead.
See `json-par--wrap-region-with-brackets' for details.

Return a list (FINAL-START FINAL-END) where FINAL-START and FINAL-END are the
start and end of the brackets.

DEFAULT-BRACKETS-STYLE affects whether or not to insert line breaks when
inserting an empty array or object.  It is one of the following:

- `one-line': no line breaks.
  Example (`|' is the point): [|]

- `multiline': insert line breaks and indentation.
  Example:
  [
    |
  ]

It is used only if the style cannot be guessed from the context:

- If the previous value or the next value is an array/object, the style is same
  to those array/object.  If one is multiline and another is one-line, the style
  is multiline.

- If the parent object/array is one-line, the style is one-line.

- Otherwise, `default-brackets-style' is used.

If DEFAULT-BRACKETS-STYLE is omitted, `one-line' is used when called from Lisp
program.  If called interactively, the value of
`json-par-default-brackets-style' is used."
  (interactive
   (list
    json-par-default-brackets-style))
  (unless default-brackets-style
    (setq default-brackets-style 'one-line))
  (json-par-insert-brackets "[" "]" default-brackets-style))

(push #'json-par-insert-square-brackets json-par--fixup-adviced-functions)

(defun json-par-insert-curly-brackets (&optional default-brackets-style)
  "Insert a pair of curly brackets before or after the current member.

See `json-par--insert-value' for details.

If the region is active, wrap it with the brackets instead.
See `json-par--wrap-region-with-brackets' for details.

Then, prepend an empty key and a colon for each member if missing.

Return a list (FINAL-START FINAL-END) where FINAL-START and FINAL-END are the
start and end of the brackets.

DEFAULT-BRACKETS-STYLE affects whether or not to insert line breaks when
inserting an empty array or object.  It is one of the following:

- `one-line': no line breaks.
  Example (`|' is the point): [|]

- `multiline': insert line breaks and indentation.
  Example:
  [
    |
  ]

It is used only if the style cannot be guessed from the context:

- If the previous value or the next value is an array/object, the style is same
  to those array/object.  If one is multiline and another is one-line, the style
  is multiline.

- If the parent object/array is one-line, the style is one-line.

- Otherwise, `default-brackets-style' is used.

If DEFAULT-BRACKETS-STYLE is omitted, `one-line' is used when called from Lisp
program.  If called interactively, the value of
`json-par-default-brackets-style' is used."
  (interactive
   (list
    json-par-default-brackets-style))
  (unless default-brackets-style
    (setq default-brackets-style 'one-line))
  (let* ((positions (json-par-insert-brackets "{" "}" default-brackets-style))
         (start (nth 0 positions)))
    (json-par--prepend-empty-keys-to-values-if-missing start)))

(push #'json-par-insert-curly-brackets json-par--fixup-adviced-functions)

(defun json-par--prepend-empty-keys-to-values-if-missing (start)
  "Prepend an empty key and a colon for each members if missing.

START is the start position of the object."
  (save-excursion
    (goto-char start)
    (forward-char)
    (let (token
          last-token)
      (while (progn
               (setq token (json-par-forward-token-or-list))
               (not (json-par-token-close-curly-bracket-p token)))
        (unless (or (json-par--object-key-p token)
                    (memq (json-par-token-type token) '(\, other :))
                    (eq (json-par-token-type last-token) ':))
          (save-excursion
            (goto-char (json-par-token-start token))
            (insert "\"\": ")))
        (setq last-token token)))))

(defun json-par-insert-double-quotes
    (&optional arg action-when-inserting-double-quotes-at-end)
  "Insert double quotes before or after the current member.

- If ARG is given or called with a prefix arg, unwrap the string.
  See `json-par-destringify' for details.

  Example (`|' is the point):
  [ \"|1, 2, 3\" ]
  ↓
  [ |1, 2, 3 ]

- Inside a string, insert an escaped double quote.  If the point is just
  after escape (backslash), insert just one double quote without backslash.

  \"abc |def ghi\"
  ↓
  \"abc \\\"|def ghi\"

  If the string is not closed, insert a unescaped double quote.

- Inside a comment, insert just a double quote.

- If the point is just before the closing double quote, action depends on
  ACTION-WHEN-INSERTING-DOUBLE-QUOTES-AT-END

  - `insert': insert an escaped double quote.
  - `exit': move the point after the string.

  Defaults to `exit' when called from Lisp program, or the value of
  `json-par-action-when-inserting-double-quotes-at-end' variable when called
  interactively.

- If the region is active, wrap it with the double quotes.

  [ |1, 2, 3| ]
  ↓
  [ |\"1, 2, 3\"| ]

- Otherwise, insert a pair of double quotes.
  See `json-par--insert-value' for details."
  (interactive
   (list
    current-prefix-arg
    json-par-action-when-inserting-double-quotes-at-end))
  (unless action-when-inserting-double-quotes-at-end
    (setq action-when-inserting-double-quotes-at-end 'insert))
  (let (parser-state)
    (if arg
        (json-par-destringify)
      (cond
       ;; Region is active
       ((use-region-p)
        (json-par-stringify-region (region-beginning) (region-end)))

       ;; Inside a string
       ((save-excursion (nth 3 (setq parser-state (syntax-ppss))))
        (cond
         ;; After escape
         ((json-par--escaped-p)
          (insert-char ?\"))

         ;; Unclosed string
         ((save-excursion
            (goto-char (json-par--string-like-beginning-position
                        parser-state))
            (forward-sexp)
            (or (not (eq (char-before) ?\"))
                (not (memq (char-after)
                           '(nil ?: ?\] ?\) ?} ?\, ?\s ?\t ?\n ?/)))))
          (insert-char ?\"))

         ;; Before closing quote
         ((eq (char-after) ?\")
          (if (eq action-when-inserting-double-quotes-at-end 'exit)
              (forward-char)
            (insert "\\\"")))

         ;; Otherwise
         (t
          (insert "\\\""))))

       ;; Inside a comment
       ((json-par--string-like-beginning-position parser-state)
        (insert "\""))

       ;; Otherwise
       (t
        (json-par--insert-key-or-value "\"\"")
        (backward-char))))))

(push #'json-par-insert-double-quotes json-par--fixup-adviced-functions)

(defun json-par--escaped-p ()
  "Return non-nil if the point is just after odd number of escape characters.

Return nil otherwise."
  (and (eq (char-before) ?\\)
       (save-excursion
         (= (mod (skip-chars-backward "\\\\") 2) 1))))

(defun json-par-stringify-region (start end)
  "Wrap the region from START to END with double quotes.

Special characters in the region is escaped.

If START or END is inside strings, escaped double quotes are inserted."
  (interactive "r")
  (save-excursion
    (let* ((start-atom (json-par--current-atom start))
           (end-atom (json-par--current-atom end))
           (start-in-string (and (json-par-token-string-p start-atom)
                                 (json-par-token-inside-p start-atom)))
           (end-in-string (and (json-par-token-string-p end-atom)
                               (json-par-token-inside-p end-atom))))
      (setq end (copy-marker (json-par-escape-region-for-string start end)))
      (goto-char end)
      (insert-before-markers (if end-in-string "\\\"" "\""))
      (goto-char start)
      (insert (if start-in-string "\\\"" "\""))
      (json-par--free-marker end))))

(push #'json-par-stringify-region json-par--fixup-adviced-functions)

(defun json-par-destringify ()
  "Unwrap strings in the region or at the point.

If the region is active, unwrap all strings in it.

Otherwise, unwrap the nearest string.

Escaped characters in the strings are unescaped.

Inhibit fixing up until next modification."
  (interactive)
  (let* ((parser-state (save-excursion (syntax-ppss)))
         (current-atom (json-par--current-atom parser-state))
         (string-like-beginning-position
          (json-par--string-like-beginning-position parser-state)))
    (cond
     ;; Region is active
     ((use-region-p)
      (json-par-destringify-region (region-beginning) (region-end)))

     ;; Inside a string
     ((and (json-par-token-string-p current-atom)
           (json-par-token-inside-p current-atom))
      (save-excursion
        (goto-char (json-par-token-start current-atom))
        (json-par--destringify-after)))

     ;; Inside a comment
     (string-like-beginning-position
      nil)

     ;; Before or after a string
     ((or
       (save-excursion
         (json-par--forward-spaces)
         (eq (char-after) ?\"))
       (save-excursion
         (json-par--backward-spaces)
         (eq (char-before) ?\")))
      ;; Prefer nearer string
      (if (or (eq (char-after) ?\")
              (and
               (not (eq (char-before) ?\"))
               (or (save-excursion
                     (skip-chars-forward "\s\t")
                     (eq (char-after) ?\"))
                   (and
                    (not (save-excursion
                           (skip-chars-backward "\s\t")
                           (eq (char-before) ?\")))
                    (save-excursion
                      (json-par--forward-spaces)
                      (eq (char-after) ?\"))))))
          (save-excursion
            (json-par--forward-spaces)
            (json-par--destringify-after))
        (save-excursion
          (json-par-backward-token)
          (json-par--destringify-after))))

     ;; Otherwise; do nothing
     (t
      nil))))

(push #'json-par-destringify json-par--fixup-adviced-functions)

(defun json-par-destringify-region (start end)
  "Unwrap strings overlapping the region from START to END.

Escaped characters in the strings are unescaped.

Inhibit fixing up until next modification.

Return the end position."
  (interactive "r")
  (save-excursion
    (let ((start-string-like-beginning-position
           (json-par--string-like-beginning-position start))
          (end-string-like-beginning-position
           (json-par--string-like-beginning-position end))
          token)
      (when start-string-like-beginning-position
        (setq start start-string-like-beginning-position))
      (when end-string-like-beginning-position
        (goto-char end-string-like-beginning-position)
        (if (eq (char-after) ?\")
            (json-par-forward-token)
          (json-par--forward-spaces))
        (setq end (point)))
      (setq end (copy-marker end))
      (goto-char start)
      (while (progn
               (setq token (json-par-forward-token))
               (and (< (json-par-token-start token) end)))
        (when (json-par-token-string-p token)
          (goto-char (json-par-token-start token))
          (json-par--destringify-after)))
      (json-par--free-marker end))))

(push #'json-par-destringify-region json-par--fixup-adviced-functions)

(defun json-par--destringify-after ()
  "Unwrap a string just after the point.

Inhibit fixing up until next modification."
  (let ((start (point-marker))
        (end (save-excursion (json-par-forward-token) (point-marker))))
    (goto-char end)
    (delete-char -1)
    (goto-char start)
    (delete-char 1)
    (goto-char (json-par-unescape-region-for-string start end))
    (setq json-par--inhibit-fixup-tick (buffer-chars-modified-tick))))

(defun json-par--insert-key-or-value (value)
  "Insert VALUE as a key or a value.

It is almost same as `json-par--insert-value' but if the point is inside an
object and not where a value is expected but missing, insert as a key instead.


Examples (`|' is the point):

Where a value is expected but not a key:

  { \"a\": | }
  ↓
  { \"a\": \"new_value\"| }

  { |\"a\": 1 }
  ↓
  { \"\": \"new_value\"|, \"a\": 1 }

Where a key is expected:

  { |: 1 }
  ↓
  { \"new_key\": 1| }

  { \"a\": 1, | }
  ↓
  { \"a\": 1, \"new_key\": | }

  { \"a\": 1| }
  ↓
  { \"a\": 1, \"new_key\": | }"
  (json-par--out-comment)
  (json-par--out-atom t)
  (let* ((next-token (save-excursion (json-par-forward-token)))
         (previous-token (save-excursion (json-par-backward-token)))
         (parent-token (json-par--parent-token))
         (inside-object (json-par-token-open-curly-bracket-p parent-token)))
    (cond
     ;; Not inside an object.
     ((not inside-object)
      (json-par--insert-value value t))

     ;; Place expecting a value but not a key.
     ;;
     ;; A value is expected:
     ;; {
     ;;   "a": |
     ;;   : 1
     ;; }
     ;;
     ;; A key is expected:
     ;; {
     ;;   "a":
     ;;   |: 1
     ;; }
     ;;
     ;; A Value is expected:
     ;; { "a": | : 1 }
     ((and (json-par-token-colon-p previous-token)
           (or (not (json-par-token-atom-p next-token))
               (json-par--object-key-p next-token t))
           (not (json-par-token-open-bracket-p next-token))
           (or (not (json-par-token-colon-p next-token))
               (not (json-par--same-line-p (json-par-token-start next-token)
                                           (point)))
               (json-par--same-line-p (json-par-token-end previous-token)
                                      (point))))
      (json-par--insert-value value t))

     ;; Before colon without a key.
     ((and (json-par-token-colon-p next-token)
           (not (json-par--object-key-p previous-token)))
      (json-par--insert-key value previous-token next-token))

     ;; Value without key.
     ((and (or (json-par-token-open-bracket-p next-token)
               (json-par-token-atom-p next-token))
           (not (json-par--object-key-p next-token))
           (not (json-par-token-colon-p previous-token)))
      (json-par--insert-key value previous-token next-token))

     ;; Otherwise; insert a new member and replace the key with the `value'.
     (t
      (json-par--insert-value "0" t)
      (delete-char -1)
      (json-par-backward-token)
      (let ((key-token (json-par-backward-token)))
        (delete-region (json-par-token-start key-token)
                       (json-par-token-end key-token)))
      (insert value)))))

(defun json-par--insert-key (key previous-token next-token)
  "Insert KEY before or after the current member as a object key.

Insert spaces, commas, a colon if needed.

Example (`|' is the point):

  { |: 1 }
  ↓
  { \"new_key\": 1| }

  { \"a\": 1| }
  ↓
  { \"a\": 1, \"new_key\": | }

  { \"a\": |1 }
  ↓
  { \"new_key\": | , \"a\": 1}

PREVIOUS-TOKEN and NEXT-TOKEN is the previous token and the next token around
the point."
  (unless
      (or (json-par-token-open-bracket-p previous-token)
          (json-par-token-comma-p previous-token)
          (json-par-token-outside-of-buffer-p previous-token))
    (json-par-insert-comma))
  (unless (memq (char-before) '(?\s ?\t ?\n nil))
    (insert-char ?\s))
  (insert key)
  (save-excursion
    (unless (json-par-token-colon-p next-token)
      (insert-char ?:)
      (unless (memq (char-after) '(?\s ?\t ?\n))
        (insert-char ?\s)))))

(defconst json-par--escape-table
  (let ((table (make-char-table nil)))
    (set-char-table-range table ?\" "\\\"")
    (set-char-table-range table ?\\ "\\\\")
    (set-char-table-range table ?\u0008 "\\b")
    (set-char-table-range table ?\u000C "\\f")
    (set-char-table-range table ?\u000A "\\n")
    (set-char-table-range table ?\u000D "\\r")
    (set-char-table-range table ?\u0009 "\\t")
    table)
  "Character table from raw characters to two-character escape sequences.

Only mandatory characters; solidus (\"/\") is not mandatory to escape.")

(defconst json-par--unescape-table
  (let ((table (make-char-table nil)))
    (set-char-table-range table ?\" "\"")
    (set-char-table-range table ?\\ "\\")
    (set-char-table-range table ?/ "/")
    (set-char-table-range table ?b "\u0008")
    (set-char-table-range table ?f "\u000C")
    (set-char-table-range table ?n "\u000A")
    (set-char-table-range table ?r "\u000D")
    (set-char-table-range table ?t "\u0009")
    table)
  "Character table from two-character escape sequences to raw characters.

The keys are the second characters of escape sequences.")

(defun json-par-escape-region-for-string (start end)
  "Escape the region from START to END to be a valid string.

Escape only mandatory characters; quotation mark, reverse solidus, and the
control characters (U+0000 through U+001F).  Solidus (\"/\") is left as is.

Return the end position of the region."
  (interactive "r")
  (save-excursion
    (save-match-data
      (goto-char start)
      (setq end (copy-marker end))
      (while (re-search-forward "[\u0000-\u001F\"\\]" end t)
        (let* ((char (char-after (match-beginning 0)))
               (short-form (char-table-range json-par--escape-table char)))
          (replace-match (or short-form (format "\\u%04x" char)) t t)))
      (json-par--free-marker end))))

(push #'json-par-escape-region-for-string json-par--fixup-adviced-functions)

(defun json-par-unescape-region-for-string (start end)
  "Unescape the region from START to END as a string.

Invalid sequences are left as is.

Return the end position of the region."
  (interactive "r")
  (save-excursion
    (save-match-data
      (goto-char start)
      (setq end (copy-marker end))
      (while (re-search-forward "\\\\." end t)
        (let* ((char (char-after (1+ (match-beginning 0))))
               (short-form (char-table-range json-par--unescape-table char))
               (match-beginning (match-beginning 0))
               (match-end (match-end 0))
               high-surrogate
               low-surrogate
               replacement)
          (cond
           ;; Two-character sequence escape
           (short-form
            (replace-match short-form t t))

           ;; Six-character sequence escape
           ((and (eq char ?u)
                 (looking-at "[0-9a-zA-Z]\\{4\\}"))
            (setq match-end (match-end 0))
            (setq high-surrogate
                  (string-to-number (match-string-no-properties 0) 16))
            (if (and (<= #xD800 high-surrogate #xDBFF)
                     (save-excursion
                       (goto-char match-end)
                       (looking-at "\\\\u\\([0-9a-zA-Z]\\{4\\}\\)")))
                ;; This may be a surrogate pair; proceeding.
                (progn
                  (setq match-end (match-end 0))
                  (setq low-surrogate
                        (string-to-number (match-string-no-properties 1) 16))
                  (if (<= #xDC00 low-surrogate #xDFFF)
                      ;; This is actually a surrogate pair.
                      (setq replacement
                            (string (logior
                                     #x10000
                                     (ash (- high-surrogate #xD800) 10)
                                     (- low-surrogate #xDC00))))
                    ;; Not a surrogate pair; go back to the first sequence
                    (setq match-end (- match-end 6))
                    (setq replacement (string high-surrogate))))
              ;; Not a surrogate pair
              (setq replacement (string high-surrogate)))
            (goto-char match-beginning)
            (delete-region match-beginning match-end)
            (insert-before-markers replacement))

           ;; Invalid sequence; leave it as is
           (t nil))))
      (json-par--free-marker end))))

(push #'json-par-unescape-region-for-string json-par--fixup-adviced-functions)

(defvar-local json-par--protection-markers nil
  "List of markers protecting commas from fixing up.

Markers are placed before new commas or open brackets before them.

Commas with markers or commas immediately following the commas, excluding
spaces and newlinews, are protected.

When a value is inserted after the comma or the open bracket, its maker is
removed.")

(defun json-par--add-protection-marker (&optional position)
  "Add marker at POSITION to the list of protection markers.

If POSITION is omitted or nil, the point is used.

See also `json-par--protection-markers'."
  (let ((marker (copy-marker (or position (point)))))
    (set-marker-insertion-type marker t)
    (push marker json-par--protection-markers)))

(defun json-par--record-protection-markers-to-undo-list
    (&optional protection-markers)
  "Record current PROTECTION-MARKERS to the undo list.

If PROTECTION-MARKERS is omitted, default to `json-par--protection-markers'."
  (push (list
         'apply
         (lambda (protection-markers)
           (setq json-par--protection-markers protection-markers))
         (or protection-markers json-par--protection-markers))
        buffer-undo-list))

(defvar electric-indent-mode)

(defun json-par-insert-comma ()
  "Insert a comma before or after the current member.

Insert spaces and line breaks if needed.

If insertion of a comma results in a place suitable for inserting a value, move
to it and return the point.

Return nil otherwise.

Details:

- Inside an empty brackets, or between two commas or other places expecting a
  value (but not before/after a colon):

  Insert a comma and place the point before the comma.  Return the point.

  Examples (`|' is the point):

  [|]
  ↓
  [ |, ]

  [ |, 1 ]
  ↓
  [ |, , 1 ]

  [ 1, | ]
  ↓
  [ 1, |, ]

  [ 1, |, 2 ]
  ↓
  [ 1, |, , 2 ]

- Between values, value and object key, colon and object key, or colons:

  Insert a comma after the previous token and return nil.  Keep position unless
  the point is just after the new comma.  If so, insert a space or forward a
  character if the next character is a space.

  Examples:

  [ 1| 2 ]
  ↓
  [ 1, |2 ]

  {
    \"a\": 1
    |\"b\": 2
  }
  ↓
  {
    \"a\": 1,
    |\"b\": 2
  }

  {
    \"a\":
    |\"b\":
  }
  ↓
  {
    \"a\": ,
    |\"b\":
  }


- Before close bracket or comma (or the point is on a token before close bracket
  or comma):

  Insert a comma after the previous token and return the point.  Keep position
  unless the point is just after the new comma.  If so, insert a space or a line
  break, or forward a character if the next character is a space.

  If the point was just before the close bracket, insert empty line before the
  bracket.

  Examples:

  [ 1| ]
  ↓
  [ 1, | ]

  [ \"a|bc\" ]
  ↓
  [ \"abc\", | ]

  [
    1
  |]
  ↓
  [
    1,
    |
  ]

  {
    \"a\": 1|,
    \"b\": 2
  }
  ↓
  {
    \"a\": 1,
    |,
    \"b\": 2
  }

- Otherwise (before or after a colon, or before some value or object key):

  Insert a comma just before the current member and insert a space or a line
  break if needed.

  Move to the position before the comma and return the point.

  Examples:

  [ |1 ]
  ↓
  [ |, 1  ]

  {
    \"a\": |1
  }
  ↓
  {
    |,
    \"a\": 1,
  }

  {
    \"a|aa\": 1
  }
  ↓
  {
    |,
    \"aaa\": 1,
  }

  {
    \"a\": 1,|
    \"b\": 2
  }
  ↓
  {
    \"a\": 1,
    |,
    \"b\": 2
  }

  {
    \"a\": 1,
    |\"b\": 2
  }
  ↓
  {
    \"a\": 1,
    |,
    \"b\": 2
  }"
  (interactive)
  (json-par--out-comment)
  (json-par--out-atom)
  (let* ((electric-indent-mode nil)
         (next-token (save-excursion (json-par-forward-token)))
         (previous-token (save-excursion (json-par-backward-token)))
         (parent-token (json-par--parent-token))
         (next-is-object-key (json-par--object-key-p next-token t))
         (previous-is-object-key (json-par--object-key-p previous-token))
         (next-is-value
          (and (or (json-par-token-atom-p next-token)
                   (json-par-token-open-bracket-p next-token))
               (not next-is-object-key)))
         (previous-is-value
          (and (or (json-par-token-atom-p previous-token)
                   (json-par-token-close-bracket-p previous-token))
               (not previous-is-object-key)))
         (one-line
          (json-par--same-line-p (json-par-token-start parent-token)
                                 (json-par-token-start next-token)))
         (insert-newline-after-comma
          (and (not one-line)
               (not (json-par--multiple-members-on-same-line-around-point-p
                     3))))
         value-placeholder-marker)
    (when (or (and (json-par-token-colon-p next-token)
                   previous-is-object-key)
              (and (json-par-token-colon-p previous-token)
                   next-is-value))
      (json-par-beginning-of-member))
    (when (memq (char-before) '(?\[ ?\( ?{ ?\,))
      (insert-char ?\s)
      (when (and (json-par-token-comma-p previous-token)
                 insert-newline-after-comma)
        (newline-and-indent)))
    (when (memq (char-after) '(?} ?\) ?\]))
      (if one-line
          (save-excursion (insert-char ?\s))
        (json-par--open-line-and-indent-both)))
    (cond
     ;; Inside an empty brackets, or between two commas or other places
     ;; expecting a value (but not before/after a colon)
     ((and (memq (json-par-token-type previous-token)
                 '({ \( \[ \, outside-of-buffer))
           (memq (json-par-token-type next-token)
                 '(} \) \] \, outside-of-buffer)))
      (when (and (eq (char-after) ?\,)
                 insert-newline-after-comma)
        (json-par--open-line-and-indent-both))
      (setq value-placeholder-marker (point-marker))
      (json-par--insert-comma-after-point insert-newline-after-comma))

     ;; Between values, value and object key, colon and object key, or colons
     ((or (and previous-is-value next-is-value)
          (and (or previous-is-value
                   (json-par-token-colon-p previous-token))
               (or next-is-object-key
                   (json-par-token-colon-p next-token))))
      (json-par--insert-comma-after-previous-token)
      (when (eq (char-before) ?\,)
        (if (memq (char-after) '(?\s ?\t))
            (forward-char)
          (unless (eq (char-after) ?\n)
            (insert-char ?\s))))
      (setq value-placeholder-marker nil))

     ;; Before close bracket or comma
     ((or (json-par-token-close-bracket-p next-token)
          (json-par-token-comma-p next-token)
          (json-par-token-outside-of-buffer-p next-token))
      (json-par--insert-comma-after-previous-token)
      (when (eq (char-before) ?\,)
        (insert-char ?\s)
        (when insert-newline-after-comma (newline-and-indent)))
      (when (and (json-par-token-close-bracket-p next-token)
                 (not one-line))
        (json-par-indent-line))
      (setq value-placeholder-marker (point-marker))
      (when (memq (char-after) '(?} ?\) ?\]))
        (save-excursion (insert-char ?\s))))

     ;; Otherwise (before or after a colon, or before some value or object key)
     (t
      (setq value-placeholder-marker (point-marker))
      (json-par--insert-comma-after-point insert-newline-after-comma)))
    (when value-placeholder-marker
      (save-excursion
        (let (token)
          (json-par--record-protection-markers-to-undo-list)
          (while (json-par-token-comma-p
                  (setq token (json-par-forward-token)))
            t)
          (goto-char (json-par-token-start token))
          (unless (or (json-par-token-close-bracket-p token)
                      (json-par-token-outside-of-buffer-p token))
            (setq token (json-par-backward-token)))
          (while (json-par-token-comma-p
                  (setq token (json-par-backward-token)))
            (json-par--add-protection-marker))
          (when (or (json-par-token-open-bracket-p token)
                    (json-par-token-outside-of-buffer-p token))
            (json-par--add-protection-marker))))
      (json-par--free-marker value-placeholder-marker))))

(push #'json-par-insert-comma json-par--fixup-adviced-functions)

(defun json-par--open-line-and-indent-both ()
  "Insert a newline and indent both lines.

Delete trailing spaces of the first line.

This is almost identical to `newline-and-indent' but handles indentation well:
  (‘|’ is the point)

  [
  ,|]

  ↓ (save-excursion (newline-and-indent))

  [
  ,|
  ]

  ↓ (indent-according-to-mode)

  [
      ,|
  ]

Also similar to `reindent-then-newline-and-indent', but keep the point on
original line."
  (save-excursion
    (delete-horizontal-space t)
    ;; Not using `newline-and-indent' to suppress `post-self-insert-hook'.
    (newline)
    (json-par-indent-line))
  (json-par-indent-line))

(defun json-par--insert-comma-after-point (insert-newline-after-comma)
  "Insert a comma after the point.

If INSERT-NEWLINE-AFTER-COMMA is non-nil and not at the end of a line, insert a
line break and indent the line."
  (save-excursion
    (insert-char ?\,)
    (unless (memq (char-after) '(?\s ?\t ?\n))
      (insert-char ?\s))
    (when (and insert-newline-after-comma
               (not (eq (char-after) ?\n)))
      (newline-and-indent))))

(defun json-par--insert-comma-after-previous-token ()
  "Insert a comma before the previous token.

If the previous token is a colon, keep one space after it."
  (save-excursion
    (json-par--backward-spaces)
    (when (eq (char-before) ?:)
      (insert-before-markers " ")
      (when (memq (char-after) '(?\s ?\t))
        (delete-char 1)))
    (insert-before-markers ",")))

(defun json-par-insert-colon ()
  "Insert a colon before or after the current member.

- If the point is before a colon, move after it.

- If the point is after a key without a colon, insert a colon.

- Otherwise, insert a empty key and colon before/after the member.
  See `json-par--insert-key' for details."
  (interactive)
  (json-par--out-comment)
  (json-par--out-atom)
  (let* ((next-token (save-excursion (json-par-forward-token)))
         (previous-token (save-excursion (json-par-backward-token)))
         (parent-token (json-par--parent-token))
         (next-is-object-key
          (json-par--object-key-p next-token t))
         (next-is-value
          (or (json-par-token-atom-p next-token)
              (json-par-token-open-bracket-p next-token)))
         (one-line
          (<= (save-excursion (goto-char (json-par-token-start next-token))
                              (line-beginning-position))
              (json-par-token-end parent-token))))
    (cond
     ;; Before a colon
     ((json-par-token-colon-p next-token)
      (forward-char)
      (if next-is-value
          (skip-chars-forward "\s\t\n")
        (when (memq (char-after) '(?\s ?\t))
          (forward-char))))

     ;; After a key
     ((and (json-par-token-string-p previous-token)
           (save-excursion
             (json-par-beginning-of-member)
             (= (point) (json-par-token-start previous-token))))
      (save-excursion
        (json-par--backward-spaces)
        (insert-char ?:))
      (when (and (eq (char-before) ?:)
                 (not (memq (char-after)
                            '(?\s ?\t ?\n))))
        (insert-char ?\s))
      (if next-is-value
          (skip-chars-forward "\s\t\n")
        (when (and (eq (char-before) ?:)
                   (memq (char-after) '(?\s ?\t)))
          (forward-char))))

     ;; Otherwise
     (t
      (when next-is-object-key
        (json-par-insert-comma))
      (json-par--insert-key "\"\"" previous-token next-token)
      (forward-char)
      (when (and (eq (char-after) ?\n) (not next-is-value))
        (insert-char ?\s))
      (when (memq (char-after) '(?\s ?\t))
        (forward-char))
      (when (memq (char-after) '(?} ?\) ?\]))
        (if one-line
            (save-excursion (insert-char ?\s))
          (json-par--open-line-and-indent-both)
          (insert-char ?\s)))
      (when next-is-value
        (skip-chars-forward "\s\t\n"))))))

(push #'json-par-insert-colon json-par--fixup-adviced-functions)

(defun json-par-insert-reverse-solidus ()
  "Insert a reverse solidus.

Inhibit fixup until next modification."
  (interactive)
  (insert-char ?\\)
  (setq json-par--inhibit-fixup-tick (buffer-chars-modified-tick)))

(provide 'json-par-insert)

;;; json-par-insert.el ends here
