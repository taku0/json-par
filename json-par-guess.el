;;; json-par-guess.el --- Inserting guessed values/keys in JSON Par mode -*- lexical-binding: t -*-

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

;; Inserting guessed values or keys in JSON Par mode like `dabbrev-expand'.

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'subr-x))
(require 'json)
(require 'json-par-lexer)
(require 'json-par-motion)
(require 'json-par-insert)


;;; Customizations

(defcustom json-par-guess-max-ancestors 2
  "Max number of ancestors to search in `json-par-insert-guessed'.

For example, if this value is 2 and the point is `|' below, keys \"c\" and \"b\"
below are searched in buffers.

{
  \"a\": [
    {
      \"b\": [
        [
          {
            \"c\": |
          }
        ]
      ]
    }
  ]
}"
  :type 'integer
  :group 'json-par
  :safe 'integerp)

(defcustom json-par-friend-buffer-function nil
  "A function to limit buffers searched by `json-par-insert-guessed'.

A buffer with `json-mode' is passed to the function, and it should return
non-nil if and only if `json-par-insert-guessed' should search the buffer.

If this variable is nil, all buffers with `json-mode' are searched.

The current buffer is always searched and not passed to this function."
  :type '(choice function (const nil))
  :group 'json-par
  :safe (lambda (v) (or (null v) (functionp v))))


;;; State

(defun json-par--guess-state
    (target
     key-token
     depth
     sibling-keys
     max-ancestors
     inserted-start
     inserted-end
     searched-start
     searched-end
     json-buffers
     pending-buffers
     candidates
     seen-values)
  "Construct and return state object for `json-par-insert-guessed'.

TARGET: What to guess: `value', `key', or `member'.

KEY-TOKEN: The token object of the object key of the current context.

DEPTH: The depth (number of open brackets) between the point and the key.

SIBLING-KEYS: A list of key strings in the current object.

MAX-ANCESTORS: The max number of ancestors to search.

INSERTED-START: The start of the last inserted text.

INSERTED-END: The end of the last inserted text.

SEARCHED-START: The start of the range searched for the key.

SEARCHED-END: The end of the range searched for the key.

JSON-BUFFERS: A list of the JSON buffers.  The head is the current buffer.

PENDING-BUFFERS: A list of the buffers not searched yet.

CANDIDATES: A list of the start position of the candidates under the last
searched key.

SEEN-VALUES: A list of the values or members tried so far."
  (vector
   target
   key-token
   depth
   sibling-keys
   max-ancestors
   inserted-start
   inserted-end
   searched-start
   searched-end
   json-buffers
   pending-buffers
   candidates
   seen-values))

(defun json-par--guess-state-target (state)
  "Return target of STATE."
  (elt state 0))

(defun json-par--guess-state-key-token (state)
  "Return key-token of STATE."
  (elt state 1))

(defun json-par--guess-state-depth (state)
  "Return depth of STATE."
  (elt state 2))

(defun json-par--guess-state-sibling-keys (state)
  "Return sibling-keys of STATE."
  (elt state 3))

(defun json-par--guess-state-max-ancestors (state)
  "Return max-ancestors of STATE."
  (elt state 4))

(defun json-par--guess-state-inserted-start (state)
  "Return inserted-start of STATE."
  (elt state 5))

(defun json-par--guess-state-inserted-end (state)
  "Return inserted-end of STATE."
  (elt state 6))

(defun json-par--guess-state-searched-start (state)
  "Return searched-start of STATE."
  (elt state 7))

(defun json-par--guess-state-searched-end (state)
  "Return searched-end of STATE."
  (elt state 8))

(defun json-par--guess-state-json-buffers (state)
  "Return json-buffers of STATE."
  (elt state 9))

(defun json-par--guess-state-pending-buffers (state)
  "Return pending-buffers of STATE."
  (elt state 10))

(defun json-par--guess-state-candidates (state)
  "Return preceding-candidates of STATE."
  (elt state 11))

(defun json-par--guess-state-seen-values (state)
  "Return seen-values of STATE."
  (elt state 12))

(defun json-par--guess-state-updated (state &rest new-values)
  "Return the updated `json-par--guess-state'.

STATE is the old state.

NEW-VALUES is a plist of new values.  Its keys are slot name prefixed with a
colon."
  (let* ((keys [:target
                :key-token
                :depth
                :sibling-keys
                :max-ancestors
                :inserted-start
                :inserted-end
                :searched-start
                :searched-end
                :json-buffers
                :pending-buffers
                :candidates
                :seen-values])
         (updated (make-vector (length keys) nil)))
    (dotimes (i (length keys))
      (aset updated
            i
            (let ((cell (plist-member new-values (elt keys i))))
              (if cell (cadr cell) (elt state i)))))
    updated))

(defvar-local json-par--last-state nil
  "The last state of `json-par--guess-state'.")

(defun json-par--guess-state-initialize (max-ancestors)
  "Return a new `json-par--guess-state'.

It is initialized as suitable for a fresh invocation of
`json-par-insert-guessed'.

MAX-ANCESTORS is the max number of ancestors to search."
  (save-excursion
    (json-par--out-comment)
    (json-par--out-atom)
    (let* ((json-par--already-out-of-comment t)
           (json-par--already-out-of-atom t)
           (previous-token (save-excursion
                             (json-par-backward-token)))
           (next-token (save-excursion
                         (json-par-forward-token)))
           (start-of-member (save-excursion
                              (json-par-beginning-of-member)
                              (point)))
           (end-of-member (save-excursion
                            (goto-char start-of-member)
                            (json-par-end-of-member)
                            (point)))
           key-token-and-depth
           key-token
           (depth 0)
           (json-buffers
            (cons
             (current-buffer)
             (delq
              nil
              (mapcar
               (lambda (buffer)
                 (when (and (not (eq (current-buffer) buffer))
                            (with-current-buffer buffer
                              (eq major-mode 'json-mode))
                            (or (null json-par-friend-buffer-function)
                                (funcall
                                 json-par-friend-buffer-function
                                 buffer)))
                   buffer))
               (buffer-list)))))
           position-in-member)
      (cond
       ((and (json-par-token-colon-p previous-token)
             (memq (json-par-token-type next-token)
                   '(} \) \] \, outside-of-buffer)))
        (setq key-token (save-excursion
                          (goto-char start-of-member)
                          (json-par-forward-token)))
        (when (json-par--object-key-p key-token)
          (json-par--guess-state-updated
           nil
           :target 'value
           :key-token key-token
           :depth depth
           :sibling-keys ()
           :max-ancestors max-ancestors
           :inserted-start (point)
           :inserted-end (point)
           :searched-start start-of-member
           :searched-end end-of-member
           :json-buffers json-buffers
           :pending-buffers json-buffers
           :candidates ()
           :seen-values ())))

       (t
        (setq key-token-and-depth (json-par--parent-key))
        (setq key-token (nth 0 key-token-and-depth))
        (setq depth (nth 1 key-token-and-depth))
        (save-excursion
          (setq position-in-member (json-par--position-in-member))
          (if (eq position-in-member 'after-member)
              (progn
                (json-par--forward-spaces)
                (skip-chars-backward "\s\t\n"))
            (goto-char start-of-member)
            (json-par--backward-spaces)
            (skip-chars-forward "\s\t\n"))
          (json-par--guess-state-updated
           nil
           :target (if (json-par-token-open-curly-bracket-p
                        (json-par--parent-token))
                       'key
                     'member)
           :key-token key-token
           :depth depth
           :sibling-keys
           (save-excursion
             (json-par-up-backward)
             (delq nil
                   (mapcar (lambda (pos)
                             (goto-char pos)
                             (let ((token (json-par-forward-token)))
                               (when (json-par--object-key-p token)
                                 (json-par--read-token token))))
                           (json-par--locate-all-members 1))))
           :max-ancestors max-ancestors
           :inserted-start (point)
           :inserted-end (point)
           :searched-start (or (json-par-token-start key-token)
                               (point-min))
           :searched-end (or (json-par-token-start key-token)
                             (point-min))
           :json-buffers json-buffers
           :pending-buffers json-buffers
           :candidates ()
           :seen-values ())))))))


;;; Functions

(defun json-par--parent-key ()
  "Return the key of the parent array/object.

If the parent don't have a key, its ancestors are searched recursively.

Return a list (KEY-TOKEN DEPTH) where KEY-TOKEN is the token object of the key,
and DEPTH is the number of open brackets between the key and the point.

If any ancestors don't have a key, return a list (nil DEPTH)."
  (save-excursion
    (let ((key-token nil)
          (depth 0))
      (while (and (null key-token)
                  (not (bobp)))
        (setq depth (1+ depth))
        (json-par-up-backward)
        (json-par-beginning-of-member)
        (setq key-token (save-excursion (json-par-forward-token)))
        (unless (json-par--object-key-p key-token)
          (setq key-token nil))
        (json-par--backward-spaces))
      (list key-token depth))))

(defun json-par--locate-all-members (depth &optional result)
  "Return a list of the positions of descendants those are DEPTH levels away.

If the DEPTH is 0, the current position is returned if the buffer is not empty.
If the buffer is empty, return an empty list.

If RESULT is given, the positions are prepended to it."
  (if (zerop depth)
      (if (and (save-excursion
                 (json-par--backward-spaces)
                 (bobp))
               (save-excursion
                 (json-par--forward-spaces)
                 (eobp)))
          result
        (cons (point) result))
    (json-par-beginning-of-object-value)
    (if (memq (char-after) '(?\[ ?\( ?{))
        (progn
          (forward-char)
          (json-par--find-member-forward
           (lambda (_)
             (setq result (json-par--locate-all-members (1- depth) result))
             nil))
          result)
      result)))

(defun json-par-insert-guessed (&optional max-ancestors last-state save-state)
  "Insert a guessed value/key like `dabbrev-expand'.

If the point is after a key and colon, and the value is missing, guess a value
from the key.  Otherwise, guess a key (if inside an object) or member (if inside
an array) from the key of the parent.

The nearest value/key/member from the point, which is under the searching key,
is inserted at the point and indented.  If the command is repeated, the inserted
text is replaced with the next candidate.

If all candidates in this buffer are tried, other buffers with `json-mode' are
searched.

If all buffers are searched, the key of ancestors are searched from this buffer
and other `json-mode' buffers, up to MAX-ANCESTORS keys, including the first
key.  MAX-ANCESTORS is meaningful only for the first invocation of successive
invocations.

When all candidates are tried, remove the inserted text.  Note that when
inserting a member, a comma may be left after the previous member.  This
behavior may be fixed in future.

Return a state object (`json-par--guess-state').  When the state is passed as
LAST-STATE, resume the search from the state.  If SAVE-STATE is non-nil, the
updated state is saved to the variable `json-par--last-state'.  When called
interactively, the state is saved to the variable and resumed on the next
invocation."
  (interactive
   (if (eq last-command 'json-par-insert-guessed)
       (list json-par-guess-max-ancestors json-par--last-state t)
     (list json-par-guess-max-ancestors nil t)))
  (when last-state
    (delete-region
     (json-par--guess-state-inserted-start last-state)
     (json-par--guess-state-inserted-end last-state))
    (goto-char (json-par--guess-state-inserted-start last-state)))
  (unless last-state
    (setq last-state (json-par--guess-state-initialize max-ancestors)))
  (let* ((target (json-par--guess-state-target last-state))
         (guessed
          (cond
           ((eq target 'value)
            (json-par--guess-value last-state))

           ((eq target 'key)
            (json-par--guess-key last-state))

           ((eq target 'member)
            (json-par--guess-member last-state))))
         (guessed-text (nth 0 guessed))
         (new-state (nth 1 guessed))
         inserted-start
         inserted-end)
    (if guessed
        (progn
          (setq inserted-start (point-marker))
          (cond
           ((eq target 'value)
            (insert guessed-text))

           ((eq target 'key)
            (json-par--insert-value guessed-text t t))

           ((eq target 'member)
            (json-par--insert-value guessed-text nil t)))
          (unless (json-par--same-line-p inserted-start (point))
            (json-par-indent-region inserted-start (point)))
          (setq inserted-start
                (save-excursion
                  (goto-char (json-par--free-marker inserted-start))
                  (skip-chars-forward "\s\t\n")
                  (point)))
          (setq inserted-end (point))
          (setq new-state (json-par--guess-state-updated
                           new-state
                           :inserted-start inserted-start
                           :inserted-end inserted-end))
          (when save-state
            (setq json-par--last-state new-state))
          new-state)
      (when (called-interactively-p 'interactive)
        (message
         (cond
          ((eq target 'value)
           "Cannot guess value")

          ((eq target 'key)
           "Cannot guess key")

          ((eq target 'member)
           "Cannot guess member"))))
      (when save-state
        (setq json-par--last-state nil))
      nil)))

(defun json-par--guess-next
    (last-state guess-from-candidates &optional extract-candidates)
  "Return a guessed value/key/member from LAST-STATE.

Return a list (TEXT UPDATED-STATE) where TEXT is the string of the guessed
value/key/member, and UPDATED-STATE is the updated state.

GUESS-FROM-CANDIDATES is a function which return a guessed value/key/member from
the candidates in the LAST-STATE.  It is called with a state and should return a
list (TEXT UPDATED-STATE), or nil if not found.

EXTRACT-CANDIDATES is a function which return a list of the positions of the
candidates under the point, which is located at the found key.  The function is
called with LAST-STATE.

If no value/key/member is found, return nil."
  (save-excursion
    (let ((result nil)
          (done nil)
          (pos (point)))
      (unless extract-candidates
        (setq extract-candidates (lambda (last-state)
                                   (sort
                                    (json-par--locate-all-members
                                     (json-par--guess-state-depth last-state))
                                    (lambda (pos1 pos2)
                                      (< (abs (- pos1 pos))
                                         (abs (- pos2 pos))))))))
      (while (not done)
        (setq result (funcall guess-from-candidates last-state))
        (if result
            (setq done t)
          (setq last-state
                (json-par--guess-find-candidates
                 last-state
                 pos
                 extract-candidates))
          (unless last-state
            (setq done t))))
      result)))

(defun json-par--guess-find-candidates (last-state pos extract-candidates)
  "Find the candidates from LAST-STATE and return the updated state.

POS is the point where the searching is started.

For details of EXTRACT-CANDIDATES, see `json-par--guess-next'."
  (let* ((key-token (json-par--guess-state-key-token last-state))
         (key (and key-token
                   (json-par-token-text-no-properties key-token)))
         (depth (json-par--guess-state-depth last-state))
         (searched-start (json-par--guess-state-searched-start last-state))
         (searched-end (json-par--guess-state-searched-end last-state))
         (pending-buffers (json-par--guess-state-pending-buffers last-state))
         json-buffers
         max-ancestors
         preceding-match-key
         following-match-key
         key-token-and-depth
         new-key-token
         first-token)
    (with-current-buffer (car pending-buffers)
      (when key
        (while (and (not preceding-match-key)
                    (not following-match-key)
                    (or (< (point-min) searched-start)
                        (< searched-end (point-max))))
          (goto-char searched-start)
          (if (search-backward key nil t)
              (progn
                (setq searched-start (point))
                (setq first-token (json-par-forward-token))
                (when (json-par--object-key-p first-token)
                  (setq preceding-match-key first-token)))
            (setq searched-start (point-min)))
          (goto-char searched-end)
          (if (search-forward key nil t)
              (progn
                (setq searched-end (point))
                (setq first-token (json-par-backward-token))
                (when (json-par--object-key-p first-token)
                  (setq following-match-key first-token)))
            (setq searched-end (point-max)))))
      (cond
       ((and (null key)
             (or (< (point-min) searched-start)
                 (= searched-end (point-min)))
             (not (and (save-excursion
                         (json-par--backward-spaces)
                         (bobp))
                       (save-excursion
                         (json-par--forward-spaces)
                         (eobp)))))
        (goto-char (point-min))
        (json-par--guess-state-updated
         last-state
         :candidates (funcall extract-candidates last-state)
         :searched-start (point-min)
         :searched-end (point-max)))

       ((and (null preceding-match-key) (null following-match-key))
        (if (cdr pending-buffers)
            (progn
              (pop pending-buffers)
              (with-current-buffer (car pending-buffers)
                (json-par--guess-state-updated
                 last-state
                 :candidates ()
                 :pending-buffers pending-buffers
                 :searched-start (point-min)
                 :searched-end (point-min))))
          (setq json-buffers (json-par--guess-state-json-buffers last-state))
          (setq max-ancestors (json-par--guess-state-max-ancestors last-state))
          (when (and (< 1 max-ancestors) key)
            (with-current-buffer (car json-buffers)
              (goto-char (json-par-token-start key-token))
              (setq key-token-and-depth (json-par--parent-key))
              (setq new-key-token (nth 0 key-token-and-depth))
              (when new-key-token
                (json-par--guess-state-updated
                 last-state
                 :key-token new-key-token
                 :depth (+ depth (nth 1 key-token-and-depth))
                 :max-ancestors (1- max-ancestors)
                 :candidates ()
                 :pending-buffers json-buffers
                 :searched-start (point-min)
                 :searched-end (point-min)))))))

       ((or (null following-match-key)
            (and preceding-match-key
                 (< (- pos (json-par-token-end preceding-match-key))
                    (- (json-par-token-start following-match-key) pos))))
        (when following-match-key
          (setq searched-end (json-par-token-start following-match-key)))
        (goto-char (json-par-token-start preceding-match-key))
        (json-par--guess-state-updated
         last-state
         :candidates (funcall extract-candidates last-state)
         :searched-start searched-start
         :searched-end searched-end))

       (t
        (when preceding-match-key
          (setq searched-start (json-par-token-end preceding-match-key)))
        (goto-char (json-par-token-start following-match-key))
        (json-par--guess-state-updated
         last-state
         :candidates (funcall extract-candidates last-state)
         :searched-start searched-start
         :searched-end searched-end))))))

(defun json-par--normalize-guessed-text (text)
  "Normalize the TEXT of the guessed value/member for deduplication."
  (string-trim (replace-regexp-in-string "^[\s\t]+" "" text)))

(defun json-par--guess-value (last-state)
  "Return a guessed value from LAST-STATE.

See `json-par--guess-next' for details."
  (json-par--guess-next last-state #'json-par--guess-value-from-candidates))

(defun json-par--guess-value-from-candidates (last-state)
  "Return a guessed value from the candidates in LAST-STATE.

See `json-par--guess-next' for details."
  (save-excursion
    (let ((candidates (json-par--guess-state-candidates last-state))
          (value-token nil)
          (seen-values (json-par--guess-state-seen-values last-state))
          (buffer (car (json-par--guess-state-pending-buffers last-state)))
          text)
      (with-current-buffer buffer
        (while (and candidates (null value-token))
          (goto-char (pop candidates))
          (json-par-beginning-of-object-value)
          (setq value-token (json-par-forward-token-or-list))
          (unless (json-par--guess-eligible-value-p value-token seen-values)
            (setq value-token nil)))
        (when value-token
          (setq text (json-par--normalize-guessed-text
                      (json-par-token-text-no-properties value-token)))
          (list
           text
           (json-par--guess-state-updated
            last-state
            :candidates candidates
            :seen-values (cons text seen-values))))))))

(defun json-par--guess-eligible-value-p (value-token seen-values)
  "Return non-nil if the VALUE-TOKEN is an eligible value for guessing.

SEEN-VALUES is a list of text inserted before."
  (and (or (json-par-token-atom-p value-token)
           (json-par-token-matching-brackets-p value-token))
       (let ((text (json-par--normalize-guessed-text
                    (json-par-token-text-no-properties value-token))))
         (and (not (string-empty-p text))
              (not (member text seen-values))))))

(defun json-par--guess-key (last-state)
  "Return a guessed key from LAST-STATE.

See `json-par--guess-next' for details."
  (json-par--guess-next
   last-state
   #'json-par--guess-key-from-candidates
   #'json-par--extract-candidate-keys))

(defun json-par--extract-candidate-keys (last-state)
  "Return a list of the locations of the candidates for guessing keys.

LAST-STATE is a `json-par--guess-state'.

See `json-par--guess-next' for details."
  (let* ((sibling-keys (json-par--guess-state-sibling-keys last-state))
         (pos (json-par--guess-state-inserted-start last-state))
         (parent-locations (sort
                            (json-par--locate-all-members
                             (1- (json-par--guess-state-depth last-state)))
                            (lambda (pos1 pos2)
                              (< (abs (- pos1 pos))
                                 (abs (- pos2 pos))))))
         (candidates '())
         member-locations
         key-token
         keys)
    (dolist (parent-location parent-locations)
      (save-excursion
        (goto-char parent-location)
        (json-par-beginning-of-object-value)
        (when (eq (char-after) '?{)
          (setq member-locations (reverse (json-par--locate-all-members 1)))
          (setq keys
                (delq
                 nil
                 (mapcar
                  (lambda (member-location)
                    (goto-char member-location)
                    (setq key-token (save-excursion (json-par-forward-token)))
                    (when (json-par--object-key-p key-token)
                      (json-par--read-token key-token)))
                  member-locations)))
          (when (cl-subsetp sibling-keys keys :test #'equal)
            (push member-locations candidates)))))
    (apply #'append (reverse candidates))))

(defun json-par--guess-key-from-candidates (last-state)
  "Return a guessed key from the candidates in LAST-STATE.

See `json-par--guess-next' for details."
  (save-excursion
    (let* ((candidates (json-par--guess-state-candidates last-state))
           (seen-values (json-par--guess-state-seen-values last-state))
           (sibling-keys (json-par--guess-state-sibling-keys last-state))
           (buffer (car (json-par--guess-state-pending-buffers last-state)))
           key-token
           (text nil))
      (with-current-buffer buffer
        (while (and candidates (null text))
          (goto-char (pop candidates))
          (setq key-token (save-excursion (json-par-forward-token)))
          (when (and (json-par--object-key-p key-token)
                     (not (member (json-par--read-token key-token)
                                  sibling-keys)))
            (setq text (concat (json-par-token-text-no-properties key-token)
                               ": "))
            (when (member text seen-values)
              (setq text nil)))))
      (when text
        (list
         text
         (json-par--guess-state-updated
          last-state
          :candidates candidates
          :seen-values (cons text seen-values)))))))

(defun json-par--guess-member (last-state)
  "Return a guessed member from LAST-STATE.

See `json-par--guess-next' for details."
  (json-par--guess-next last-state #'json-par--guess-member-from-candidates))

(defun json-par--guess-member-from-candidates (last-state)
  "Return a guessed member from the candidates in LAST-STATE.

See `json-par--guess-next' for details."
  (save-excursion
    (let ((candidates (json-par--guess-state-candidates last-state))
          (seen-values (json-par--guess-state-seen-values last-state))
          (sibling-keys (json-par--guess-state-sibling-keys last-state))
          (buffer (car (json-par--guess-state-pending-buffers last-state)))
          key-token
          (text nil))
      (with-current-buffer buffer
        (while (and candidates (null text))
          (goto-char (pop candidates))
          (setq key-token (save-excursion (json-par-forward-token)))
          (when (or (not (json-par--object-key-p key-token))
                    (not (member (json-par--read-token key-token)
                                 sibling-keys)))
            (setq text (json-par--normalize-guessed-text
                        (buffer-substring-no-properties
                         (point)
                         (save-excursion
                           (json-par-end-of-member)
                           (point)))))
            (when (or (string-empty-p text)
                      (member text seen-values))
              (setq text nil)))))
      (when text
        (list
         text
         (json-par--guess-state-updated
          last-state
          :candidates candidates
          :seen-values (cons text seen-values)))))))


(provide 'json-par-guess)

;;; json-par-guess.el ends here
