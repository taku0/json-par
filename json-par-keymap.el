;;; json-par-kemap.el --- Keymap helpers for JSON Par mode -*- lexical-binding: t -*-

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

;; Helper functions for keymaps for JSON Par mode.

;;; Code:

(require 'cl-lib)
(require 'json-par-lexer)

(defvar json-par-mode)

;; `skk-pre-command' clears partial kana inputs unless `this-command' is in
;; `skk-kana-cleanup-command-list'.
;;
;; We must defer to invoke `skk-pre-command' until commands defined by
;; `json-par-define-special-key-command' set `this-command'.
;;
;; We accomplish this by adding an advice to `skk-pre-command'.
;;
;; We also add a function to `skk-load-hook' to add advice when SKK is loaded
;; after JSON Par mode is enabled.

(defvar json-par--deferred-pre-command-hook-functions '(skk-pre-command)
  "Functions in `pre-command-hooks' that should be deferred by JSON Par mode.

Those functions are invoked by commands defined by
`json-par-define-special-key-command'.")

(defvar json-par--hooks-to-install-advices-to-deferred-hook-functions
  '(skk-load-hook)
  "List of hooks to add `json-par--install-advice-to-deferred-hook-functions'.")

(defun json-par-define-special-key (keymap key command &optional special-p)
  "Define KEY as wrapped version of COMMAND in KEYMAP.

COMMAND is wrapped with `json-par-define-special-key-command' with SPECIAL-P."
  (define-key keymap key
    (json-par-define-special-key-command command special-p)))

(defun json-par-define-special-key-command (command &optional special-p)
  "Define a command that invokes COMMAND when SPECIAL-P return non-nil.

Otherwise, it calls `self-insert-command' interactively.

It has a name COMMAND-if-special.

If SPECIAL-P is nil, `json-par--special-p' is used."
  (unless special-p
    (setq special-p #'json-par--special-p))
  (let ((function-name (intern (concat (symbol-name command) "-if-special"))))
    (put function-name 'json-par--special-key-command t)
    (defalias function-name
      (lambda ()
        (interactive)
        (let ((actual-command
               (if (funcall special-p)
                   command
                 (let ((json-par-mode nil))
                   (or (key-binding (this-single-command-keys))
                       #'undefined)))))
          (setq this-command actual-command)
          (dolist (fun json-par--deferred-pre-command-hook-functions)
            (when (member fun pre-command-hook)
              (funcall fun)))
          (call-interactively actual-command)))
      (concat "Call `" (symbol-name command) "' interactively if the buffer is "
              "special state.\n"
              "\n"
              "Call `self-insert-command' interactively otherwise."))
    function-name))

(defun json-par--special-p (&optional parser-state)
  "Return non-nil if the current buffer is \"special state\".

The current buffer is \"special state\" if and only if:

- the region is active, or
- the point is not in a string or a comment.

JSON Par mode interprets ordinal keys as a command like the vi editor if the
buffer is special state.  Otherwise, it inserts keys as is.

If PARSER-STATE is given, it is used instead of (syntax-ppss).
If PARSER-STATE is a number or a marker, use that position for (syntax-ppss)."
  (or
   (use-region-p)
   (not (json-par--string-like-beginning-position parser-state))))

(defun json-par--special-but-not-number-p (&optional parser-state)
  "Return non-nil if the current buffer is \"special state\" for number keys.

Return non-nil if and only if:

- the region is active, or
- the point is not in a string or a comment.
- the point is not in a number or just before/after a number.

If PARSER-STATE is given, it is used instead of (syntax-ppss).
If PARSER-STATE is a number or a marker, use that position for (syntax-ppss)."
  (and
   (json-par--special-p parser-state)
   (not (json-par-token-number-p (json-par--current-atom parser-state)))))

(defun json-par--special-for-e-p (&optional parser-state)
  "Return non-nil if the current buffer is \"special state\" for key `e'.

Return non-nil if and only if:

- the region is active, or
- the point is not in a string or a comment.
- the point is not in a number, just before a number, or the number contains
  a character `e'.

If PARSER-STATE is given, it is used instead of (syntax-ppss).
If PARSER-STATE is a number or a marker, use that position for (syntax-ppss)."
  (and
   (json-par--special-p parser-state)
   (let ((current-atom (json-par--current-atom parser-state)))
     (or
      (not (json-par-token-number-p current-atom))
      (eq (json-par-token-start current-atom) (point))
      (cl-find ?e (json-par-token-text-no-properties current-atom))
      (cl-find ?E (json-par-token-text-no-properties current-atom))))))

(defvar-local json-par--prefix-prompt nil
  "The prompt for the temporary keymap of `json-par-prefix-command'.")

(defvar-local json-par--prefix-help-form nil
  "The help form for the temporary keymap of `json-par-prefix-command'.")

(defvar-local json-par--prefix-keymap nil
  "The keymap installed by `json-par-prefix-command'.")

(defun json-par-prefix-command (arg prompt prefix-help-form keymap)
  "Install a temporary keymap KEYMAP as a prefix command.

ARG is passed to the commands bound in the keymap.

PROMPT is shown as a message if given.

PREFIX-HELP-FORM is a help form for the keymap and shown if the user hits \"?\"
key or other help keys."
  (setq prefix-arg arg)
  (setq json-par--prefix-prompt prompt)
  (when prompt
    (message prompt
             (if prefix-arg
                 (concat
                  " ("
                  (prin1-to-string prefix-arg)
                  ")")
               "")))
  (setq json-par--prefix-help-form prefix-help-form)
  (setq json-par--prefix-keymap keymap)
  (set-transient-map keymap))

(defun json-par-prefix-universal-argument (arg)
  "Start prefix argument while reading key after `json-par-prefix-command'.

See `universal-argument' and `universal-argument-more' for details and ARG."
  (interactive "P")
  (json-par-prefix-command
   (cond
    ((null arg)
     (list 4))
    ((consp arg)
     (list (* 4 (car arg))))
    ((eq arg '-)
     (list -4))
    (t
     arg))
   json-par--prefix-prompt
   json-par--prefix-help-form
   json-par--prefix-keymap))

(defun json-par-prefix-digit-argument (arg event)
  "Update prefix argument while reading key after `json-par-prefix-command'.

ARG is the current prefix argument.
EVENT is the event triggered this command.

See `digit-argument' for details."
  (interactive
   (list
    current-prefix-arg
    last-command-event))
  ;; This command may be called with n, C-n, M-n, or C-M-n, where n is a digit.
  (let* ((base-key (event-basic-type event))
         (number (- base-key ?0))
         (new-arg (cond
                   ((integerp arg)
                    (+
                     (* arg 10)
                     (if (< arg 0) (- number) number)))

                   ((eq arg '-)
                    (if (zerop number)
                        '-
                      (- number)))

                   ;; C-u C-2 is 2, not 8.
                   (t
                    number))))
    (json-par-prefix-command
     new-arg
     json-par--prefix-prompt
     json-par--prefix-help-form
     json-par--prefix-keymap)))

(defun json-par-prefix-negative-argument (arg)
  "Update prefix argument while reading key after `json-par-prefix-command'.

ARG is the current prefix argument.

See `negative-argument' for details."
  (interactive "P")
  (let* ((new-arg (cond
                   ((integerp arg) (- arg))
                   ;; C-- C-- C-- is '-
                   ((eq arg '-) nil)
                   ;; C-u C-- is '-, not -4
                   (t '-))))
    (json-par-prefix-command
     new-arg
     json-par--prefix-prompt
     json-par--prefix-help-form
     json-par--prefix-keymap)))

(defun json-par-prefix-default ()
  "Handle unknown event for `json-par-prefix-command'.

If the event is `C-g', call `keyboard-quit'.

If the event is `help-char', \"?\", or an element of `help-event-list', show a
help message if any.

Otherwise, signal `undefined' error."
  (interactive)
  (cond
   ;; Keyboard quit
   ((eq last-command-event ?\C-g)
    (keyboard-quit))

   ;; Help
   ((and (or (eq last-command-event help-char)
             (eq last-command-event ??)
             (memq last-command-event help-event-list))
         json-par--prefix-help-form)
    (let ((help-form json-par--prefix-help-form))
      (help-form-show)))

   ;; Others; undefined event
   (t
    (undefined))))

(cl-defun json-par--menu-item (command &key label help prefixes)
  "Return a menu item for COMMAND suitable for `easy-menu-define'.

LABEL and HELP are used as the label and help if given.

If LABEL is omitted, the name of the COMMAND without the \"json-par-\" prefix
is used.

If HELP is omitted, the first line of the docstring is used.

If PREFIXES is given, it should be a list of pairs (PREFIX-COMMAND
. PREFIX-KEYMAP) and PREFIX-COMMAND is a prefix command like
`json-par-clone-prefix-command' and PREFIX-KEYMAP is a prefix keymap like
`json-par-clone-prefix-map'.  It is used to show key sequence on the menu item."
  (unless label
    (setq label (mapconcat
                 (lambda (word)
                   (if (member word '("of" "or" "in" "to"))
                       word
                     (capitalize word)))
                 (split-string
                  (replace-regexp-in-string
                   "json-par-"
                   ""
                   (symbol-name command))
                  "-")
                 " ")))
  (unless help
    (setq help (car (split-string (documentation command) "\n"))))
  (let* ((special-key-command
          (intern (concat (symbol-name command) "-if-special")))
         (prefix-key-string (mapconcat
                             (lambda (prefix)
                               (let ((prefix-command (car prefix))
                                     (prefix-keymap (cdr prefix)))
                                 (concat
                                  "\\["
                                  (symbol-name prefix-command)
                                  "]"
                                  "\\<"
                                  (symbol-name prefix-keymap)
                                  ">")))
                             prefixes
                             ""))
         (keys (concat prefix-key-string
                       "\\["
                       (symbol-name (if (fboundp special-key-command)
                                        special-key-command
                                      command))
                       "]")))
    (vector label command :help help :keys keys)))

(defun json-par--format-command-help (keymap)
  "Return description of KEYMAP.

It contains the key sequence, command name, and the first line of the docstring
of the command for each bindings in keymap.

`json-par-prefix-default' is excluded from the description."
  (apply
   #'concat
   (mapcar
    (lambda (pair)
      (let ((key-sequence (car pair))
            (binding (cdr pair)))
        (if (eq binding 'json-par-prefix-default)
            ""
          (format
           "%s:\n  %s\n\n"
           (key-description key-sequence)
           (cond
            ((null binding)
             "nil")
            ((arrayp binding)
             (concat "keyboard macro: " (key-description binding)))
            ((commandp binding)
             (format "`%s'\n  %s"
                     (if (symbolp binding)
                         (symbol-name binding)
                       "(lambda)")
                     (car (split-string
                           (or (documentation binding)
                               "(not documented)")
                           "\n")))))))))
    (json-par--flatten-keymap keymap))))

(defun json-par--flatten-keymap (keymap &optional tail prefix seen-keymaps)
  "Return a list of key sequences and commands in KEYMAP.

Return a list of cons (KEY-SEQUENCE . BINDING) where KEY-SEQUENCE is a vector of
input events and BINDING is one of nil, vector of input events (keyboard macro),
or command (symbol or lambda).

Nested keymap (prefix commands) are flatten.

TAIL, PREFIX, and SEEN-KEYMAPS are internal variable for recursion.

TAIL is the accumulator for the result; the list of cons are prepended to it.

PREFIX is the key sequence so far.  It is a list in reversed order.

SEEN-KEYMAPS is a hash table to stop infinite recursion."
  (unless seen-keymaps
    (setq seen-keymaps (make-hash-table :test 'eq)))
  (if (gethash keymap seen-keymaps)
      tail
    (puthash keymap t seen-keymaps)
    (map-keymap
     (lambda (event binding)
       (setq tail (json-par--flatten-keymap-1
                   event
                   binding
                   tail
                   prefix
                   seen-keymaps)))
     keymap)
    tail))

(defun json-par--flatten-keymap-1 (event binding tail prefix seen-keymaps)
  "Process a mapping from EVENT to BINDING and return the updated list TAIL.

See `json-par--flatten-keymap' for TAIL, PREFIX, and SEEN-KEYMAPS."
  (cond
   ;; Terminal binding
   ((or (null binding)
        (commandp binding)
        (arrayp binding))
    (let ((key-sequence (vconcat (reverse (cons event prefix)))))
      (cons (cons key-sequence binding) tail)))

   ;; Nested keymap
   ((keymapp binding)
    (json-par--flatten-keymap binding tail (cons event prefix) seen-keymaps))

   ;; Indirection
   ((symbolp binding)
    (if (symbol-function binding)
        (json-par--flatten-keymap-1
         event
         (symbol-function binding)
         tail
         prefix
         seen-keymaps)
      tail))

   ;; Others; ignored
   (t tail)))

(defun json-par--supress-pre-command-hooks (fun &rest args)
  "Call FUN with ARGS only if `this-command' would call it later."
  (when (or (null json-par-mode)
            (null (get this-command 'json-par--special-key-command)))
    (apply fun args)))

(defun json-par--install-advice-to-deferred-hook-functions ()
  "Add an advice around pre command hook functions to defer."
  (dolist (fun json-par--deferred-pre-command-hook-functions)
    (when (fboundp fun)
      (advice-add fun :around #'json-par--supress-pre-command-hooks))))

(defun json-par--remove-advice-from-deferred-hook-functions ()
  "Remove the advice from pre command hook functions to defer."
  (dolist (fun json-par--deferred-pre-command-hook-functions)
    (when (fboundp fun)
      (advice-remove fun #'json-par--supress-pre-command-hooks))))

(defun json-par--add-install-advice-to-deferred-hook-functions-to-hooks ()
  "Add `json-par--install-advice-to-deferred-hook-functions' to hooks."
  (dolist (hook json-par--hooks-to-install-advices-to-deferred-hook-functions)
    (add-hook hook #'json-par--install-advice-to-deferred-hook-functions)))


(provide 'json-par-keymap)

;;; json-par-keymap.el ends here
