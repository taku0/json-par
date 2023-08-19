;;; json-par-ancestor-overlay.el --- Show ancestors in JSON Par mode -*- lexical-binding: t -*-

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

;; Apply special faces to ancestor members.  Also show ancestors out of window
;; at the top of the window.

;;; Code:

(require 'json-par-lexer)
(require 'json-par-motion)

(defvar json-par-mode)


;;; Customization

(defface json-par-current-member-face
  '((((class color) (min-colors 88) (background light))
     :bold t)
    (((class color) (min-colors 88) (background dark))
     :bold t)
    (((class color) (min-colors 16) (background light))
     :background "brightyellow" :bold t)
    (((class color) (min-colors 16) (background dark))
     :background "yellow" :bold t)
    (((class color) (min-colors 8))
     :background "yellow" :bold t)
    (t :inverse-video t :bold t))
  "Face used on current member in JSON Par mode."
  :group 'json-par)

(defface json-par-ancestor-out-of-window-face
  '((t (:inherit (font-lock-keyword-face header-line) :extend t)))
  "Face used on ancestors out of the window in JSON Par mode."
  :group 'json-par)

(defface json-par-ancestor-face
  '((t :inherit json-par-current-member-face))
  "Face used on ancestors in JSON Par mode."
  :group 'json-par)

(defun json-par-set-show-ancestors-out-of-window (sym val)
  "Set default value of `json-par-show-ancestors-out-of-window' variable.

It is intended to :set function of `defcustom'.

SYM must be the symbol `json-par-show-ancestors-out-of-window'.  VAL is the new
value."
  (set-default sym val)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (boundp 'json-par-mode)
                 (buffer-local-value 'json-par-mode (current-buffer)))
        (json-par-enable-ancestors-out-of-window-overlay val)))))

(defcustom json-par-show-ancestors-out-of-window nil
  "If non-nil, show ancestors out of window.

It is shown with `json-par-ancestor-out-of-window-face'.

If `json-par-char-before-ancestor' is non-nil, it is shown before ancestors.

To set the value from Lisp code, call
`json-par-set-show-ancestors-out-of-window' to set default value, or call
`json-par-enable-ancestors-out-of-window-overlay' to set buffer local value."
  :type 'boolean
  :group 'json-par
  :safe #'booleanp
  :set #'json-par-set-show-ancestors-out-of-window)

;; :local is available only from 27.
(make-variable-buffer-local 'json-par-show-ancestors-out-of-window)

(defun json-par-set-highlight-ancestors (sym val)
  "Set default value of `json-par-highlight-ancestors' variable.

It is intended to :set function of `defcustom'.

SYM must be the symbol `json-par-highlight-ancestors'.  VAL is the new value."
  (set-default sym val)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (boundp 'json-par-mode)
                 (buffer-local-value 'json-par-mode (current-buffer)))
        (json-par-enable-ancestor-overlays val)))))

(defcustom json-par-highlight-ancestors nil
  "If non-nil, highlight ancestors of the member under the point.

It is highlighted with `json-par-ancestor-face'.

To set the value from Lisp code, call
`json-par-set-highlight-ancestors' to set default value, or call
`json-par-enable-ancestor-overlays' to set buffer local value."
  :type 'boolean
  :group 'json-par
  :safe #'booleanp
  :set #'json-par-set-highlight-ancestors)

;; :local is available only from 27.
(make-variable-buffer-local 'json-par-highlight-ancestors)

(defcustom json-par-char-before-ancestor nil
  "If non-nil, show this character before ancestors.

This takes effect only if `json-par-highlight-ancestors' is non-nil."
  :type '(choice (const nil) character)
  :group 'json-par
  :safe (lambda (v) (or (null v) (characterp v))))

(defun json-par-set-highlight-current-member (sym val)
  "Set default value of `json-par-highlight-current-member' variable.

It is intended to :set function of `defcustom'.

SYM must be the symbol `json-par-highlight-current-member'.  VAL is the new
value."
  (set-default sym val)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (boundp 'json-par-mode)
                 (buffer-local-value 'json-par-mode (current-buffer)))
        (json-par-enable-current-member-overlay val)))))

(defcustom json-par-highlight-current-member nil
  "If non-nil, highlight the member under the point.

It is highlighted with `json-par-current-member-face'.

To set the value from Lisp code, call
`json-par-set-highlight-current-member' to set default value, or call
`json-par-enable-current-member-overlay' to set buffer local value."
  :type 'boolean
  :group 'json-par
  :safe #'booleanp
  :set #'json-par-set-highlight-current-member)

;; :local is available only from 27.
(make-variable-buffer-local 'json-par-highlight-current-member)

(defcustom json-par-char-before-current-member nil
  "If non-nil, show this character before ancestors.

This takes effect only if `json-par-highlight-ancestors' is non-nil."
  :type '(choice (const nil) character)
  :group 'json-par
  :safe (lambda (v) (or (null v) (characterp v))))


;;; Show ancestors out of window

(defun json-par-enable-ancestors-out-of-window-overlay (&optional enable)
  "Enable showing ancestors out of windows.

Ancestors of the beginning of the window is shown with
`json-par-ancestor-out-of-window-face' before the beginning of the window.

With prefix arg or if ENABLE is nil, disable it."
  (interactive (list (not current-prefix-arg)))
  (setq json-par-show-ancestors-out-of-window enable)
  (dolist (hook '(pre-redisplay-functions window-scroll-functions))
    (if enable
        (add-hook hook #'json-par--update-ancestors-out-of-window-overlay nil t)
      (remove-hook hook #'json-par--update-ancestors-out-of-window-overlay t)))
  (if enable
      (force-window-update (current-buffer))
    (dolist (window (get-buffer-window-list nil nil t))
      (json-par--delete-ancestors-out-of-window-overlay window))))

(defun json-par-disable-ancestors-out-of-window-overlay ()
  "Disable showing ancestors out of windows."
  (interactive)
  (json-par-enable-ancestors-out-of-window-overlay nil))

(defun json-par-toggle-ancestors-out-of-window-overlay ()
  "Toggle showing ancestors out of windows."
  (interactive)
  (json-par-enable-ancestors-out-of-window-overlay
   (null json-par-show-ancestors-out-of-window)))

(defun json-par--update-ancestors-out-of-window-overlay
    (&optional window window-start)
  "Update the overlay showing ancestors out of the window.

Intended to be a `window-scroll-functions' and `pre-redisplay-functions'.

If `json-par-show-ancestors-out-of-window' is nil, this is no-op.

WINDOW is the window to be updated, default to the selected window.
WINDOW-START is the start position of the visible area of the WINDOW, default to
\(window-start WINDOW)."
  (unless window
    (setq window (selected-window)))
  (unless window-start
    (setq window-start (window-start window)))
  (when json-par-show-ancestors-out-of-window
    (let ((overlay (window-parameter
                    window
                    'json-par--ancestors-out-of-window-overlay))
          first-line-path
          point-path
          merged-path)
      (unless overlay
        (setq overlay (make-overlay window-start (1+ window-start)))
        (overlay-put overlay 'window window)
        (set-window-parameter window
                              'json-par--ancestors-out-of-window-overlay
                              overlay))
      (delete-overlay overlay)
      (with-current-buffer (window-buffer window)
        (setq first-line-path
              (json-par--get-ancestors-for-overlay window-start))
        (setq point-path
              (if json-par-highlight-ancestors
                  (json-par--get-ancestors-for-overlay (window-point window))
                ()))
        (while first-line-path
          (if (eq (car first-line-path) (car point-path))
              (progn
                (push (json-par--format-ancestor-out-of-window
                       (car first-line-path)
                       '(json-par-ancestor-face
                         json-par-ancestor-out-of-window-face)
                       json-par-char-before-ancestor)
                      merged-path)
                (setq first-line-path (cdr first-line-path))
                (setq point-path (cdr point-path)))
            (dolist (path first-line-path)
              (push
               (json-par--format-ancestor-out-of-window
                path
                'json-par-ancestor-out-of-window-face
                nil)
               merged-path))
            (setq first-line-path nil)
            (setq point-path nil)))
        (setq merged-path (reverse merged-path))
        (overlay-put overlay 'before-string
                     (mapconcat #'identity merged-path ""))
        (move-overlay overlay window-start (1+ window-start))))))

(defun json-par--get-ancestors-for-overlay (point)
  "Return a list of ancestor positions of POINT.

The first position is the position of the root value.

If the beginning of the current member is before POINT, include it at the last."
  (save-excursion
    (let ((path ()))
      (goto-char point)
      (json-par--out-comment)
      (json-par--out-atom)
      (json-par-beginning-of-member)
      (when (< (point) point)
        (push (point) path))
      (while (progn
               (json-par--backward-spaces)
               (not (bobp)))
        (json-par-up-backward)
        (json-par-beginning-of-member)
        (push (point) path))
      path)))

(defun json-par--format-ancestor-out-of-window (point face prefix-char)
  "Return a formatted ancestor, which is out of window at POINT.

FACE is applied to the string.

If PREFIX-CHAR is non-nil, it is shown before the ancestor"
  (save-excursion
    (goto-char point)
    (let* ((key-token (save-excursion (json-par-forward-token)))
           (link-text (cond
                       ((json-par--object-key-p key-token)
                        (concat (json-par-token-text key-token) ":"))
                       ((save-excursion
                          (json-par--backward-spaces)
                          (bobp))
                        (json-par-token-text-no-properties key-token))
                       (t
                        (concat "["
                                (number-to-string
                                 (json-par--current-member-index))
                                "]:")))))
      (propertize
       (concat (make-string (current-column) ?\s)
               (if prefix-char (string prefix-char) "")
               (propertize
                link-text
                'keymap
                (let ((map (make-sparse-keymap)))
                  (define-key map [down-mouse-2] (lambda () (interactive)))
                  (define-key map [down-mouse-1] (lambda () (interactive)))
                  (define-key map [mouse-2]
                              (lambda ()
                                (interactive)
                                (push-mark)
                                (goto-char (json-par-token-start key-token))))
                  (define-key map [follow-link] 'mouse-face)
                  map)
                'mouse-face
                'link)
               "\n")
       'face
       face))))

(defun json-par--delete-ancestors-out-of-window-overlay (window)
  "Delete the overlay showing ancestors out of the window.

WINDOW is the window to be modified."
  (when (window-parameter window 'json-par--ancestors-out-of-window-overlay)
    (delete-overlay (window-parameter
                     window
                     'json-par--ancestors-out-of-window-overlay))
    (set-window-parameter window
                          'json-par--ancestors-out-of-window-overlay
                          nil)))


;;; Highlight ancestors

(defun json-par-enable-ancestor-overlays (&optional enable)
  "Enable showing ancestors.

Ancestors of the point is highlighted with `json-par-ancestor-face'.

With prefix arg or if ENABLE is nil, disable it."
  (interactive (list (not current-prefix-arg)))
  (setq json-par-highlight-ancestors enable)
  (if enable
      (progn
        (add-hook 'pre-redisplay-functions
                  #'json-par--update-ancestor-overlays
                  nil
                  t)
        (force-window-update (current-buffer)))
    (remove-hook 'pre-redisplay-functions
                 #'json-par--update-ancestor-overlays
                 t)
    (dolist (window (get-buffer-window-list nil nil t))
      (json-par--delete-ancestor-overlays window))))

(defun json-par-disable-ancestor-overlays ()
  "Disable showing ancestors."
  (interactive)
  (json-par-enable-ancestor-overlays nil))

(defun json-par-toggle-ancestor-overlays ()
  "Toggle showing ancestors."
  (interactive)
  (json-par-enable-ancestor-overlays (not json-par-highlight-ancestors)))

(defun json-par--update-ancestor-overlays (&optional window window-start)
  "Add overlay to ancestors of the current member.

Intended to be a `window-scroll-functions' and `pre-redisplay-functions'.

If `json-par-highlight-ancestors' is non-nil, this is no-op.

WINDOW is the window to be updated, default to the selected window.
WINDOW-START is the start position of the visible area of the WINDOW, default to
\(window-start WINDOW)."
  (unless window
    (setq window (selected-window)))
  (unless window-start
    (setq window-start (window-start window)))
  (when json-par-highlight-ancestors
    (json-par--delete-ancestor-overlays window)
    (let (token
          overlay
          before-ancestor-overlay
          before-start)
      (with-current-buffer (window-buffer window)
        (save-excursion
          (goto-char (window-point window))
          (json-par--out-comment)
          (json-par--out-atom)
          (while (progn
                   (json-par--backward-spaces)
                   (not (bobp)))
            (json-par-up-backward)
            (json-par-beginning-of-member)
            (setq token (save-excursion (json-par-forward-token)))
            (if (< (json-par-token-end token) window-start)
                (goto-char (point-min))
              (setq overlay (make-overlay (json-par-token-start token)
                                          (json-par-token-end token)))
              (overlay-put overlay 'face 'json-par-ancestor-face)

              (overlay-put overlay 'window window)
              (push overlay (window-parameter
                             window
                             'json-par--ancestor-overlays))
              (when json-par-char-before-ancestor
                (setq before-start
                      (max (1- (json-par-token-start token)) (point-min)))
                (setq before-ancestor-overlay
                      (make-overlay before-start (1+ before-start)))
                (overlay-put before-ancestor-overlay 'window window)
                (overlay-put before-ancestor-overlay 'display
                             (string json-par-char-before-ancestor))
                (overlay-put before-ancestor-overlay 'face
                             '(json-par-ancestor-face font-lock-keyword-face))
                (push before-ancestor-overlay
                      (window-parameter
                       window
                       'json-par--ancestor-overlays))))))))))

(defun json-par--delete-ancestor-overlays (window)
  "Delete overlays on the ancestors of the point.

WINDOW is the window to be modified."
  (dolist (overlay (window-parameter window 'json-par--ancestor-overlays))
    (delete-overlay overlay))
  (set-window-parameter window 'json-par--ancestor-overlays ()))


;;; Highlight current member

(defun json-par-enable-current-member-overlay (&optional enable)
  "Enable highlighting current member.

Ancestors of the point is highlighted with `json-par-current-member-face'.

If `json-par-char-before-current-member' is non-nil, it is shown before the
current member.

With prefix arg or if ENABLE is nil, disable it."
  (interactive (list (not current-prefix-arg)))
  (setq json-par-highlight-current-member enable)
  (if enable
      (progn
        (add-hook 'pre-redisplay-functions
                  #'json-par--update-current-member-overlay
                  nil
                  t)
        (force-window-update (current-buffer)))
    (remove-hook 'pre-redisplay-functions
                 #'json-par--update-current-member-overlay
                 t)
    (dolist (window (get-buffer-window-list nil nil t))
      (json-par--delete-current-member-overlay window))))

(defun json-par-disable-current-member-overlay ()
  "Disable highlighting current member."
  (interactive)
  (json-par-enable-current-member-overlay nil))

(defun json-par-toggle-current-member-overlay ()
  "Toggle highlighting current member."
  (interactive)
  (json-par-enable-current-member-overlay
   (not json-par-highlight-current-member)))

(defun json-par--update-current-member-overlay (&optional window)
  "Update the overlay highlighting the current member.

Intended to be a `pre-redisplay-functions'.

WINDOW is the window to be updated, default to the selected window."
  (unless window
    (setq window (selected-window)))
  (when json-par-highlight-current-member
    (let* ((current-member-overlay
            (window-parameter window 'json-par--current-member-overlay))
           (before-current-member-overlay
            (window-parameter window 'json-par--before-current-member-overlay))
           before-start
           start
           end)
      (unless current-member-overlay
        (setq current-member-overlay (make-overlay (point) (point)))
        (overlay-put current-member-overlay 'window window)
        (set-window-parameter window
                              'json-par--current-member-overlay
                              current-member-overlay))
      (unless before-current-member-overlay
        (setq before-current-member-overlay (make-overlay (point) (point)))
        (overlay-put before-current-member-overlay 'window window)
        (set-window-parameter window
                              'json-par--before-current-member-overlay
                              before-current-member-overlay))
      (delete-overlay current-member-overlay)
      (delete-overlay before-current-member-overlay)
      (with-current-buffer (window-buffer window)
        (save-excursion
          (goto-char (window-point window))
          (setq start (save-excursion
                        (json-par-beginning-of-member)
                        (point)))
          (setq end (save-excursion
                      (goto-char start)
                      (json-par-end-of-member)
                      (point)))
          (setq before-start (max (1- start) (point-min)))
          (overlay-put current-member-overlay
                       'face
                       'json-par-current-member-face)
          (move-overlay current-member-overlay start end)
          (when json-par-char-before-current-member
            (overlay-put before-current-member-overlay
                         'display
                         (string json-par-char-before-current-member))
            (overlay-put before-current-member-overlay
                         'face
                         '(json-par-current-member-face
                           font-lock-keyword-face))
            (move-overlay before-current-member-overlay
                          before-start
                          (1+ before-start))))))))

(defun json-par--delete-current-member-overlay (window)
  "Delete overlay highlighting the current member.

WINDOW is the window to be modified."
  (dolist (key '(json-par--current-member-overlay
                 json-par--before-current-member-overlay))
    (when (window-parameter window key)
      (delete-overlay (window-parameter window key))
      (set-window-parameter window key nil))))


(provide 'json-par-ancestor-overlay)

;;; json-par-ancestor-overlay.el ends here
