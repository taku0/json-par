;;; json-par-test-mode.el --- Mode lifecycle tests  -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'json-par)
(require 'json-par-test)

(defun json-par-run-test-mode-restores-functions (&optional
                                                  jsonc-mode
                                                  _indent-level-variable
                                                  error-buffer
                                                  error-counts
                                                  _progress-reporter)
  "Test that disabling JSON Par must restore function variables.

It should restore `forward-sexp-function', `indent-line-function', and
`indent-region-function'.

Enable and disable `json-par-mode' on JSONC-MODE.

Report failures to ERROR-BUFFER and update ERROR-COUNTS."
  (interactive)
  (if (not json-par-test-running)
      (json-par-run-test '(json-par-run-test-mode))
    ;; Disabling the mode must restore `forward-sexp-function',
    ;; `indent-line-function', and `indent-region-function', even after
    ;; enabling it more than once.
    (dolist (enable-count '(1 2))
      (with-temp-buffer
        (funcall jsonc-mode)
        (setq-local indent-region-function
                    'json-par-test-original-indent-region)
        (let ((original-forward-sexp-function forward-sexp-function)
              (original-indent-line-function indent-line-function)
              (original-indent-region-function indent-region-function))
          (dotimes (_ enable-count)
            (json-par-mode 1))
          (json-par-mode 0)
          (let ((status
                 (if (and (eq forward-sexp-function
                              original-forward-sexp-function)
                          (eq indent-line-function
                              original-indent-line-function)
                          (eq indent-region-function
                              original-indent-region-function))
                     'ok
                   'error)))
            (setcdr (assq status error-counts)
                    (1+ (cdr (assq status error-counts))))
            (when (eq status 'error)
              (json-par-show-error
               error-buffer "json-par-test-mode.el" 1 "error"
               (format "%S: %d enable calls, restored %S; expected %S"
                       jsonc-mode enable-count
                       (list forward-sexp-function indent-line-function
                             indent-region-function)
                       (list original-forward-sexp-function
                             original-indent-line-function
                             original-indent-region-function))))))))))

(defun json-par-run-test-mode-restore-local-variables (&optional
                                                       jsonc-mode
                                                       _indent-level-variable
                                                       error-buffer
                                                       error-counts
                                                       _progress-reporter)
  "Test that disabling JSON Par preserves buffer-local highlight settings.

Enable and disable `json-par-mode' on JSONC-MODE.

Report failures to ERROR-BUFFER and update ERROR-COUNTS."
  (dolist (disable-command '(json-par-quit json-par-disable-temporary))
    (with-temp-buffer
      (funcall jsonc-mode)
      (json-par-mode 1)
      (json-par-enable-ancestors-out-of-window-overlay t)
      (json-par-enable-ancestor-overlays t)
      (json-par-enable-current-member-overlay t)
      (funcall disable-command)
      (let ((settings '(json-par-show-ancestors-out-of-window
                        json-par-highlight-ancestors
                        json-par-highlight-current-member))
            (hooks '(json-par--update-ancestors-out-of-window-overlay
                     json-par--update-ancestor-overlays
                     json-par--update-current-member-overlay)))
        (let ((status
               (if (and (not json-par-mode)
                        (cl-every (lambda (setting)
                                    (and (local-variable-p setting)
                                         (symbol-value setting)))
                                  settings)
                        (not (memq (car hooks) window-scroll-functions))
                        (cl-every (lambda (hook)
                                    (not (memq hook pre-redisplay-functions)))
                                  hooks))
                   'ok
                 'error)))
          (setcdr (assq status error-counts)
                  (1+ (cdr (assq status error-counts))))
          (when (eq status 'error)
            (json-par-show-error
             error-buffer "json-par-test-mode.el" 2 "error"
             (format "%S: %S lost settings or left overlay hooks on disable"
                     jsonc-mode disable-command))))))))

(provide 'json-par-test-mode)

;;; json-par-test-mode.el ends here
