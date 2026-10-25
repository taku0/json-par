;;; json-par-test-guess.el --- Guess state tests  -*- lexical-binding: t -*-

;;; Code:

(require 'json-par)
(require 'json-par-test)

(defun json-par-run-test-guess-releases-markers (&optional
                                                 jsonc-mode
                                                 _indent-level-variable
                                                 error-buffer
                                                 error-counts
                                                 _progress-reporter)
  "Test marker ownership when replacing or clearing saved guess state.

Run with JSONC-MODE, report failures to ERROR-BUFFER and update ERROR-COUNTS."
  ;; `continue': Resume the saved state and replace the first candidate with
  ;; the next one, releasing the old markers and retaining the new ones.
  ;;
  ;; `restart': Start a fresh search without passing the saved state, releasing
  ;; its markers when the new state is saved.
  ;;
  ;; `exhaust': Resume after the only candidate has been inserted, releasing
  ;; the old markers and clearing the saved state when no candidates remain.
  (dolist (operation '(continue restart exhaust))
    (with-temp-buffer
      (funcall jsonc-mode)
      (json-par-mode 1)
      (insert (if (eq operation 'exhaust) "[[1], []]" "[[1, 2], []]"))
      (search-backward "[]")
      (forward-char)
      (let* ((json-par-friend-buffer-function (lambda (_) nil))
             (state (json-par-insert-guessed 2 nil t))
             (start (json-par--guess-state-inserted-start state))
             (end (json-par--guess-state-inserted-end state))
             (status 'ok))
        (json-par-insert-guessed 2 (unless (eq operation 'restart) state) t)
        (unless (and (markerp start)
                     (markerp end)
                     (null (marker-buffer start))
                     (null (marker-buffer end))
                     (if (eq operation 'exhaust)
                         (null json-par--last-state)
                       (and (eq (marker-buffer
                                 (json-par--guess-state-inserted-start
                                  json-par--last-state))
                                (current-buffer))
                            (eq (marker-buffer
                                 (json-par--guess-state-inserted-end
                                  json-par--last-state))
                                (current-buffer)))))
          (setq status 'error)
          (json-par-show-error
           error-buffer "json-par-test-guess.el" 1 "error"
           (format "%S: %S must release old markers and retain new markers"
                   jsonc-mode operation)))
        (setcdr (assq status error-counts)
                (1+ (cdr (assq status error-counts))))))))

(provide 'json-par-test-guess)

;;; json-par-test-guess.el ends here
