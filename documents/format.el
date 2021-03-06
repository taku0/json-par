(require 'text-property-search)

(defun json-par-to-html (start end)
  (interactive "r")
  (let ((original-buffer (current-buffer))
        (spans '())
        (splitted-spans '())
        (classes '((font-lock-keyword-face . "keyword")
                   (font-lock-string-face . "string")
                   (font-lock-constant-face . "constant")))
        class
        (point nil)
        (mark nil)
        match
        previous-span
        formatted)
    (with-temp-buffer
      (insert-buffer-substring-no-properties original-buffer start end)
      (json-mode)
      (font-lock-fontify-buffer)
      (goto-char (point-min))
      (while (search-forward "&" nil t)
        (replace-match "&amp;"))
      (goto-char (point-min))
      (while (search-forward "<" nil t)
        (replace-match "&lt;"))
      (goto-char (point-min))
      (while (search-forward ">" nil t)
        (replace-match "&gt;"))
      (goto-char (point-min))
      (while (search-forward-regexp "[*|↓]" nil t)
        (cond
         ((equal (match-string 0) "|")
          (setq point (match-beginning 0))
          (push (list (match-beginning 0) (match-end 0) '("point")) spans)
          (when (or (eolp) (eobp))
            (insert-char ?\s))
          (push (list (1+ point) (+ 2 point) '("after-point")) spans))
         ((equal (match-string 0) "*")
          (setq mark (match-beginning 0))
          (push (list (match-beginning 0) (match-end 0) '("mark")) spans))
         (t
          (setq point nil)
          (setq mark nil)))
        (when (and point mark)
          (push (list (1+ (min point mark)) (max point mark) '("region")) spans)
          (setq point nil)
          (setq mark nil)))
      (goto-char (point-min))
      (while (setq match (text-property-search-forward 'face))
        (setq class (alist-get (prop-match-value match) classes))
        (when class
          (push (list (prop-match-beginning match)
                      (prop-match-end match)
                      (list class))
                spans)))
      ;; (goto-char (point-max))
      ;; (insert (prin1-to-string spans))
      (setq splitted-spans (json-par--flatten-spans spans))
      ;; (goto-char (point-max))
      ;; (insert (prin1-to-string splitted-spans))
      (dolist (span splitted-spans)
        (goto-char (nth 1 span))
        (insert "</span>")
        (when (member "point" (nth 2 span))
          (insert "</span>"))
        (goto-char (nth 0 span))
        (when (member "point" (nth 2 span))
          (insert "<span class=\"point-container\">"))
        (insert "<span class=\"")
        (dolist (class (nth 2 span))
          (insert class)
          (insert-char ?\s))
        (delete-backward-char 1)
        (insert "\">"))
      (goto-char (point-max))
      (insert "</code></pre>")
      (goto-char (point-min))
      (insert "<pre class=\"emacs\"><code>")
      (setq formatted (buffer-string)))
    (delete-region start end)
    (insert formatted)))

(defun json-par--flatten-spans (spans)
  (let ((splitted-spans '()))
    (setq spans (sort spans
                      (lambda (s1 s2)
                        (or (< (nth 0 s1) (nth 0 s2))
                            (and (= (nth 0 s1) (nth 0 s2))
                                 (< (nth 1 s1) (nth 1 s2)))))))
    (when spans
      (setq previous-span (car spans))
      (dolist (current-span (cdr spans))
        (if (<= (nth 1 previous-span)
                (nth 0 current-span))
            (progn
              (push previous-span splitted-spans)
              (setq previous-span current-span))
          (when (< (nth 0 previous-span)
                   (nth 0 current-span))
            (push (list (nth 0 previous-span)
                        (nth 0 current-span)
                        (nth 2 previous-span))
                  splitted-spans)
            (setq previous-span
                  (list
                   (nth 0 current-span)
                   (nth 1 previous-span)
                   (nth 2 previous-span))))
          (cond
           ((= (nth 1 previous-span)
               (nth 1 current-span))
            (setq previous-span
                  (list
                   (nth 0 previous-span)
                   (nth 1 previous-span)
                   (append (nth 2 previous-span)
                           (nth 2 current-span)))))
           ((< (nth 1 previous-span)
               (nth 1 current-span))
            (push
             (list
              (nth 0 current-span)
              (nth 1 previous-span)
              (append (nth 2 previous-span)
                      (nth 2 current-span)))
             splitted-spans)
            (setq previous-span
                  (list
                   (nth 1 previous-span)
                   (nth 1 current-span)
                   (nth 2 current-span))))
           ((< (nth 1 current-span)
               (nth 1 previous-span))
            (push
             (list
              (nth 0 previous-span)
              (nth 1 current-span)
              (append (nth 2 previous-span)
                      (nth 2 current-span)))
             splitted-spans)
            (setq previous-span
                  (list
                   (nth 1 current-span)
                   (nth 1 previous-span)
                   (nth 2 previous-span)))))))
      (push previous-span splitted-spans))
    splitted-spans))
