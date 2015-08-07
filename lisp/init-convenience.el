;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

;; occur TODOs
(defun cz-occur-TODOs ()
  (interactive)
  (occur "TODO\\|FIXME\\|FXME\\|KAEFER\\|OFFEN")
  (other-window 1)
  (next-line)
  )

(provide 'init-convenience)
