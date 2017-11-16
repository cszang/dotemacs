(global-company-mode t)
(define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)

(eval-after-load
 'company
 '(add-to-list 'company-backends 'company-omnisharp))

(provide 'init-company)
