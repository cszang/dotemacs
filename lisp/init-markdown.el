(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$" . markdown-mode))
(add-hook 'markdown-mode-hook 'auto-fill-mode)

(provide 'init-markdown)
