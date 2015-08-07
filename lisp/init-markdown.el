(use-package markdown
  :mode
  ("\\.md$" . markdown-mode)
  ("\\.mkd$" . markdown-mode)
  :config
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook 'auto-fill-mode))

(provide 'init-markdown)
