(use-package anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode)
  :bind
  ("M-%" . anzu-query-replace)
  ("C-M-%" . anzu-query-replace-regexp))

(provide 'init-anzu)
