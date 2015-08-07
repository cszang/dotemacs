(use-package magit
  :bind
  ("C-x g" . magit-status)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  )

(use-package git-gutter
  :bind
  ("C-x C-g" . git-gutter:toggle)
  :config
  (global-git-gutter-mode +1)
  )

(provide 'init-git)
