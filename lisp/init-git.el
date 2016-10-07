(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")

(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-git-gutter-mode +1)

(provide 'init-git)
