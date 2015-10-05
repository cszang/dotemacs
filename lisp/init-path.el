(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/usr/texbin:/Library/TeX/texbin"))
(setq exec-path (append exec-path '("/usr/texbin")))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Library/TeX/texbin")))
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups"))))
(setq bookmark-default-file "~/ownCloud/Emacs/Lesezeichen")
(setq diary-file "~/ownCloud/Emacs/Kalender")

(provide 'init-path)
