(setq delete-by-moving-to-trash t)
(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\.DS_Store$\\|^\\.Rhistory"))
(setq dired-listing-switches "-lah")
(provide 'init-dired)
