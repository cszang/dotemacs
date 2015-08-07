(use-package smex
  :bind
  ("M-x" . smex)
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items")))

(provide 'init-smex)
