(use-package ess-site
  :load-path "~/lisp/ess/lisp/"
  :commands R
  :mode ("\\.R\\'" . R-mode)
  :config
  (setq ess-language "R")
  (setq ess-ask-for-ess-directory nil)
  (setq ess-local-process-name "R")
  (setq ess-R-font-lock-keywords
      (quote
       ((ess-R-fl-keyword:modifiers . t)
        (ess-R-fl-keyword:fun-defs . t)
        (ess-R-fl-keyword:keywords . t)
        (ess-R-fl-keyword:assign-ops . t)
        (ess-R-fl-keyword:constants . t)
        (ess-fl-keyword:fun-calls . t)
        (ess-fl-keyword:numbers . t)
        (ess-fl-keyword:operators . t)
        (ess-fl-keyword:delimiters . t)
        (ess-fl-keyword:=)
        (ess-R-fl-keyword:F&T))))
  (ess-toggle-underscore nil)
  (setq ansi-color-for-comint-mode 'filter)
  (add-hook 'ess-mode-hook 'auto-complete-mode)
  (add-hook 'inferior-ess-mode-hook 'auto-complete-mode)
  (setq ess-use-auto-complete t)
  (add-hook 'ess-mode-hook 'smartparens-mode)
  (add-hook 'ess-mode-hook 'turn-on-auto-fill)
  (add-hook 'inferior-ess-mode-hook 'turn-on-auto-fill)
  (defun myindent-ess-hook ()
    (setq ess-indent-level 2))
  (add-hook 'ess-mode-hook 'myindent-ess-hook)
  (define-key ess-mode-map (kbd "C-c C-a") 'cz-insert-R-section)
  (define-key ess-mode-map (kbd "C-c =") 'cz-occur-R-sections))

(provide 'init-ess)
