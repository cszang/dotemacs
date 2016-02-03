(use-package ess-site
  :load-path "~/lisp/ess/lisp/"
  :commands R
  :mode ("\\.R\\'" . R-mode)
  :config
  (setq ess-language "R")
  (setq-default ess-dialect "R")
  (setq ess-ask-for-ess-directory nil)
  (setq ess-local-process-name "R")
  ;; (load "~/lisp/ess/lisp/ess-site")
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
  (setq inferior-julia-program-name "/usr/local/bin/julia")
  (ess-toggle-underscore nil)
  (setq ansi-color-for-comint-mode 'filter)
  (add-hook 'ess-mode-hook 'auto-complete-mode)
  (add-hook 'inferior-ess-mode-hook 'auto-complete-mode)
  (setq ess-use-auto-complete t)
  (add-hook 'ess-mode-hook 'smartparens-mode)
  (add-hook 'ess-mode-hook 'turn-on-auto-fill)
  (add-hook 'inferior-ess-mode-hook 'turn-on-auto-fill)
  (setq ess-default-style 'RRR)
  (define-key ess-mode-map (kbd "C-c C-a") 'cz-insert-R-section)
  (define-key ess-mode-map (kbd "C-c =") 'cz-occur-R-sections)
  (define-key ess-mode-map (kbd "C-c m") 'cz-insert-magrittr-pipe)
  )

(provide 'init-ess)
