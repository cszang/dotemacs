(prettify-symbols-mode 1)

(when (boundp 'global-prettify-symbols-mode)
  (add-hook 'ess-mode-hook
            (lambda ()
              (push '("%>%" . ?⟾) prettify-symbols-alist)
              ))
  (add-hook 'inferior-ess-mode-hook
            (lambda ()
              (push '("%>%" . ?⟾) prettify-symbols-alist)
              ))
  (add-hook 'org-mode-hook
            (lambda ()
              (push '("PROJEKT" . ?❖) prettify-symbols-alist)
              (push '("LISTE" . ?☰) prettify-symbols-alist)
              (push '("PR_FESTGEFAHREN" . ?❄) prettify-symbols-alist)
              (push '("OFFEN" . ?⚪) prettify-symbols-alist)
              (push '("HABIT" . ?♺) prettify-symbols-alist)
              (push '("JAHRESTAG" . ?★) prettify-symbols-alist)
              (push '("LOVE" . ?❤) prettify-symbols-alist)
              (push '("ERLEDIGT" . ?✓) prettify-symbols-alist)
              (push '("STORNIERT" . ?✗) prettify-symbols-alist)
              (push '("WARTEN" . ?➾) prettify-symbols-alist)
              (push '("AREA" . ?⧉) prettify-symbols-alist)
              ))
  (global-prettify-symbols-mode +1))

(provide 'init-pretty)
