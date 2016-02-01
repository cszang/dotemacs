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
  (global-prettify-symbols-mode +1))

(provide 'init-pretty)
