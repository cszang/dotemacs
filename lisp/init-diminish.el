(diminish 'auto-fill-function " α")
(diminish 'visual-line-mode)
(diminish 'ivy-mode)
(defun cz-ess-diminish-modes ()
  (diminish 'smartparens-mode)
  (diminish 'eldoc-mode))
(add-hook 'ess-mode-hook 'cz-ess-diminish-modes)
(provide 'init-diminish)
