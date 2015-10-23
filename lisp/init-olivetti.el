(defun cz-toggle-olivetti-mode ()
  (interactive)
  (if (bound-and-true-p olivetti-mode)
      (progn
        (olivetti-mode 0)
        (palimpsest-mode 0)
        ;; (olivetti-toggle-hide-modeline 0)
        )
      (progn 
        (olivetti-mode 1)
        (palimpsest-mode 1)
        ;; (olivetti-toggle-hide-modeline 1)
        (diminish 'palimpsest-mode " ↓")
        (diminish 'olivetti-mode " ⚓"))))

(global-set-key (kbd "M-ö") 'cz-toggle-olivetti-mode)
(global-set-key (kbd "M-Ö") 'olivetti-toggle-hide-modeline)

(provide 'init-olivetti)

