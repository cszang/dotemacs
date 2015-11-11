(projectile-global-mode)

(setq projectile-mode-line (quote
                            (:eval (format " ☭|%s|"
                                           (projectile-project-name)))))

(provide 'init-projectile)
