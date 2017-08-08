(require 'flycheck)

(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode markdown-mode gfm-mode))

(add-to-list 'flycheck-checkers 'proselint)

(defun setup-flycheck-clang-project-path ()
  (let ((root (ignore-errors (projectile-project-root))))
    (when root
      (add-to-list 
       (make-variable-buffer-local 'flycheck-clang-include-path)
       root))))

(add-hook 'c++-mode-hook 'setup-flycheck-clang-project-path)

(setq flycheck-global-modes '(c++-mode ess-mode))
(global-flycheck-mode 1)

(provide 'init-flycheck)
