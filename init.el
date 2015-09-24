(setq my-packages
      '(use-package
        ace-jump-mode
        anzu
        auctex
        auto-complete
        autopair
        atom-one-dark-theme
        diminish
        dired-details
        expand-region
        git-gutter
        hl-todo
        js2-mode
        macro-math
        magit
        markdown-mode
        org
        polymode
        smartparens
        smex
        smooth-scrolling
        ))

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize)
  ;; Fetch packages from melpa/elpa if not present:
  (defvar refreshed nil)
  (mapc
    (lambda (package)
      (unless (package-installed-p package)
        (unless refreshed
          (package-refresh-contents)
          (setq refreshed t))
        (package-install package))) my-packages))

(require 'use-package)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'server)
(unless (server-running-p)
    (server-start))

(require 'init-path)
(require 'init-custom)
(require 'init-appearance)
(require 'init-editing)
(require 'init-smex)
(require 'init-anzu)
(require 'init-ess)
(require 'init-git)
(require 'init-polymode)
(require 'init-tex)
(require 'init-org)
(require 'init-markdown)
(require 'init-r-navigation)
(require 'init-convenience)
(require 'init-dired)
(require 'init-platforms)
