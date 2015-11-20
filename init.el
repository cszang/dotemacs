(setq my-packages
      '(use-package
        anzu
        auctex
        auto-complete
        autopair
        arjen-grey
        avy
        beacon
        diminish
        expand-region
        flycheck
        git-gutter
        goto-chg
        hl-todo
        js2-mode
        macro-math
        magit
        markdown-mode
        olivetti
        org
        org-bullets
        palimpsest
        polymode
        projectile
        smartparens
        smex
        smooth-scrolling
        swiper
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

(require 'init-swiper)
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
(require 'init-r-addons)
(require 'init-convenience)
(require 'init-dired)
(require 'init-platforms)
(require 'init-olivetti)
(require 'init-pretty)
(require 'init-spelling)
(require 'init-projectile)
(require 'init-diminish)
