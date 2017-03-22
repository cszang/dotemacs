(setq my-packages
      '(annotate
        anzu
        auctex
        auto-complete
        autopair
        avy
        beacon
        bm
        dash
        deft
        diminish
        dired-quick-sort
        elfeed
        expand-region
        flycheck
        ggtags
        git-gutter
        helm
        helm-projectile
        hl-todo
        js2-mode
        julia-mode
        macro-math
        magit
        markdown-mode
        neotree
        nyan-mode
        nyan-prompt
        olivetti
        org
        org-board
        org-tree-slide
        org-wc
        polymode
        projectile
        realgud
        s
        smartparens
        smex
        smooth-scrolling
        sr-speedbar
        swiper
        tldr
        undo-tree
        wc-mode
        wgrep
        yaml-mode
        xah-lookup
        zenburn-theme
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

;(require 'use-package)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'server)
(unless (server-running-p)
  (server-start))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(require 'init-swiper)
(require 'init-deft)
(require 'init-path)
(require 'init-calendar)
(require 'init-custom)
(require 'init-appearance)
(require 'init-oblique)
(require 'init-editing)
(require 'init-smex)
(require 'init-helm)
(require 'init-anzu)
(require 'init-ess)
(require 'init-git)
(require 'init-polymode)
(require 'init-tex)
(require 'init-org)
(require 'init-markdown)
(require 'init-convenience)
(require 'init-dired)
(require 'init-platforms)
(require 'init-olivetti)
(require 'init-pretty)
(require 'init-spelling)
(require 'init-projectile)
(require 'init-diminish)
(require 'init-flycheck)
(require 'init-diff)
(require 'init-elfeed)
(require 'init-undo)

(load-file "~/.emacs.d/lisp/gud.el")
(load-file "~/lisp/realgud-lldb/realgud-lldb.el")
