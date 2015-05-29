;;;;;;;;;;;;;;;;;;;;
;; Initialisation ;;
;;;;;;;;;;;;;;;;;;;;

(setq my-packages
      '(use-package
        ace-jump-mode
        anzu
        auctex
        auto-complete
        autopair
        deft
        diminish
        dired-details
        ess
        expand-region
        helm
        js2-mode
        macro-math
        magit
        markdown-mode
        polymode
        smartparens
        smooth-scrolling
        zenburn-theme
        ))

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
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

;; Set and load custom file

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Always have server running

(server-start)

;;;;;;;;;;;;;
;; Path(s) ;;
;;;;;;;;;;;;;

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/usr/texbin"))
(setq exec-path (append exec-path '("/usr/texbin")))
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups"))))
(setq bookmark-default-file "~/Dropbox/Emacs/bookmarks")
(setq diary-file "~/Dropbox/Emacs/diary")

;;;;;;;;;;;;;
;; Browser ;;
;;;;;;;;;;;;;

(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "firefox")

;;;;;;;;;;;;;;;;
;; Appearance ;;
;;;;;;;;;;;;;;;;

(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

(add-to-list 'default-frame-alist '(height . 34))
(add-to-list 'default-frame-alist '(width . 84))

(setq mouse-avoidance-mode 'banish)
(setq mouse-yank-at-point t)
(mouse-wheel-mode t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)

(display-time-mode 1)
(setq display-time-24h-format 1)

(set-face-attribute 'default nil :height 121 :font "Terminus")
(setq-default line-spacing 3)

(load-theme 'zenburn)

(setq shift-select-mode nil)

(setq uniquify-buffer-name-style 'forward)

(setq frame-title-format '(buffer-file-name "%f" ("%b")))

(setq visible-bell t)

(blink-cursor-mode -1)

(set-default 'indicate-empty-lines t)

(defun switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))

(global-set-key [f11] 'switch-full-screen)

(setq global-visual-line-mode t)

;;;;;;;;;;;;;
;; Editing ;;
;;;;;;;;;;;;;

(defalias 'yes-or-no-p 'y-or-n-p)

;; autoindent newlines
(define-key global-map (kbd "RET") 'newline-and-indent)

;; autoindent pasted code
(setq autopair-global-mode t)

;; autopair things
(setq autopair-autowrap t)

;; autorevert everything
(global-auto-revert-mode)

;; Textmate-style (un)commenting
(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    )
  )
(global-set-key [(control c) (c)] 'comment-or-uncomment-line-or-region)

;; Ace-Jump
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; Expand region
(global-set-key (kbd "C-x =") 'er/expand-region)

(show-paren-mode 1)

(set-default 'indent-tabs-mode nil)

;; Hippie expand
(dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
  (delete f hippie-expand-try-functions-list))
;; Add this back in at the end of the list.
(add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t)
(global-set-key (kbd "M-/") 'hippie-expand)

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(use-package helm
  :config
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (require 'helm-config)
  (setq helm-quick-update                     t
        helm-split-window-in-side-p           t
        helm-buffers-fuzzy-matching           t)
  :bind
  ("C-x b" . helm-mini)
  ("C-c h o" . helm-occur)
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files))

(use-package anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode)
  :bind
  ("M-%" . anzu-query-replace)
  ("C-M-%" . anzu-query-replace-regexp))

(use-package ess-site
  :load-path "site-lisp/ess/lisp/"
  :commands R
  :mode ("\\.R\\'" . R-mode)
  :config
  (setq ess-language "R")
  (setq ess-ask-for-ess-directory nil)
  (setq ess-local-process-name "R")
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
  (ess-toggle-underscore nil)
  (setq ansi-color-for-comint-mode 'filter)
  ;; (setq comint-scroll-to-bottom-on-input t)
  ;; (setq comint-scroll-to-bottom-on-output t)
  ;; (setq comint-move-point-for-output t)
  (add-hook 'ess-mode-hook 'auto-complete-mode)
  (add-hook 'inferior-ess-mode-hook 'auto-complete-mode)
  (setq ess-use-auto-complete t)
  (add-hook 'ess-mode-hook 'smartparens-mode)
  (add-hook 'ess-mode-hook 'turn-on-auto-fill)
  (add-hook 'inferior-ess-mode-hook 'turn-on-auto-fill)
  (setq ess-default-style 'OWN)
  (setq ess-indent-level 2))

(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package polymode
  :config
  (add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode)))

(use-package reftex
  :config
  (setq reftex-cite-format (quote natbib))
  (setq reftex-cite-prompt-optional-args nil)
  (setq reftex-cite-cleanup-optional-args t)
  (setq reftex-enable-partial-scans t)
  (setq reftex-save-parse-info t)
  (setq reftex-use-multiple-selection-buffers t)
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-section-levels '(("part" . 0)
                    ("chapter" . 1)
                    ("section" . 2)
                    ("subsection" . 3)
                    ("subsubsection" . 4)
                    ("paragraph" . 5)
                    ("subparagraph" . 6)
                    ("frametitle" . 7)
                    ("addchap" . -1)
                    ("addsec" . -2)
                    ("begin{frame}" . -3))))

(use-package tex-site
  :ensure auctex
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

(use-package auto-complete
  :diminish auto-complete-mode)

(use-package auto-complete-config
  :config
  (global-auto-complete-mode t)
  (setq ac-modes (append ac-modes '(org-mode R-mode))) 
  (ac-config-default)
  (define-key ac-complete-mode-map [tab] 'ac-expand)
  (setq ac-auto-start 2)
  (ac-flyspell-workaround))

(use-package deft
  :bind
  ("<f9>" . deft)
  :config
  (use-package deft)
  (setq deft-extension "txt")
  (setq deft-directory "~/Dropbox/Zettelkasten")
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filename-as-title t))

(use-package org
  :bind
  ("C-c a" . org-agenda)
  ("C-c C-+" . org-capture)
  :mode
  ("\\.org$" . org-mode)
  ("\\.txt$" . org-mode)
  :config
  (setq org-hide-leading-stars t)
  (setq org-tags-column -70)
  (setq org-directory "~/Dropbox/Org")
  (setq org-archive-location "~/Dropbox/Org/gtd-archived.org::From %s")
  (require 'ox-latex)
  (require 'ox-beamer)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (sh . t)
     (R . t)
     (python . t)
     (js . t)
     (haskell . t)
     (ditaa . t)))
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)")
                            (sequence "|" "CNCL(c@!)")
                            (sequence "|" "DELG (e@/!)")
                            (sequence "FXME(f)" "|" "FIXD(x!)")
                            (sequence "PROJ(p!)" "PRSC(u)" "PROH(o)" "|" "PRDN(r!)" "PRCL(o@/!)")
                            (sequence "LIST(l)" "|")
                            (sequence "SDMB(s)" "|")))
  (setq org-todo-keyword-faces
        '(("PROJ" . "yellow")
          ("LIST" . "pink")
          ("WAIT" . "orange")
          ("CNCL" . "grey")
          ("PROH" . "grey")))
  (setq org-log-into-drawer t)
  (setq org-agenda-files (quote ("~/Dropbox/Org/gtd-inbox.org" "~/Dropbox/Org/gtd-active.org" "~/Dropbox/Org/gtd-habits.org" "~/Dropbox/Org/gtd-anniversaries.org")))
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-habit-preceding-days 0)
  (setq org-agenda-custom-commands
        ;; three day context
        '(("O" "Three day context"
           ((agenda "Due or scheduled within next three days"
                    ((org-agenda-ndays 3)
                     (org-agenda-start-on-weekday nil)
                     (org-agenda-overriding-header "Due or scheduled within next three days")
                     ))
            ))
          ;; Review Group
          ("r" . "GTD Review")
          ("rd" "Daily Review"
           ((tags-todo "+REVIEW=\"daily\""
                       ((org-agenda-overriding-header "Projects for Daily Review")))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Waiting for...")))))
          
          ("rw" "Weekly Review"
           ((tags-todo "+REVIEW=\"weekly\"|+REVIEW=\"daily\""
                       ((org-agenda-overriding-header "Projects for Weekly Review")))
            (stuck ""
                   ((org-agenda-overriding-header "Stuck Projects")))
            (todo "SDMB"
                  ((org-agenda-overriding-header "Open Loops")))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Waiting for...")))))
          
          ("rm" "Monthly Review"
           ((tags-todo "+REVIEW=\"monthly\"|+REVIEW=\"weekly\"|+REVIEW=\"daily\""
                       ((org-agenda-overriding-header "Projects for Monthly Review")))
            (stuck ""
                   ((org-agenda-overriding-header "Stuck Projects")))
            (todo "SDMB"
                  ((org-agenda-overriding-header "Open Loops")))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Waiting for...")))))
          
          ;; list of projects
          ("P" "Projects" tags-todo "LEVEL=1"
           ((org-agenda-overriding-header "List of Projects")))
          
          ;; List of upcoming deadlines
          ("d" "Upcoming Deadlines" agenda ""
           ((org-agenda-ndays 1)
            (org-agenda-start-day nil)
            (org-agenda-time-grid nil)
            (org-deadline-warning-days 365)
            (org-agenda-entry-types '(:deadline))
            ))
          ))
  (setq org-agenda-include-diary t)
  (setq org-default-notes-file (concat org-directory "/gtd-inbox.org"))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline (concat org-directory "/gtd-inbox.org") "Inbox")
           "* TODO %?\n  %i\n")
          ("k" "Todo (link)" entry (file+headline (concat org-directory "/gtd-inbox.org") "Inbox")
           "* TODO %?\n  %i\n  %a")
          ("i" "Limbo" entry (file+headline (concat org-directory "/gtd-limbo.org") "Ideas")
           "* %?\n  %i\n  %a")
          ("f" "Fixme" entry (file+headline (concat org-directory "/gtd-inbox.org") "Inbox")
           "* FXME %?\n  %i\n %a")
          ))
  (setq org-refile-targets (quote ((org-agenda-files :todo . "PROJ") (org-agenda-files :todo . "LIST") (org-agenda-files :todo . "PROH") (org-agenda-files :todo . "PRSC") (org-agenda-files :tag . "bucket"))))
  (setq org-refile-use-cache nil)
  (setq org-stuck-projects (quote ("+LEVEL=1/+PROJ-PRDN-PROH-SDMB" ("TODO" "NEXT" "FXME" "STRT") nil "")))
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq org-crypt-key nil))

(use-package markdown
  :mode
  ("\\.md$" . markdown-mode)
  ("\\.mkd$" . markdown-mode)
  :config
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook 'auto-fill-mode))

;;;;;;;;;;;;;
;; Windows ;;
;;;;;;;;;;;;;

(cond
 ((string-match "mingw" system-configuration)
  ;; to be _absolutely_ shure, we use unix encoding in all files
  (set-default buffer-file-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix)
  (set-default default-buffer-file-coding-system 'utf-8-unix)
  ;; some thing concerning the PATH to mark magit and spell-checking work correctly
  (setq exec-path (append exec-path '("C:/Program Files (x86)/Git/bin")))
  (setq magit-git-executable "C:/Program Files (x86)/Git/bin/git.exe")
  (setq-default ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell.exe")
  ;; make sure firefox is found
  (setq browse-url-firefox-program "C:/Program Files (x86)/Mozilla Firefox/firefox.exe")
  (setq browse-url-generic-program "C:/Program Files (x86)/Mozilla Firefox/firefox.exe")
  ;; make org-babel find R on Windows
  (setq org-babel-R-command "C:/Progra~1/R/R-3.0.1/bin/x64/R.exe --slave --no-save")
  ;; use bigger font
  (set-face-attribute 'default nil :height 131 :font "Terminus")
  ;; add Python installation and scripts to the PATH
  (setq exec-path (append exec-path '("C:/Python27")))
  (setq exec-path (append exec-path '("C:/Python27/Scripts")))
  ))

;;;;;;;;;;;;;;;;;;;;;;;
;; Special functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; occur sections in R code like Rstudio does
(defun cz-occur-R-sections ()
  (interactive)
  (occur "^#+.*-\\{3,\\}")
  (other-window 1)
  (next-line)
  )

;; insert new R section
(defun cz-insert-R-section ()
  (interactive)
  (setq the-section-name (read-string "Section? "))
  (move-beginning-of-line nil)
  (message the-section-name)
  (insert (concat "### " the-section-name " ---"))
  (newline-and-indent)
  )

;; autocomplete tags in Zettelkasten
(defun cz-complete-zetteltag ()
  (interactive)
  (shell-command "grep -horE '^(@|\+|\$).+' ~/Dropbox/Zettelkasten/ | sort | uniq > ~/.zetteltags")
  ;; TODO: how to complete from this list of tags?
  )
