;;;;;;;;;;;;;;;;;;;;
;; Initialisation ;;
;;;;;;;;;;;;;;;;;;;;

(setq my-packages
      '(use-package
        ace-jump-mode
        anzu
        atom-one-dark-theme
        auctex
        auto-complete
        autopair
        deft
        diminish
        dired-details
        ess
        expand-region
        f
        js2-mode
        macro-math
        magit
        markdown-mode
        polymode
        powerline
        scratch
        smartparens
        smex
        smooth-scrolling
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

;; OS X
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

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
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups"))))
(setq bookmark-default-file "~/ownCloud/Emacs/Lesezeichen")
(setq diary-file "~/ownCloud/Emacs/Kalender")

;;;;;;;;;;;;;
;; Browser ;;
;;;;;;;;;;;;;

(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "iceweasel")

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

(if (string= "buck" system-name)
    (progn (set-face-attribute 'default nil :height 131 :font "Inconsolata")
           (setq-default line-spacing 3))
  (progn (set-face-attribute 'default nil :height 151 :font "Inconsolata")
         (setq-default line-spacing 5)))

(load-theme 'atom-one-dark)

(require 'powerline)
(powerline-default-theme)

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

(display-time-mode 1)
(setq display-time-24h-format 1)

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

;; ido
(require 'ido)
(ido-mode t)
(global-set-key (kbd "C-x f") 'ido-find-file)

;; Hippie expand
(dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
  (delete f hippie-expand-try-functions-list))
;; Add this back in at the end of the list.
(add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t)
(global-set-key (kbd "M-/") 'hippie-expand)

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(use-package smex
  :bind
  ("M-x" . smex)
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items")))

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
  (define-key ess-mode-map (kbd "C-c C-a") 'cz-insert-R-section)
  (define-key ess-mode-map (kbd "C-c =") 'cz-occur-R-sections)
  (setq ess-default-style 'RStudio))

(use-package magit
  :bind
  ("C-x g" . magit-status)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  )

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

(use-package org
  :bind
  ("C-c a" . org-agenda)
  ("C-c C-+" . org-capture)
  :mode
  ("\\.org$" . org-mode)
  ("\\.txt$" . org-mode)
  :config
  (require 'org-protocol)
  (setq org-hide-leading-stars t)
  (setq org-tags-column -70)
  (setq org-directory "~/ownCloud/Org")
  (setq org-archive-location "~/ownCloud/Org/GTD-Archiv.org::Von %s")
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
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
  (setq org-todo-keywords '((sequence "OFFEN(o)" "WARTEN(w@/!)" "|" "ERLEDIGT(e!)")
                            (sequence "|" "STORNIERT(s@!)")
                            (sequence "|" "DELIGIERT (d@/!)")
                            (sequence "KAEFER(k)" "|" "ERSCHLAGEN(a!)")
                            (sequence "PROJEKT(p!)" "PR_FESTGEFAHREN(f)" "|" "PR_ERLEDIGT(i!)" "PR_STORNIERT(r@/!)")
                            (sequence "LISTE(l)" "|")))
  (setq org-todo-keyword-faces
        '(("PROJEKT" . "yellow")
          ("LISTE" . "pink")
          ("WARTEN" . "orange")
          ("STORNIERT" . "grey")
          ("PR_FESTGEFAHREN" . "grey")))
  (setq org-log-into-drawer t)
  (setq org-agenda-files (quote ("~/ownCloud/Org/GTD-Eingang.org" "~/ownCloud/Org/GTD-Aktiv.org" "~/ownCloud/Org/GTD-Gewohnheiten.org" "~/ownCloud/Org/GTD-Jahrestage.org")))
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-habit-preceding-days 0)
  (setq org-agenda-custom-commands
        ;; three day context
        '(("O" "3-Tages Ansicht"
           ((agenda "Fällig oder geplant im Laufe der nächsten drei Tage"
                    ((org-agenda-ndays 3)
                     (org-agenda-start-on-weekday nil)
                     (org-agenda-overriding-header "Fällig oder geplant im Laufe der nächsten drei Tage")
                     ))
            ))
          ;; Review Group
          ("r" . "GTD Überprüfung")
          ("rd" "Tägliche Überprüfung"
           ((tags-todo "+REVIEW=\"daily\""
                       ((org-agenda-overriding-header "Projekte zur täglichen Überprüfung")))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Warten auf...")))))
          
          ("rw" "Wöchentliche Überprüfung"
           ((tags-todo "+REVIEW=\"weekly\"|+REVIEW=\"daily\""
                       ((org-agenda-overriding-header "Projekte zur wöchentlichen Überprüfung")))
            (stuck ""
                   ((org-agenda-overriding-header "Festgefahrene Projekte")))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Waiting for...")))))
          
          ("rm" "Monatliche Überprüfung"
           ((tags-todo "+REVIEW=\"monthly\"|+REVIEW=\"weekly\"|+REVIEW=\"daily\""
                       ((org-agenda-overriding-header "Projekte zur monatlichen Überprüfung")))
            (stuck ""
                   ((org-agenda-overriding-header "Festgefahrene Projekte")))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Warten auf...")))))
          
          ;; list of projects
          ("P" "Projekte" tags-todo "LEVEL=1"
           ((org-agenda-overriding-header "Liste aller Projekte")))
          
          ;; List of upcoming deadlines
          ("d" "Nächste Fristen" agenda ""
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
        '(("a" "Aufgabe" entry (file+headline (concat org-directory "/GTD-Eingang.org") "Eingang")
           "* OFFEN %?\n  %i\n")
          ("l" "Aufgabe (mit Link)" entry (file+headline (concat org-directory "/GTD-Eingang.org") "Eingang")
           "* OFFEN %?\n  %i\n  %a")
          ("k" "Käfer" entry (file+headline (concat org-directory "/GTD-Eingang.org") "Eingang")
           "* KAEFER %?\n  %i\n %a")
          ("b" "Buch" plain (file "~/ownCloud/Notizen/Buecher.txt"))
          ))
  (setq org-refile-targets (quote ((org-agenda-files :todo . "PROJEKT") (org-agenda-files :todo . "LISTE") (org-agenda-files :todo . "PR_FESTGEFAHREN") (org-agenda-files :tag . "eimer"))))
  (setq org-refile-use-cache nil)
  (setq org-stuck-projects (quote ("+LEVEL=1/+PROJEKT-PR_ERLEDIGT-PR_FESTGEFAHREN" ("OFFEN" "KAEFER") nil "")))
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


;;;;;;;;;;;;;;;;;;
;; Zettelkasten ;;
;;;;;;;;;;;;;;;;;;

(setq zettelkasten-directory "~/ownCloud/Zettelkasten")

;; Zettelkasten is built on top of Deft mode for quick filtering, the
;; rest is custom made
(use-package deft
  :bind
  ("<f9>" . deft)
  ("C-c C-z" . zk-new-zettel)
  ("C-c n" . zk-new-zettel)
  :config
  (use-package deft)
  (setq deft-extension "zkn")
  (setq deft-text-mode 'zettelkasten-mode)
  (setq deft-directory zettelkasten-directory)
  (setq deft-use-filename-as-title t))

(defun flatten (mylist)
  (cond
   ((null mylist) nil)
   ((atom mylist) (list mylist))
   (t
    (append (flatten (car mylist)) (flatten (cdr mylist))))))

(defun zk-match-tag-in-buffer (f)
  "append all matches of tags in a buffer to a list"
  (save-excursion
    (setq tag-regex "^\\(@\\|\\+\\|\\$\\).+$")
    (find-file f)
    (setq zk-tags-in-zettel (s-match-strings-all tag-regex (buffer-string)))
    (setq zk-tag-list (append zk-tag-list zk-tags-in-zettel))
    (kill-buffer (current-buffer))))

(defun zk-get-tag-list()
  "gets all tags from all zettelfiles"
  (interactive)
  (require 'dash)
  (setq zk-tag-list '(()))
  (mapc 'zk-match-tag-in-buffer
        (directory-files zettelkasten-directory t ".zkn$"))
  (setq zk-tag-list (-distinct (flatten zk-tag-list))))

(defun zk-complete-tag ()
  "completes zettelkasten tags from all previously used tags"
  (interactive)
  (insert (ido-completing-read "Schlagwort? " zk-tag-list))
  (newline-and-indent))

(defun zk-complete-structure ()
  "completes structural elements for Zettelkasten system"
  (interactive)
  (setq zk-structures (list "*Schlagwörter*" "*Literatur*" "*Verknüpfungen*"))
  (setq zk-structure (ido-completing-read "Struktur? " zk-structures))
  (insert zk-structure)
  (newline-and-indent)
  (cond ((string= "*Schlagwörter*" zk-structure) (zk-complete-tag))
        ((string= "*Literatur*" zk-structure) (yank))))

(defun zk-new-zettel ()
  "create a new zettelkasten-zettel and visit the file"
  (interactive)
  (require 'cl)
  (require 'f)
  (setq zk-new-number
        (+ 1
           (reduce #'max
                   (mapcar 'string-to-number
                           (mapcar 'file-name-sans-extension
                                   (directory-files zettelkasten-directory nil "^\[0-9\]\\{4\\}")
                                   )))))
  (setq new-number-preceeding-zeros
        (cond ((>= zk-new-number 1000 ) ""   )
              ((>= zk-new-number 100  ) "0"  )
              ((>= zk-new-number 10   ) "00" )
              ((>= zk-new-number 0    ) "000")))
  (setq new-zettel-file-name
        (concat (f-slash zettelkasten-directory) new-number-preceeding-zeros (number-to-string zk-new-number) ".zkn"))
  (find-file new-zettel-file-name)
  (zk-get-tag-list))

(setq zk-keywords
      '(("^\\*.+\\*" . font-lock-builtin-face)
        ("^@.+" . font-lock-constant-face)
        ("^\\+.+" . font-lock-keyword-face)
        ("^\\$.+" . font-lock-function-name-face)))

(define-minor-mode zettelkasten-mode
  "Zettelkasten management"
  :lighter " Zkn"
  :keymap (let ((map (make-sparse-keymap)))
            (progn
              (define-key map (kbd "@") 'zk-complete-tag)
              (define-key map (kbd "*") 'zk-complete-structure)
              (define-key map (kbd "C-c C-n") 'zk-new-zettel)
              )
            map)
  (auto-fill-mode)
  (turn-on-orgstruct)
  (font-lock-add-keywords nil zk-keywords)
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer))))
  )

(add-to-list 'auto-mode-alist '("\\.zkn" . zettelkasten-mode))
(global-set-key (kbd "C-c z") 'zk-new-zettel)

;;;;;;;;;;;;;;;;;;
;; R Navigation ;;
;;;;;;;;;;;;;;;;;;

;; visit topic and kill Occur buffer
(defun cz-goto-R-section ()
  (interactive)
  (occur-mode-goto-occurrence)
  (other-window 1)
  (quit-window))

;; occur sections in R code like Rstudio does
(defun cz-occur-R-sections ()
  (interactive)
  (occur "^#+.*-\\{3,\\}")
  (other-window 1)
  (next-line)
  (local-set-key (kbd "RET") 'cz-goto-R-section)
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

;;;;;;;;;;;;;;;;;
;; Convenience ;;
;;;;;;;;;;;;;;;;;

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

;; occur TODOs
(defun cz-occur-TODOs ()
  (interactive)
  (occur "TODO\\|FIXME\\|FXME\\|KAEFER\\|OFFEN")
  (other-window 1)
  (next-line)
  )

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
  (setq org-babel-R-command "C:/Progra~1/R/R-3.2.1/bin/x64/R.exe --slave --no-save")
  ;; add Python installation and scripts to the PATH
  (setq exec-path (append exec-path '("C:/Python27")))
  (setq exec-path (append exec-path '("C:/Python27/Scripts")))
  ))
