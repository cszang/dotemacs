;;  _____                   
;; |   __|_____ ___ ___ ___ 
;; |   __|     | .'|  _|_ -|
;; |_____|_|_|_|__,|___|___|
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -– Neal Stephenson, In the Beginning was the Command Line (1998)
;;
;; This config takes inspiration from other configs, most notably:
;; - https://vxlabs.com/2016/04/11/step-by-step-guide-to-c-navigation-and-completion-with-emacs-and-the-clang-based-rtags/
;; - https://gist.github.com/nilsdeppe/7645c096d93b005458d97d6874a91ea9#file-emacs-el
;; - https://onze.io/emacs/c++/2017/03/16/emacs-cpp.html

;;;;;;;;;;
;; Meta ;;
;;;;;;;;;;

;; Start emacs server
(server-start)

;; More security
(setq network-security-level 'high)

;;;;;;;;;;;
;; Paths ;;
;;;;;;;;;;;

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/usr/texbin:/Library/TeX/texbin"))
(setq exec-path (append exec-path '("/usr/texbin")))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Library/TeX/texbin")))
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups"))))
(setq bookmark-default-file "~/ownCloud/Emacs/Lesezeichen")

;;;;;;;;;;;;;;;;;;;;;;;
;; General behaviour ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; We don't want to type yes and no all the time so, do y and n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Check (on save) whether the file edited contains a shebang, if yes,
;; make it executable from
;; http://mbork.pl/2015-01-10_A_few_random_Emacs_tips
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; No splash screen and initial scratch message
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

;; Always wrap words and act on visual lines
(setq global-visual-line-mode t)

;; No bell, please
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Use external custom file and load it without erros
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; No backup files
(setq make-backup-files nil)

;; Autorevert everything
(global-auto-revert-mode)

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("gnu" . "http://elpa.gnu.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	))
(package-initialize)
;; packages to install
(defvar package-list
  '(ag
    auctex
    autopair
    company
    company-ycmd
    counsel
    default-text-scale
    deft
    dired-quick-sort
    dumb-jump
    expand-region
    eyebrowse
    flycheck
    flycheck-ycmd
    git-gutter
    google-c-style
    hl-todo
    magit
    markdown-mode
    modern-cpp-font-lock
    org
    osx-trash
    polymode
    powerline
    projectile
    s
    smex
    spaceline
    swiper
    yaml-mode
    ycmd
    zenburn-theme)
  )
;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; macOS specific settings
(cond
 ((string-match "darwin" system-configuration)
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq magit-git-executable "/usr/bin/git")
  (setq mac-option-modifier nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load custom functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;;;;;;;;;;;;;;;;
;; Appearance ;;
;;;;;;;;;;;;;;;;

;; Disable the menu bar except for macOS GUI Emacs, where it does
;; waste additional screen real estate
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))

;; Disable all that GUI stuff
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)

;; Don't ring the bell
(setq ring-bell-function 'ignore)

;; Non-nil means draw block cursor as wide as the glyph under it.
;; For example, if a block cursor is over a tab, it will be drawn as
;; wide as that tab on the display.
(setq x-stretch-cursor t)

;; Do not display mouse cursor when typing
(setq mouse-avoidance-mode 'banish)

;; Mouse yank commands yank at point instead of a click
(setq mouse-yank-at-point t)

;; Enable mouse wheeling
(mouse-wheel-mode t)

;; Three mouse-related lines in my init? Hey, this is Emacs!

;; Highlight keywords and allow navigation
(global-hl-todo-mode)
(add-hook 'ess-mode-hook 'hl-todo-mode)

;; Set default font to Input or IBM Plex Mono
(set-face-attribute 'default nil :height 141 :font "IBM Plex Mono")

;; Make line spacing a bit wider than default
(setq-default line-spacing 5)

;; Use the Zenburn theme
(load-theme 'zenburn)

;; Powerline setup
(require 'powerline)
(setq powerline-image-apple-rgb t)
(setq ns-use-srgb-colorspace nil)
(require 'spaceline-config)
(display-time-mode 1)
(setq display-time-format "%H:%M")
(setq spaceline-window-numbers-unicode t)
(setq spaceline-workspace-numbers-unicode t)
(setq-default
   powerline-default-separator 'utf-8
   spaceline-flycheck-bullet "❖ %s"
   spaceline-separator-dir-left '(right . right)
   spaceline-separator-dir-right '(left . left))
(spaceline-install
    'main
    '((workspace-number :face highlight-face)
      (buffer-modified)
      ((remote-host buffer-id) :face region)
      (major-mode)
      (projectile-root)
      (process :when active))
    '((selection-info :face region :when mark-active)
      ((flycheck-error flycheck-warning flycheck-info) :when active)
      (which-function)
      (version-control :when active)
      (line-column)
      (global :when active)
      (buffer-position)))
(setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))
;; (end of Powerline setup)

;; Construct unique buffer names as name|bar/mumble
(setq uniquify-buffer-name-style 'post-forward)

(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; Do not blink the cursor
(blink-cursor-mode -1)

;; Always indicate empty lines in the fringe
(set-default 'indicate-empty-lines t)

;; Increase/Decrease faces globally; binds C-M-= to increasing and
;; C-M-- to decreasing
(default-text-scale-mode 1)

;; Highlight the line we are currently on
(global-hl-line-mode t)

;; Show parens
(show-paren-mode 1)

;; Change frame name to oblique strategy and change every 15 minutes
(load-file "~/.emacs.d/lisp/oblique.el")
(run-with-timer 0 900 'cz-display-oblique)

;;;;;;;;;;;;;;;;;;;;;
;; General editing ;;
;;;;;;;;;;;;;;;;;;;;;

;; autoindent newlines
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Automatically at closing brace, bracket and quote
(require 'autopair)
(autopair-global-mode)

;; Autopair things
(setq autopair-autowrap t)

;; Better comment/uncomment (Textmate-style)
(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    )
  )
;; @bind to C-c c
(global-set-key [(control c) (c)] 'comment-or-uncomment-line-or-region)

;; Do (normally) not indent with tabs
(set-default 'indent-tabs-mode nil)

;; Set the size that a tab CHARACTER is interpreted as
;; (unnecessary if there are no tab characters in the file!)
(setq tab-width 2)

;; Fill/unfill paragraph with one binding
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))
;; @bind remap M-q to fill-or-unfill
(global-set-key [remap fill-paragraph]
                #'endless/fill-or-unfill)

;; Use eyebrowse for managing workspaces
(eyebrowse-mode t)

;; Expand regions with @bind
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;;;;;;;;;;;;;;;
;; Completion ;;
;;;;;;;;;;;;;;;;

;; ycmd setup for C++
(require 'ycmd)
(setq ycmd-startup-timeout 5)
(add-hook 'c++-mode-hook #'ycmd-mode)
(set-variable 'ycmd-server-command '("python" "/Users/christian/lisp/ycmd/ycmd"))
(set-variable 'ycmd-global-config (expand-file-name "~/lisp/ycmd/examples/.ycm_extra_conf.py"))
(set-variable 'ycmd-extra-conf-whitelist '("~/repos/*" "~/LPJ-GUESS/4.0.1"))
(setq ycmd-force-semantic-completion t)

;; Use eldoc info for function arguments
(add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)

;; Use company for all completion
(require 'company)

;; Zero delay when pressing tab
(setq company-idle-delay 0)

;; @bind Show doc buffer with M-h
(define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)

;; Activate globally
(add-hook 'after-init-hook 'global-company-mode)

(require 'company-ycmd)
(company-ycmd-setup)


;;;;;;;;;;;;;;;;
;; Navigation ;;
;;;;;;;;;;;;;;;;

;; 1. Dump-Jump
(dumb-jump-mode)
(setq dumb-jump-selector 'ivy)
;; create .dumbjump file in project root to speed things up!

;; 2. Rtags (for C++)
;; Install rtags locally from M-x install-rtags
(setq rtags-install-path "~/.emacs.d/")
(setq rtags-path "~/.emacs.d/rtags-2.18/bin/")

(defun setup-rtags ()
  (define-key c-mode-base-map (kbd "M-.")
    (function rtags-find-symbol-at-point))
  (define-key c-mode-base-map (kbd "M-,")
    (function rtags-find-references-at-point))
  (rtags-start-process-unless-running)
  (rtags-enable-standard-keybindings)
  )
(add-hook 'c-mode-common-hook 'setup-rtags)
            
;;;;;;;;;;;;;;
;; Flycheck ;;
;;;;;;;;;;;;;;

(setq flycheck-global-modes '(c++-mode ess-mode))
(global-flycheck-mode 1)
(require 'flycheck-ycmd)
(add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup)

;;;;;;;;;;;;;;
;; Spelling ;;
;;;;;;;;;;;;;;

(setq ispell-program-name "aspell")

(setenv "LANG" "en_GB.UTF-8")
(setq ispell-dictionary "en")

;; general toggling between English and German
(defun cz-toggle-dictionary ()
  (interactive)
  (setq the-dict ispell-dictionary)
  (cond
   ((string-equal the-dict "en")
    (progn (ispell-change-dictionary "de")
           (message "changed dictionary from English to German")))
   ((string-equal the-dict "de")
    (progn (ispell-change-dictionary "en")
           (message "changed dictionary from German to English")))))

;; @bind
(global-set-key (kbd "C-c C-x l") 'cz-toggle-dictionary)

;;;;;;;;;;;
;; Dired ;;
;;;;;;;;;;;

(when (eq system-type 'darwin)
  (osx-trash-setup))
(setq delete-by-moving-to-trash t)
(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\.DS_Store$\\|^\\.Rhistory"))
(setq dired-listing-switches "-lah")
(require 'dired-quick-sort)
(dired-quick-sort-setup)
(setq insert-directory-program "/usr/local/bin/gls")

;;;;;;;;;
;; Ivy ;;
;;;;;;;;;

(require 'ivy)
(ivy-mode t)
;; uses bookmarks and recentf
(setq ivy-use-virtual-buffers t)
(setq ivy-height 5)
(setq ivy-display-style 'fancy)
(setq ivy-count-format "(%d/%d) ")
(setq enable-recursive-minibuffers t)
(setq ivy-wrap t)
;; @bind
(global-set-key (kbd "C-c C-r") 'ivy-resume)

(require 'swiper)
;; @bind
(global-set-key "\C-s" 'swiper)
(global-set-key "\C-r" 'swiper)

(require 'counsel)
;; @bind
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x f") 'counsel-recentf)
(global-set-key (kbd "C-c g") 'counsel-git-grep)
(global-set-key (kbd "C-c j") 'counsel-git)
(global-set-key (kbd "C-c k") 'counsel-ag)

;; Show #/total when scrolling buffers
(setq ivy-use-virtual-buffers t ivy-count-format "%d/%d ")

;;;;;;;;;;;;;;;;
;; Projectile ;;
;;;;;;;;;;;;;;;;

;; Mostly used for information purposes: project information in
;; mode-line, R-sessions etc.; also: projectile-find-file is good!
(projectile-global-mode)

;;;;;;;;;
;; Git ;;
;;;;;;;;;

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-completing-read-function 'ivy-completing-read)
(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-git-gutter-mode t)
(setq git-gutter:update-interval 5)

;;;;;;;;;;
;; Deft ;;
;;;;;;;;;;

(setq deft-directory "~/ownCloud/Zettelkasten")
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)
(setq deft-extensions '("org"))
(global-set-key (kbd "C-c C-d") 'deft)

;; Use custom functions for Zettelkasten setup
(load-file "~/.emacs.d/lisp/zettelkasten.el")

;;;;;;;;;
;; ESS ;;
;;;;;;;;;

;; Setup; using version from Git instead of MELPA
(add-to-list 'load-path "~/lisp/ess/lisp/")
(load "ess-site")

;; Use R, not S
(setq ess-language "R")
(setq-default ess-dialect "R")
(setq ess-local-process-name "R")

;; Do not expand underscore to <-, we live in the tidyverse!
(ess-toggle-underscore nil)

;; Be compatible with RStudio
(setq ess-default-style 'RStudio)

;; Be more colourful
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

;; Colourise the comint buffer
(setq ansi-color-for-comint-mode 'filter)

;; Use autofill in all R related buffers
(add-hook 'ess-mode-hook 'turn-on-auto-fill)
(add-hook 'inferior-ess-mode-hook 'turn-on-auto-fill)

;; Use company for autocompletion in all R related buffers
(setq ess-use-company t)

;; Use custom functions
(load-file "~/.emacs.d/lisp/ess.el")

;; @bind Insertion of R section to C-c C-a
(define-key ess-mode-map (kbd "C-c C-a") 'cz-insert-R-section)
;; @bind Overview of R sections to C-c =
(define-key ess-mode-map (kbd "C-c =") 'cz-occur-R-sections)
;; @bind Insert magrittr pipe to C-c m
(define-key ess-mode-map (kbd "C-c m") 'cz-insert-magrittr-pipe)

;; remove some linters from flycheck
(setq flycheck-lintr-linters
      "default_linters[-which(names(default_linters) %in% c(\"trailing_whitespace_linter\",\"commented_code_linter\"))]") 

;;;;;;;;;;;;;;
;; Polymode ;;
;;;;;;;;;;;;;;

;; Connect typical R-related literate programming sources to Polymode
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

;;;;;;;;;;;;;;
;; Calendar ;;
;;;;;;;;;;;;;;

(setq calendar-latitude 48.4029)
(setq calendar-longitude 11.7412)
(setq calendar-location-name "Freising, BY")
(setq diary-file "~/ownCloud/Emacs/Kalender")

;;;;;;;;;
;; Org ;;
;;;;;;;;;

;; Auto-fill orgmode buffers
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; @bind Important global orgmode bindings
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c C-+") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

;; org-entities displays \alpha etc. as Unicode characters.
(setq org-pretty-entities t)

;; Hide the /italics/ and *bold* markers
(setq org-hide-emphasis-markers t)

;; Hit return on a link to open it in a browser
(setq org-return-follows-link t)

;; Allow a) b) c) lists
(setq org-list-allow-alphabetical t)

;; Embed an image with [[file:foo.png]] and then C-c C-x C-v to view
(setq org-display-inline-images t)

;; Automatically refresh inline images that are generated from Babel
;; blocks
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; Display images when a file is loaded (I can always toggle them off
;; if I don't want them)
(add-hook 'org-mode-hook (lambda () (org-toggle-inline-images)))

;; Preview LaTeX equations in buffers by showing images (C-c C-x C-l)
;; Details:
;; http://orgmode.org/worg/org-tutorials/org-latex-preview.html
(setq org-latex-create-formula-image-program 'imagemagick)

;; Turn ' and " into ‘posh’ “quotes”
(setq org-export-with-smart-quotes t)

(require 'org-protocol)
(setq org-hide-leading-stars t)
(setq org-tags-column -70)

;; LaTeX setup
(require 'ox-latex)
(require 'ox-beamer)
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process
      '("latexmk -pdflatex='xelatex -shell-escape -interaction nonstopmode' -pdf -f %f"))
(setq org-highlight-latex-and-related '(latex))

;; to allow for global image etc. settings on per-file basis
(setq org-export-allow-bind-keywords t)

;; Org Babel setup
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (R . t)
   (python . t)
   (js . t)
   (haskell . t)
   (ditaa . t)))

;; GTD setup
(setq org-directory "~/ownCloud/Org")
(setq org-agenda-files (quote ("~/ownCloud/Org/GTD-Termine.org" "~/ownCloud/Org/GTD-Eingang.org" "~/ownCloud/Org/GTD-Aktiv.org" "~/ownCloud/Org/GTD-Planung.org" "~/ownCloud/Org/GTD-Irgendwann.org" "~/ownCloud/Org/GTD-Jahrestage.org" "~/ownCloud/Org/GTD-Habits.org" "~/ownCloud/Org/Muell.org")))
(setq org-archive-location "~/ownCloud/Org/GTD-Archiv.org::Von %s")
(setq org-todo-keywords '((sequence "OFFEN(o)" "GESTARTET(g)" "WARTEN(w@/!)" "|" "ERLEDIGT(e!)" "DELIGIERT(d@/!)")
                          (sequence "|" "STORNIERT(s@!)")
                          (sequence "HABIT" "|" "ERLEDIGT")
                          (sequence "JAHRESTAG" "|" "ERLEDIGT")
                          (sequence "PROJEKT(p!)" "PR_INAKTIV(i)" "|" "PR_ERLEDIGT(g!)" "PR_STORNIERT(r@/!)")
                          (sequence "LISTE(l)" "|")))

(setq org-todo-keyword-faces
      '(("LISTE"  (:foreground "#529dff" :inverse-video t))
        ("PROJEKT" (:foreground "#529dff" :inverse-video t))
        ("PR_INAKTIV" (:foreground "#999999" :inverse-video t))
        ("WARTEN" . "#de8485")
        ("GESTARTET" . "#DFAF8F")
        ("STORNIERT" . "#999999")))

(setq org-fontify-done-headline t)
(setq org-log-into-drawer t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-habit-preceding-days 0)
(setq org-agenda-window-setup 'current-window)

(setq org-agenda-custom-commands
      '(("o" agenda "Is' was, Doc?"
         ((org-agenda-span 3)
          (org-agenda-start-on-weekday nil)
          (org-show-context-detail 'minimal)
          (org-agenda-overriding-header "Is' was, Doc?")))
        ("d" "Is' heute was, Doc?"
         ((agenda ""
                  ((org-agenda-span 1)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'regexp ":abends:"))
                   (org-show-context-detail 'minimal)
                   (org-agenda-overriding-header "Is' was tagsüber, Doc?")))
          (tags-todo "abends"
                     ((org-agenda-overriding-header "Is' was abends, Doc?")))
        ))
      ))

(setq org-agenda-include-diary t)
(setq org-agenda-show-future-repeats nil)
(setq org-default-notes-file (concat org-directory "/Eingang.org"))
(setq org-capture-templates
      '(("a" "Aufgabe" entry (file+headline (concat org-directory "/GTD-Eingang.org") "Eingang")
         "** OFFEN %?\n  %i\n")
        ("l" "Labnotebook" entry
         (file+datetree (concat org-directory "/Labnotebook.org"))
         "** %^{Überschrift}")
        ("x" "Lesezeichen" entry (file (concat org-directory "/Lesezeichen.org"))
         "* %c\n%U\n%i\n" :empty-lines 1)
        ("t" "Termin" entry
         (file+datetree+prompt (concat org-directory "/GTD-Termine.org"))
         "* %^{Bezeichnung}\n<%(org-read-date nil nil org-read-date-final-answer)>\n%i\n%?\n")
        ("k" "Aufgabe (mit Link)" entry (file+headline (concat org-directory "/GTD-Eingang.org") "Eingang")
         "** OFFEN %?\n  %i\n  %a")
        ("i" "Projektidee" plain (file (concat org-directory "/Projektideen.org"))
         "%?\n")
        ("z" "Zitat" entry (file (concat org-directory "/Zitate.org"))
         "* %?")
        ("m" "Machen" entry (file (concat org-directory "/Machen.org"))
         "* %?")
        ("g" "Geschenkidee" plain (file (concat org-directory "/Geschenkideen.org"))
         "* %?")
        ("p" "Phrase" plain (file (concat org-directory "/Phrasen.org"))
         "\n\n%?\n")
        ("b" "Buch" entry (file (concat org-directory "/Buecher.org"))
         "* LESEN %?\n  %U\n")
        ("o" "Motto" entry (file (concat org-directory "/Mottos.org"))
         "* %?")
        ("n" "Notiz" entry (file (concat org-directory "/Notizen.org"))
         "\n* %?\n  %U\n")
        ("e" "Essensidee" entry (file (concat org-directory "/Essensideen.org"))
         "* %?\n  %U\n")
        ("v" "Vokabel" entry (file (concat org-directory "/Vokabeln.org"))
         "* %^{Das Wort}\n** %^{Übersetzung}")
        ("w" "Workflow" entry (file (concat org-directory "/Workflow.org"))
         "* %^{Was?}\n  %?")
        ))
(setq org-attach-directory "~/ownCloud/Org/Attachments")
(setq org-refile-use-outline-path 'file)
(setq org-refile-targets 
      '(
        (
         ,(append org-agenda-files (list tech))
         :regexp . "refile")
        (
         ,(append org-agenda-files (list tech))
         :maxlevel . 1)
        ))
(setq org-refile-targets (quote ((org-agenda-files :todo . "PROJEKT") (org-agenda-files :todo . "LISTE") (org-agenda-files :todo . "PR_FESTGEFAHREN") (org-agenda-files :tag . "eimer"))))
(setq org-refile-use-cache nil)

;; Store captured stuff at the beginning of the file
(setq org-reverse-note-order t)

;; Crypt setup
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key nil)

;; Grab links from Mail.app etc.
(add-to-list 'org-modules 'org-mac-link)
;; @bind org-mac-grab-link to C-c g
(add-hook 'org-mode-hook (lambda () 
                           (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

;;;;;;;;;;;
;; LaTeX ;;
;;;;;;;;;;;

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq-default TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (push
             '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
               :help "Run latexmk on file")
             TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

(custom-set-variables
 '(TeX-source-correlate-method 'synctex)
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t))

(setq reftex-cite-format (quote natbib))
(setq reftex-default-bibliography "~/Desktop/ResBib/refs.bib")
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
                              ("begin{frame}" . -3)))

(setq LaTeX-verbatim-environments-local
      '("Rcode" "bashcode" "juliacode" "cppcode" "lstlisting"))

;; use ESS like R eval in TeX buffers (useful for presentations with
;; code blocks)
(defun r-eval-latex-hook ()
 (local-set-key [(control return)] 'ess-eval-region-or-line-and-step))

(add-hook 'LaTeX-mode-hook 'r-eval-latex-hook)

;;;;;;;;;
;; C++ ;;
;;;;;;;;;

;; Modern C++ code highlighting
(require 'modern-cpp-font-lock)
(modern-c++-font-lock-global-mode t)

;; Set the number of spaces that the tab key inserts (usually 2 or 4)
(setq c-basic-offset 2)

;; Enable Google style things
(add-hook 'c-mode-common-hook 'google-set-c-style)

;; Autoindent using google style guide
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Enable hide/show of code blocks
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;;;;;;;;;;;;;;;;
;; JavaScript ;;
;;;;;;;;;;;;;;;;

;; TODO
