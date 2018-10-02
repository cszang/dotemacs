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
;; (server-start)

;; More security
(setq network-security-level 'high)

;;;;;;;;;;;;
;; Locale ;;
;;;;;;;;;;;;

(setq system-time-locale "C") 

;;;;;;;;;;;
;; Paths ;;
;;;;;;;;;;;

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/usr/texbin:/Library/TeX/texbin"))
(setq exec-path (append exec-path '("/usr/texbin")))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Library/TeX/texbin")))
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups"))))
(setq bookmark-default-file "~/Dropbox/Emacs/Lesezeichen")

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
  '(academic-phrases
    ag
    auctex
    autopair
    beacon
    company
    company-lsp
    company-quickhelp
    conda
    counsel
    cquery
    default-text-scale
    deft
    dired-quick-sort
    dumb-jump
    elfeed
    elfeed-goodies
    elpy
    emojify
    expand-region
    eyebrowse
    flycheck
    flycheck-ycmd
    git-gutter
    git-timemachine
    go-mode
    google-c-style
    hl-todo
    ivy
    ivy-xref
    lsp-mode
    lsp-ui
    magit
    magit-org-todos
    markdown-mode
    modern-cpp-font-lock
    moody
    org
    osx-trash
    polymode
    poly-R
    pos-tip
    powerline
    projectile
    rainbow-mode
    s
    smex
    spaceline
    swiper
    yaml-mode
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

;; Let titlebar appear in background colour of theme
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

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

(setq cz-theme 'solarized-light)

(defun cz-apply-theme (theme)
  (if (equal theme 'zenburn)
    (progn
      (load-theme 'zenburn' t)
      (zenburn-with-color-variables
       (set-face-attribute 'mode-line-inactive nil :overline   zenburn-bg-05)
       (set-face-attribute 'mode-line          nil :overline   zenburn-bg-1)
       (set-face-attribute 'mode-line          nil :underline  zenburn-bg-1)
       (set-face-attribute 'mode-line-inactive nil :underline  zenburn-bg-05)
       (set-face-attribute 'mode-line          nil :box        nil)
       (set-face-attribute 'mode-line-inactive nil :box        nil)
       (set-face-attribute 'mode-line-inactive nil :background zenburn-bg+05)
       (set-face-attribute 'mode-line          nil :background zenburn-bg-1)
       (set-face-attribute 'mode-line          nil :foreground zenburn-blue)
       (set-face-attribute 'mode-line-inactive nil :foreground zenburn-blue-2)))
  (progn
    (load-theme 'solarized-light t)
    (let ((line (face-attribute 'mode-line :underline)))
      (set-face-attribute 'mode-line-inactive nil :overline   line)
      (set-face-attribute 'mode-line          nil :overline   line)
      (set-face-attribute 'mode-line          nil :underline  line)
      (set-face-attribute 'mode-line          nil :box        nil)
      (set-face-attribute 'mode-line-inactive nil :box        nil)
      (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))))

(cz-apply-theme cz-theme)

(defun cz-toggle-theme ()
  (interactive)
  (if (equal cz-theme 'zenburn)
      (progn
        (setq cz-theme 'solarized-light)
        (setq solarized-scale-org-headlines nil)
        (setq solarized-use-variable-pitch nil)
        (add-to-list 'default-frame-alist '(ns-appearance . nil))
        (disable-theme 'zenburn))
    (progn
      (setq cz-theme 'zenburn)
      (disable-theme 'solarized-light)
      (add-to-list 'default-frame-alist '(ns-appearance . dark))))
  (cz-apply-theme cz-theme))

;; use Jonas Bernoulli' nice and clean mode-line variant
(require 'moody)
(setq x-underline-at-descent-line t)
(moody-replace-mode-line-buffer-identification)
(moody-replace-vc-mode)
(setq ns-use-srgb-colorspace nil)

(require 'minions)
(minions-mode)

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

;; Use beacon to show where the cursor is
(beacon-mode 1)

;; Show colour of colour strings
(add-hook 'prog-mode-hook 'rainbow-mode)
(add-hook 'ess-mode-hook 'rainbow-mode)

;;;;;;;;;;;;;;;;;;;;;;;
;; Window management ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Use eyebrowse for managing workspaces
(eyebrowse-mode t)

;; Use builtin windmove to use Shift-Arrow keys to traverse windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Winner mode lets me get back to a previous window config with C-c left
(when (fboundp 'winner-mode)
  (winner-mode 1))

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

;; Expand regions with @bind
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;;;;;;;;;;;;;;;
;; Completion ;;
;;;;;;;;;;;;;;;;

;; company-lsp for C++
(require 'company-lsp)
(push 'company-lsp company-backends)
;; disabling client-side cache and sorting because the server does a
;; better job
(setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)

;; Type #i" (or #include ") for quote-style includes and #i< (or
;; #include <) for system headers.

;; See
;; https://github.com/cquery-project/cquery/pull/391#issuecomment-362872732
;; for an alternative view (contextual parent as detail, signature as
;; label)

;; (setq cquery-extra-init-params '(:completion (:detailedLabel t)))

;; Use company for all completion
(require 'company)

;; use pop-ups with docs
(company-quickhelp-mode)

;; Zero delay when pressing tab
(setq company-idle-delay 0)

;; @bind Show doc buffer with M-h
(define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)

;; Activate globally
(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;;;;;;
;; Navigation ;;
;;;;;;;;;;;;;;;;

;; 1. Dump-Jump
(dumb-jump-mode)
(setq dumb-jump-selector 'ivy)
;; create .dumbjump file in project root to speed things up!

;; 2. cquery (for C++)
(require 'cquery)
(setq cquery-executable "~/lisp/cquery/build/release/bin/cquery")
;; we need compile_commands.json per project root

;;;;;;;;;
;; LSP ;;
;;;;;;;;;

(add-hook 'c-mode-common-hook 'lsp-cquery-enable)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(setq cquery-sem-highlight-method 'font-lock)
;; alternatively, (setq cquery-sem-highlight-method 'overlay)

;; For rainbow semantic highlighting
(cquery-use-default-rainbow-sem-highlight)

;;;;;;;;;;;;;;
;; Flycheck ;;
;;;;;;;;;;;;;;

(setq flycheck-global-modes '(c++-mode ess-mode))
(global-flycheck-mode 1)
(add-hook 'c++-mode-hook 'flycheck-mode)

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

(require 'ivy-xref)
(setq xref-show-xrefs-function #'ivy-xref-show-xrefs)

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
(global-set-key (kbd "C-c r") 'counsel-rg)

;; always use ripgrep
(setq counsel-git-cmd "rg --files")
(setq counsel-rg-base-command
      "rg -i -M 120 --no-heading --line-number --color never %s .")


;; Show #/total when scrolling buffers
(setq ivy-use-virtual-buffers t ivy-count-format "%d/%d ")

;;;;;;;;;;;;;;;;
;; Projectile ;;
;;;;;;;;;;;;;;;;

;; Mostly used for information purposes: project information in
;; mode-line, R-sessions etc.; also: projectile-find-file is good!
(projectile-global-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;;;;;;;;
;; Git ;;
;;;;;;;;;

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-completing-read-function 'ivy-completing-read)
(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-git-gutter-mode t)
(setq git-gutter:update-interval 5)
(require 'magit-org-todos)
(magit-org-todos-autoinsert)

;;;;;;;;;;
;; Deft ;;
;;;;;;;;;;

(setq deft-directory "~/Dropbox/Zettelkasten")
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)
(setq deft-extensions '("txt"))
(global-set-key (kbd "C-c C-d") 'deft)
(add-to-list 'auto-mode-alist '("\\.txt" . markdown-mode))

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

;; use homebrew julia
(setq inferior-julia-program-name "/usr/local/bin/julia")

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
(setq diary-file "~/Dropbox/Emacs/Kalender")

;;;;;;;;;
;; Org ;;
;;;;;;;;;

;; Auto-fill orgmode buffers
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Save all org buffers automatically to prevent locks when switching
;; hosts
(run-with-idle-timer 30 t 'org-save-all-org-buffers)

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
      '("latexmk -pdflatex='xelatex -shell-escape -bibtex -interaction nonstopmode' -pdf -f %f"))
;; (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
(setq org-highlight-latex-and-related '(latex))

(add-to-list 'org-latex-classes
          '(("koma-book"
             "\\documentclass{scrbook}
             [NO-DEFAULT-PACKAGES]
             [EXTRA]"
             ("\\chapter{%s}" . "\\chapter*{%s}")
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(add-to-list 'org-latex-classes
             '("koma-article"
               "\\documentclass{scrartcl}"
               ("\\section{%s}" . "\\section*{%s}")))

;; to allow for global image etc. settings on per-file basis
(setq org-export-allow-bind-keywords t)

;; Org Babel setup
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   ;; (sh . t)
   (R . t)
   (python . t)
   (js . t)
   (haskell . t)
   (ditaa . t)))

;; set homebrew ditaa path
(setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0/libexec/ditaa-0.11.0-standalone.jar")

;; GTD setup
(setq org-directory "~/Dropbox/Org/")
(setq org-inbox-file (concat org-directory "Org-Inbox.org"))
(setq org-agenda-files ())
(setq org-agenda-file-regexp "^Org-.*\\.org\\'")
(add-to-list 'org-agenda-files org-directory)
(setq org-archive-location (concat org-directory "Archiv.org::Von %s"))
(setq org-todo-keywords '((sequence "OFFEN(o)" "GESTARTET(g)" "WARTEN(w@/!)" "|" "ERLEDIGT(e!)" "DELIGIERT(d@/!)")
                          (sequence "|" "STORNIERT(s@!)")
                          (sequence "HABIT" "|" "ERLEDIGT")
                          (sequence "JAHRESTAG" "|" "ERLEDIGT")
                          (sequence "PROJEKT(p!)" "PR_INAKTIV(i)" "|" "PR_ERLEDIGT(x!)" "PR_STORNIERT(r@/!)")
                          (sequence "LISTE(l)" "|")
                          (sequence "KONZEPT" "DRAFT" "INREVIEW" "MAJOR" "MINOR" "|" "INPRESS")
                          ))

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
(setq org-agenda-window-setup 'current-window)

(setq org-agenda-custom-commands
      '(("o" agenda "Is' was, Doc?"
         ((org-agenda-span 3)
          (org-agenda-start-on-weekday nil)
          (org-show-context-detail 'minimal)
          (org-agenda-overriding-header "Is' was, Doc?")))
      ))

(setq org-agenda-include-diary t)
(setq org-agenda-show-future-repeats nil)
(setq org-default-notes-file org-inbox-file)
(setq org-capture-templates
      `(("a" "Aufgabe" entry (file+headline org-inbox-file "Eingang")
         "** OFFEN %?\n  %i\n")
        ("l" "Labnotebook" entry
         (file+datetree ,(concat org-directory "Labnotebook.org"))
         "** %^{Überschrift}")
        ("x" "Lesezeichen" entry (file ,(concat org-directory "Lesezeichen.org"))
         "* %c\n%U\n%i\n")
        ("j" "Journal" entry
         (file+datetree ,(concat org-directory "Journal.org"))
         "** %^{Überschrift}")
        ("t" "Termin" entry
         (file+datetree+prompt ,(concat org-directory "Org-Termine.org"))
         "* %^{Bezeichnung}\n<%(org-read-date nil nil org-read-date-final-answer)>\n%i\n%?\n")
        ("k" "Aufgabe (mit Link)" entry (file+headline org-inbox-file "Eingang")
         "** OFFEN %?\n  %i\n  %a")
        ("i" "Projektidee" plain (file ,(concat org-directory "Projektideen.org"))
         "%?\n")
        ("z" "Zitat" entry (file ,(concat org-directory "Zitate.org"))
         "* %?")
        ("m" "Machen" entry (file ,(concat org-directory "Machen.org"))
         "* %?")
        ("g" "Geschenkidee" plain (file ,(concat org-directory "Geschenkideen.org"))
         "* %?")
        ("p" "Phrase" plain (file ,(concat org-directory "Phrasen.org"))
         "\n\n%?\n")
        ("b" "Buch" entry (file ,(concat org-directory "Buecher.org"))
         "** LESEN %?\n  %U\n")
        ("o" "Motto" entry (file ,(concat org-directory "Mottos.org"))
         "* %?")
        ("n" "Notiz" entry (file ,(concat org-directory "Notizen.org"))
         "\n* %?\n  %U\n")
        ("e" "Essensidee" entry (file ,(concat org-directory "/Essensideen.org"))
         "* %?\n  %U\n")
        ("v" "Vokabel" entry (file ,(concat org-directory "Vokabeln.org"))
         "* %^{Das Wort}\n** %^{Übersetzung}")
        ("w" "Workflow" entry (file ,(concat org-directory "Workflow.org"))
         "* %^{Was?}\n  %?")
        ))
(setq org-attach-directory "~/Dropbox/Org/Attachments")
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-cache t)
(setq org-refile-targets '((org-agenda-files :maxlevel . 4)))

;; Store captured stuff at the beginning of the file
(setq org-reverse-note-order t)

;; Crypt setup
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key nil)

;; Grab links from Mail.app etc.
(add-to-list 'org-modules 'org-mac-link)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-preceding-days 7)
(setq org-habit-graph-column 70)
;; @bind org-mac-grab-link to C-c C-g
(add-hook 'org-mode-hook (lambda () 
                           (define-key org-mode-map (kbd "C-c C-g") 'org-mac-grab-link)))

(setq org-agenda-category-icon-alist
      '(("GTD" "~/.emacs.d/icons/einhorn.png" nil nil :ascent center)
        ("Gesundheit" "~/.emacs.d/icons/gesundheit.png" nil nil :ascent center)
        ("Anniv" "~/.emacs.d/icons/anniv.png" nil nil :ascent center)
        ("Schule" "~/.emacs.d/icons/schule.png" nil nil :ascent center)
        ("Maintain" "~/.emacs.d/icons/haus.png" nil nil :ascent center)
        ("Fussi" "~/.emacs.d/icons/soccer.png" nil nil :ascent center)
        ("Müll" "~/.emacs.d/icons/muell.png" nil nil :ascent center)
        ("Habits" "~/.emacs.d/icons/guru.png" nil nil :ascent center)
        ("Diary" "~/.emacs.d/icons/kalender.png" nil nil :ascent center)
        ("Dev" "~/.emacs.d/icons/code.png" nil nil :ascent center)
        ("Papers" "~/.emacs.d/icons/papers.png" nil nil :ascent center)
        ("Proposals" "~/.emacs.d/icons/proposals.png" nil nil :ascent center)
        ("Manage" "~/.emacs.d/icons/manager.png" nil nil :ascent center)
        ("Termine" "~/.emacs.d/icons/kalender.png" nil nil :ascent center)
        (".*" '(space . (:width (16))))))

;; Use appt with org to remind me when things come up
(require 'appt)
(setq appt-time-msg-list nil)
(setq appt-display-interval '10)
(setq
  appt-message-warning-time '10 
  appt-display-mode-line t
  appt-display-format 'echo)
(appt-activate 1)     
(org-agenda-to-appt)
(run-at-time "24:01" 3600 'org-agenda-to-appt)
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; Have a dedicated modeline face for appt's (from Emacs Wiki)
(defface appt-face
  '((t (:foreground "red")))
  "Face to indicate a current appointment."
  :group 'appt)

(defadvice appt-disp-window (before appt-hilite-more activate)
  (when appt-mode-string
    (put-text-property 1 (length appt-mode-string)
		       'face 'appt-face appt-mode-string)))

(defadvice appt-check (after appt-hilite activate)
  (when appt-mode-string
    (put-text-property 1 (length appt-mode-string)
		       'face 'appt-face appt-mode-string)
    (force-mode-line-update)))

;;;;;;;;;;;;
;; Emojis ;;
;;;;;;;;;;;;

(setq emojify-user-emojis '((":neckbeard:" . (("name" . "Neckbeard")
                                              ("image" . "~/.emacs.d/icons/neckbeard.png")
                                              ("style" . "github")))
                            (":cz-proposals:" . (("name" . "Proposals")
                                                 ("image" . "~/.emacs.d/icons/proposals-large.png")
                                                 ("style" . "github")))
                            (":cz-haus:" . (("name" . "Haus")
                                            ("image" . "~/.emacs.d/icons/haus-large.png")
                                            ("style" . "github")))
                            (":cz-muell:" . (("name" . "Müll")
                                             ("image" . "~/.emacs.d/icons/muell-large.png")
                                             ("style" . "github")))
                            (":cz-papers:" . (("name" . "Papers")
                                              ("image" . "~/.emacs.d/icons/papers-large.png")
                                              ("style" . "github")))
                            (":cz-manage:" . (("name" . "Manage")
                                              ("image" . "~/.emacs.d/icons/manager-large.png")
                                              ("style" . "github")))
                            (":cz-habil:" . (("name" . "Habilitation")
                                             ("image" . "~/.emacs.d/icons/habil-large.png")
                                             ("style" . "github")))
                            (":cz-talks:" . (("name" . "Talks")
                                             ("image" . "~/.emacs.d/icons/talks-large.png")
                                             ("style" . "github")))
                            (":cz-eimer:" . (("name" . "Eimer")
                                             ("image" . "~/.emacs.d/icons/eimer-large.png")
                                             ("style" . "github")))
                            (":cz-code:" . (("name" . "Code")
                                            ("image" . "~/.emacs.d/icons/code-large.png")
                                            ("style" . "github")))
                            (":cz-lehre:" . (("name" . "Lehre")
                                             ("image" . "~/.emacs.d/icons/lehre-large.png")
                                             ("style" . "github")))
                            (":cz-ninja:" . (("name" . "Ninja")
                                             ("image" . "~/.emacs.d/icons/ninja-large.png")
                                             ("style" . "github")))
                            (":cz-review:" . (("name" . "Review")
                                              ("image" . "~/.emacs.d/icons/review-large.png")
                                              ("style" . "github")))
                            (":cz-computer:" . (("name" . "Computer")
                                                ("image" . "~/.emacs.d/icons/computer-large.png")
                                                ("style" . "github")))
                            (":cz-frage:" . (("name" . "Frage")
                                             ("image" . "~/.emacs.d/icons/frage-large.png")
                                             ("style" . "github")))
                            (":cz-cz:" . (("name" . "Christian")
                                          ("image" . "~/.emacs.d/icons/cz.png")
                                          ("style" . "github")))
                            (":cz-cosz:" . (("name" . "Cosmo")
                                            ("image" . "~/.emacs.d/icons/cosz.png")
                                            ("style" . "github")))
                            (":cz-corz:" . (("name" . "Corin")
                                            ("image" . "~/.emacs.d/icons/corz.png")
                                            ("style" . "github")))
                            ))

;; If emojify is already loaded refresh emoji data
(when (featurep 'emojify)
  (emojify-set-emoji-data))

;; Use Emojis in Org, GitHub-style :smile:
(add-hook 'org-mode-hook 'emojify-mode)

;;;;;;;;;;;
;; LaTeX ;;
;;;;;;;;;;;

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq-default TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (if (boundp 'pushed-latexmk)
                nil
              (progn
                (setq pushed-latexmk t)
                (push
                 '("latexmk" "latexmk -shell-escape -bibtex -f -pdf %s" TeX-run-TeX nil t
                   :help "Run latexmk on file")
                 TeX-command-list)))))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

(custom-set-variables
 '(TeX-source-correlate-method 'synctex)
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t))

(setq reftex-cite-format (quote natbib))
(setq reftex-default-bibliography "~/Dropbox/Bibliothek/Bibliothek.bib")
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

;; Show which function we're in
(add-hook 'c-mode-common-hook 'which-function-mode)

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

(setq conda-anaconda-home (expand-file-name "~/miniconda3"))
(setq python-shell-interpreter (expand-file-name "~/miniconda3/bin/python"))
(conda-env-initialize-interactive-shells)
(conda-env-initialize-eshell)
(setq elpy-rpc-python-command (expand-file-name "~/miniconda3/bin/python"))
(setq python-check-command "/Users/christian/miniconda3/bin/flake8")
(elpy-enable)
(add-to-list 'exec-path "/Users/christian/miniconda3/bin")

;;;;;;;;;;;;
;; Octave ;;
;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(defun cz-octave-send-line-or-region (beginning end)
  (interactive "r")
  (if (use-region-p)
      (octave-send-region beginning end)
    (octave-send-line)
))

(add-hook 'octave-mode-hook 
          '(lambda nil
             (define-key octave-mode-map [(control return)]
               'cz-octave-send-line-or-region)
             (define-key octave-mode-map [(control space)]
               'octave-complete-symbol)))

;;;;;;;;;;;;;;;;
;; JavaScript ;;
;;;;;;;;;;;;;;;;

;; TODO

;;;;;;;;
;; Go ;;
;;;;;;;;

;; TODO

;;;;;;;;;;;;;;
;; Markdown ;;
;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-hook 'markdown-mode-hook 'auto-fill-mode)

;;;;;;;;;;;;;;
;; Browsing ;;
;;;;;;;;;;;;;;

;; When entering eww, use cursors to scroll without changing point
(add-hook 'eww-mode-hook 'scroll-lock-mode)

;;;;;;;;;
;; RSS ;;
;;;;;;;;;

(load "~/.emacs.d/elfeed-feeds.el")
(elfeed-goodies/setup)

;;;;;;;;;;;
;; TRAMP ;;
;;;;;;;;;;;

;; allow X11 forwarding from remote host
(add-to-list 'tramp-remote-process-environment
             (format "DISPLAY=%s" (getenv "DISPLAY")))

