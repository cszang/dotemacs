(setq calendar-latitude 48.4029)
(setq calendar-longitude 11.7412)
(setq calendar-location-name "Freising, BY")

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c C-+") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Change the ellipsis that indicates hidden content
;; http://endlessparentheses.com/changing-the-org-mode-ellipsis.html
;; (setq org-ellipsis " ⬎") ;; ⤵ ↴ ⬎ ⤷

;; org-entities displays \alpha etc. as Unicode characters.
(setq org-pretty-entities t)

;; Hide the /italics/ and *bold* markers
(setq org-hide-emphasis-markers t)

;; Hit return on a link to open it in a browser
(setq org-return-follows-link t)

;; Allow a) b) c) lists
(setq org-list-allow-alphabetical t)

;; "Single keys can be made to execute commands when the cursor is at
;; the beginning of a headline, i.e., before the first star."
(setq org-use-speed-commands t)

;; Embed an image with [[file:foo.png]] and then C-c C-x C-v to view
(setq org-display-inline-images t)

;; Automatically refresh inline images that are generated from Babel blocks
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; Display images when a file is loaded (I can always toggle them off if I don't want them)
(add-hook 'org-mode-hook (lambda () (org-toggle-inline-images)))

;; imenu integration
(add-hook 'org-mode-hook
	  (lambda () (imenu-add-to-menubar "Imenu")))

;; Preview LaTeX equations in buffers by showing images (C-c C-x C-l)
;; Details: http://orgmode.org/worg/org-tutorials/org-latex-preview.html
(setq org-latex-create-formula-image-program 'imagemagick)

;; Turn ' and " into ‘posh’ “quotes”
(setq org-export-with-smart-quotes t)

;; Hooks for prettify-symbols-mode
;; See also https://pank.eu/blog/pretty-babel-src-blocks.html for some cool stuff
(add-hook 'org-mode-hook
 	  (lambda ()
 	    (push '("<=" . ?≤) prettify-symbols-alist)
 	    (push '(">=" . ?≥) prettify-symbols-alist)
 	    (push '("#+BEGIN_SRC" . ?✎) prettify-symbols-alist)
 	    (push '("#+END_SRC" . ?□) prettify-symbols-alist)
 	    ))

(load "~/lisp/ob-julia/ob-julia.el")

(require 'org-protocol)
(setq org-hide-leading-stars t)
(setq org-tags-column -70)
(setq org-directory "~/ownCloud/Org")
(setq org-archive-location "~/ownCloud/Org/GTD-Archiv.org::Von %s")
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(add-to-list 'org-latex-classes
             '("tuftebook"
               "\\documentclass{tufte-book}\n
\\usepackage{color}
\\usepackage{amssymb}
\\usepackage{gensymb}
\\usepackage{nicefrac}
\\usepackage{units}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
             '("tuftehandout"
               "\\documentclass{tufte-handout}
\\usepackage{color}
\\usepackage{amssymb}
\\usepackage{amsmath}
\\usepackage{gensymb}
\\usepackage{nicefrac}
\\usepackage{units}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
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
   (julia . t)
   (ditaa . t)))
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-todo-keywords '((sequence "OFFEN(o)" "WARTEN(w@/!)" "|" "ERLEDIGT(e!)")
                          (sequence "|" "STORNIERT(s@!)")
                          (sequence "HABIT" "|" "ERLEDIGT")
                          (sequence "REVIEW" "|" "REVIEWED")
                          (sequence "JAHRESTAG" "|" "ERLEDIGT")
                          (sequence "|" "DELIGIERT (d@/!)")
                          (sequence "KAEFER(k)" "|" "ERSCHLAGEN(a!)")
                          (sequence "PROJEKT(p!)" "PR_INAKTIV(i)" "|" "PR_ERLEDIGT(g!)" "PR_STORNIERT(r@/!)")
                          (sequence "PROJEKT_ICH(c!)" "PR_INAKTIV(i)" "|" "PR_ERLEDIGT(g!)" "PR_STORNIERT(r@/!)")
                          (sequence "LISTE(l)" "|")))

(setq org-todo-keyword-faces
      '(("PROJEKT" . "#529dff")
        ("PROJEKT_ICH" . "#529dff")
        ("LISTE" . "#529dff")
        ("WARTEN" . "#de8485")
        ("STORNIERT" . "#999999")
        ("PR_INAKTIV" . "#999999")))

(setq org-fontify-done-headline t)
(setq org-log-into-drawer t)
(setq org-agenda-files (quote ("~/ownCloud/Org/GTD-Eingang.org" "~/ownCloud/Org/GTD-Aktiv.org" "~/ownCloud/Org/GTD-Gewohnheiten.org" "~/ownCloud/Org/GTD-Jahrestage.org" "~/ownCloud/Org/Termine.org")))
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-habit-preceding-days 0)
(setq org-agenda-window-setup 'current-window)

(setq org-agenda-custom-commands
      '(("o" agenda "Is' was, Doc?"
         ((org-agenda-ndays 3)
          (org-agenda-start-on-weekday nil)
          (org-show-context-detail 'minimal)
          (org-agenda-overriding-header "Is' was, Doc?")))
        )
      )

(setq org-agenda-include-diary t)
(setq org-default-notes-file (concat org-directory "/GTD-Eingang.org"))
(setq org-capture-templates
      '(("a" "Aufgabe" entry (file+headline (concat org-directory "/GTD-Eingang.org") "Eingang")
         "** OFFEN %?\n  %i\n")
        ("l" "Aufgabe (mit Link)" entry (file+headline (concat org-directory "/GTD-Eingang.org") "Eingang")
         "** OFFEN %?\n  %i\n  %a")
        ("t" "Termin" entry
         (file+datetree+prompt (concat org-directory "/Termine.org"))
         "* %^{Bezeichnung}\n<%(org-read-date nil nil org-read-date-final-answer)>\n%i\n%?\n")
        ("i" "Projektidee" plain (file (concat org-directory "/Projektideen.org"))
         "%?\n")
        ("k" "Käfer" entry (file+headline (concat org-directory "/GTD-Eingang.org") "Eingang")
         "** KAEFER %?\n  %i\n %a")
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
        ("x" "Lesezeichen" entry (file (concat org-directory "/Lesezeichen.org"))
         "* %c\n%U\n%i\n" :empty-lines 1)
        ))
(setq org-attach-directory "~/ownCloud/Org/Attachments")
(setq org-refile-targets (quote ((org-agenda-files :todo . "PROJEKT") (org-agenda-files :todo . "LISTE") (org-agenda-files :todo . "PR_FESTGEFAHREN") (org-agenda-files :tag . "eimer"))))
(setq org-refile-use-cache nil)
(setq org-stuck-projects (quote ("+LEVEL=2/+PROJEKT-PR_ERLEDIGT-PR_FESTGEFAHREN" ("OFFEN" "KAEFER") nil "")))
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key nil)

(provide 'init-org)
