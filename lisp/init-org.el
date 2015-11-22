(use-package org
  :bind
  ("C-c a" . org-agenda)
  ("C-c C-+" . org-capture)
  :mode
  ("\\.org$" . org-mode)
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
                            (sequence "HABIT" "|" "ERLEDIGT")
                            (sequence "JAHRESTAG" "|" "ERLEDIGT")
                            (sequence "LOVE" "|" "LOVED")
                            (sequence "|" "DELIGIERT (d@/!)")
                            (sequence "KAEFER(k)" "|" "ERSCHLAGEN(a!)")
                            (sequence "PROJEKT(p!)" "PR_FESTGEFAHREN(f)" "|" "PR_ERLEDIGT(i!)" "PR_STORNIERT(r@/!)")
                            (sequence "LISTE(l)" "|")))
  (setq org-todo-keyword-faces
        '(("PROJEKT" . "yellow")
          ("LISTE" . "yellow")
          ("WARTEN" . "orange")
          ("STORNIERT" . "grey")
          ("PR_FESTGEFAHREN" . "grey")))
  (setq org-fontify-done-headline t)
  (custom-set-faces
   '(org-headline-done 
     ((((class color) (min-colors 16) (background dark)) 
       (:foreground "#4f4f4f" :strike-through t)))))
  (setq org-log-into-drawer t)
  (setq org-agenda-files (quote ("~/ownCloud/Org/GTD-Eingang.org" "~/ownCloud/Org/GTD-Aktiv.org" "~/ownCloud/Org/GTD-Gewohnheiten.org" "~/ownCloud/Org/GTD-Jahrestage.org")))
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-habit-preceding-days 0)

  (setq org-agenda-custom-commands
        '(("o" agenda "Is' was, Doc?"
           ((org-agenda-tag-filter-preset '("-@daheim" "-@familie"))
            (org-agenda-ndays 3)
            (org-agenda-start-on-weekday nil)
            (org-agenda-overriding-header "Is' was, Doc?")))
          ("d" agenda "Daheim so..."
           ((org-agenda-tag-filter-preset '("-@büro" "-@büroleute" "-@campus"))
            (org-agenda-ndays 3)
            (org-agenda-start-on-weekday nil)
            (org-agenda-overriding-header "Daheim so...")))
          )
        )

  (setq org-agenda-include-diary t)
  (setq org-default-notes-file (concat org-directory "/GTD-Eingang.org"))
  (setq org-capture-templates
        '(("a" "Aufgabe" entry (file+headline (concat org-directory "/GTD-Eingang.org") "Eingang")
           "** OFFEN %?\n  %i\n")
          ("l" "Aufgabe (mit Link)" entry (file+headline (concat org-directory "/GTD-Eingang.org") "Eingang")
           "** OFFEN %?\n  %i\n  %a")
          ("k" "Käfer" entry (file+headline (concat org-directory "/GTD-Eingang.org") "Eingang")
           "** KAEFER %?\n  %i\n %a")
          ("z" "Zitat" entry (file "~/ownCloud/Org/Zitate.org")
           "* %?")
          ("g" "Geschenkidee" plain (file "~/ownCloud/Org/Geschenkideen.org")
           "* %?")
          ("p" "Phrase" plain (file "~/ownCloud/Org/Phrasen.org")
           "\n\n%?\n")
          ("b" "Buch" entry (file "~/ownCloud/Org/Buecher.org")
           "* LESEN %?\n  %U\n")
          ("m" "Motto" entry (file "~/ownCloud/Org/Mottos.org")
           "* %?")
          ("n" "Notiz" entry (file "~/ownCloud/Org/Notizen.org")
           "\n* %?\n  %U\n")
          ))
  (setq org-refile-targets (quote ((org-agenda-files :todo . "PROJEKT") (org-agenda-files :todo . "LISTE") (org-agenda-files :todo . "PR_FESTGEFAHREN") (org-agenda-files :tag . "eimer"))))
  (setq org-refile-use-cache nil)
  (setq org-stuck-projects (quote ("+LEVEL=2/+PROJEKT-PR_ERLEDIGT-PR_FESTGEFAHREN" ("OFFEN" "KAEFER") nil "")))
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq org-crypt-key nil)
  (setq org-ellipsis "⤵")
  ;; turn on org-bullets-mode for org-files
  (setq org-bullets-bullet-list '(" "))
  (add-hook 'org-mode-hook 'org-bullets-mode)
  )

(provide 'init-org)
