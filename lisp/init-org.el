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
          ("P" "Projekte" tags-todo "LEVEL=2"
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
  (setq org-default-notes-file (concat org-directory "/GTD-Eingang.org"))
  (setq org-capture-templates
        '(("a" "Aufgabe" entry (file+headline (concat org-directory "/GTD-Eingang.org") "Eingang")
           "** OFFEN %?\n  %i\n")
          ("l" "Aufgabe (mit Link)" entry (file+headline (concat org-directory "/GTD-Eingang.org") "Eingang")
           "** OFFEN %?\n  %i\n  %a")
          ("o" "Logbuch" entry (file+datetree (concat org-directory "/GTD-Logbuch.org"))
           "* %?\n     Hinzugefügt am %U\n  %i\n")
          ("k" "Käfer" entry (file+headline (concat org-directory "/GTD-Eingang.org") "Eingang")
           "** KAEFER %?\n  %i\n %a")
          ("z" "Zitat" plain (file "~/ownCloud/Notizen/Zitate.txt")
           "\n\n# # #\n\n%?\n")
          ("g" "Geschenkidee" plain (file "~/ownCloud/Notizen/Geschenkideen.txt")
           "\n%?\n")
          ("p" "Phrase" plain (file "~/ownCloud/Notizen/Phrasen.txt")
           "\n\n%?\n")
          ("b" "Buch" plain (file "~/ownCloud/Notizen/Buecher.txt"))
          ))
  (setq org-refile-targets (quote ((org-agenda-files :todo . "PROJEKT") (org-agenda-files :todo . "LISTE") (org-agenda-files :todo . "PR_FESTGEFAHREN") (org-agenda-files :tag . "eimer"))))
  (setq org-refile-use-cache nil)
  (setq org-stuck-projects (quote ("+LEVEL=2/+PROJEKT-PR_ERLEDIGT-PR_FESTGEFAHREN" ("OFFEN" "KAEFER") nil "")))
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq org-crypt-key nil))

(provide 'init-org)
