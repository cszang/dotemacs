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
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq-default TeX-PDF-mode t)
  (add-hook 'TeX-mode-hook
            (lambda ()
              (add-to-list 'TeX-output-view-style
                           '("^pdf$" "."
                             "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b")))
            )
  (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
  (setq TeX-view-program-list
        '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (push
                                '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
                                  :help "Run latexmk on file")
                                TeX-command-list)))
  (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
  )

(provide 'init-tex)
