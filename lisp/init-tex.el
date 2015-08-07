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

(provide 'init-tex)
