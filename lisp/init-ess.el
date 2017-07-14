(add-to-list 'load-path "~/lisp/ess/lisp/")
(load "ess-site")
(add-to-list 'auto-mode-alist '("\\.R\\'" . R-mode))
(setq ess-language "R")
(setq-default ess-dialect "R")
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
(setq inferior-julia-program-name "/usr/local/bin/julia")
(setq ansi-color-for-comint-mode 'filter)
(add-hook 'ess-mode-hook 'auto-complete-mode)
(ess-toggle-underscore nil)
(add-hook 'inferior-ess-mode-hook 'auto-complete-mode)
(setq ess-use-auto-complete t)
(add-hook 'ess-mode-hook 'smartparens-mode)
(add-hook 'ess-mode-hook 'turn-on-auto-fill)
(add-hook 'inferior-ess-mode-hook 'turn-on-auto-fill)
(setq ess-default-style 'RStudio)

;; visit topic and kill Occur buffer
(defun cz-goto-R-section ()
  (interactive)
  (occur-mode-goto-occurrence)
  (other-window 1)
  (quit-window))

;; occur sections in R code like Rstudio does
(defun cz-occur-R-sections ()
  (interactive)
  (occur "^#+.*-\\{4,\\}")
  (other-window 1)
  (next-line)
  (local-set-key (kbd "RET") 'cz-goto-R-section)
  )

;; insert new R section
(defun cz-insert-R-section ()
  (interactive)
  (setq the-section-name
        (concat "### " (read-string "Section? ") " "))
  (move-beginning-of-line nil)
  (insert
   (concat the-section-name
           (make-string
            (- 69 (string-width the-section-name)) ?-)
           ))
  (newline 2)
  (open-line)
  )

;; insert magrittr pipe
(defun cz-insert-magrittr-pipe ()
  (interactive)
  (just-one-space)
  (insert "%>%")
  (just-one-space)
  )

;; insert Rmd code chunk; from
;; http://emacs.stackexchange.com/questions/27405/insert-code-chunk-in-r-markdown-with-yasnippet-and-polymode
(defun cz-insert-r-chunk (header) 
  "Insert an r-chunk in markdown mode. Necessary due to interactions between polymode and yas snippet" 
  (interactive "sHeader: ") 
  (insert (concat "```{r " header "}\n\n```")) 
  (forward-line -1))

(define-key ess-mode-map (kbd "C-c C-a") 'cz-insert-R-section)
(define-key ess-mode-map (kbd "C-c =") 'cz-occur-R-sections)
(define-key ess-mode-map (kbd "C-c m") 'cz-insert-magrittr-pipe)
;; (define-key markdown-mode-map (kbd "C-c r") 'cz-insert-r-chunk)


(provide 'init-ess)
