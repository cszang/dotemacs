;; some ESS enhancements

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


;; restart R (from source file)
(defun cz-restart-R ()
  (interactive)
  (setq ess-project-root-dir (projectile-project-root))
  (ess-switch-to-end-of-ESS)
  (ess-quit)
  (kill-buffer)
  (switch-to-buffer ess-project-root-dir)
  (R)
  )

;; start logvogel monitoring process
(defun cz-logvogel ()
  (interactive)
  (setq ess-project-root-dir (projectile-project-root))
  (switch-to-buffer ess-project-root-dir)
  (R)
  (rename-buffer "*logvogel::autostatus()*")
  (insert "logvogel::autostatus()")
  (autopair-newline)
  )
