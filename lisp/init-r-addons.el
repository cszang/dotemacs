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
  (setq the-section-name (read-string "Section? "))
  (move-beginning-of-line nil)
  (message the-section-name)
  (insert (concat "### " the-section-name " ----"))
  (newline-and-indent)
  )

;; insert magrittr pipe
(defun cz-insert-magrittr-pipe ()
  (interactive)
  (just-one-space)
  (insert "%>%")
  )

(provide 'init-r-addons)
