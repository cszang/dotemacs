(defun cz-tea-action ()
  (switch-to-buffer "*tea*")
  (insert "Tee ist fertig!")
)

(defun cz-tea ()
  (interactive)
  (setq cz-tea-time (ido-completing-read "Wie lange? " '("2" "3" "4" "5" "6" "7")))
  (run-at-time (concat cz-tea-time " min") nil 'cz-tea-action)
  )

(provide 'init-tea)
