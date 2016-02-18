(defun cz-nag-me ()
  (interactive)
  (with-current-buffer (get-buffer " *Echo Area 0*")
    (setq-local face-remapping-alist '((default (:height 5.0)))))
  (message "was machste denn?")
  (sit-for 3600)
  (with-current-buffer (get-buffer " *Echo Area 0*")
    (setq-local face-remapping-alist '((default (:height 1.0))))))

(run-with-timer 0 1200 'cz-nag-me)

(provide 'init-nagging)
