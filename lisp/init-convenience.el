;;; fill/unfill paragraph as toggle with M-q, via
;;; http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph]
                #'endless/fill-or-unfill)

;; occur TODOs
(defun cz-occur-TODOs ()
  (interactive)
  (occur "TODO\\|FIXME\\|FXME\\|KAEFER\\|OFFEN")
  (other-window 1)
  (next-line)
  )

(global-set-key (kbd "S-SPC") 'other-frame)

(provide 'init-convenience)
