(setq ispell-program-name "aspell")

(setenv "LANG" "en_GB.UTF-8")
(setq ispell-dictionary "en")

;; this works for ispell-message
(setq ispell-message-dictionary-alist
      '(("^^To:[^\n,]+\\.com[ \t\n,>]" . "en")
        ("^^To:[^\n,]+\\.uk[ \t\n,>]" . "en")
        ("^To:[^\n,]+\\.de[ \t\n,>]" . "de")))

;; general toggling between English and German
(defun cz-toggle-dictionary ()
  (interactive)
  (setq the-dict ispell-dictionary)
  (cond
   ((string-equal the-dict "en")
    (progn (ispell-change-dictionary "de")
           (message "changed dictionary from English to German")))
   ((string-equal the-dict "de")
    (progn (ispell-change-dictionary "en")
           (message "changed dictionary from German to English")))))

(global-set-key (kbd "C-c C-x l") 'cz-toggle-dictionary)

(provide 'init-spelling)
