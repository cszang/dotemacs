(setq ispell-program-name (executable-find "hunspell"))

(setq ispell-dictionary "en_GB")

;; this works for ispell-message
(setq ispell-message-dictionary-alist
      '(("^^To:[^\n,]+\\.com[ \t\n,>]" . "en_GB")
        ("^^To:[^\n,]+\\.uk[ \t\n,>]" . "en_GB")
        ("^To:[^\n,]+\\.de[ \t\n,>]" . "de_DE_frami")))

;; general toggling between English and German
(defun cz-toggle-dictionary ()
  (interactive)
  (setq the-dict ispell-dictionary)
  (cond
   ((string-equal the-dict "en_GB")
    (progn (ispell-change-dictionary "de_DE_frami")
           (message "changed dictionary from English to German")))
   ((string-equal the-dict "de_DE_frami")
    (progn (ispell-change-dictionary "en_GB")
           (message "changed dictionary from German to English")))))

(global-set-key (kbd "C-c C-x l") 'cz-toggle-dictionary)

(provide 'init-spelling)
