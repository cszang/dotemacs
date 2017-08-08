(setq deft-directory "~/ownCloud/Zettelkasten")
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)
(setq deft-extensions '("org"))
(global-set-key (kbd "C-c C-d") 'deft)
(provide 'init-deft)

;; a minor mode for making working with deft the ultimate zettelkasten
;; experience

(require 's)
(require 'dash)

(defun zk-follow-internal-link ()
  (interactive)
  (setq zk-search-string (word-at-point))
  (deft)
  (setq deft-filter-regexp (list zk-search-string))
  (deft-filter-update)
  (deft-refresh-browser)
  )

(defun zk-insert-timestamp ()
  (interactive)
  (insert (format-time-string "%Y%m%d%k%M"))
  )

(defun zk-new-with-timestamp ()
  (interactive)
  (setq zk-time-string (format-time-string "%Y%m%d%H%M"))
  (deft)
  (setq deft-filter-regexp (list zk-time-string))
  (deft-filter-update)
  (deft-refresh-browser)
  )

(defun zk-insert-timestamp-for-internal-link ()
  (interactive)
  (setq zk-all-dated-files (directory-files deft-directory nil "^[0-9]\\{12\\}\\.*"))
  (setq zk-link-file (ido-completing-read "Link? " zk-all-dated-files))
  (insert (concat "§" (car (s-match "^[0-9]\\{12\\}" zk-link-file))))
  )

(defun flatten (mylist)
  (cond
   ((null mylist) nil)
   ((atom mylist) (list mylist))
   (t
    (append (flatten (car mylist)) (flatten (cdr mylist))))))

(defun zk-match-tag-in-buffer (f)
  "append all matches of tags in a buffer to a list"
  (save-excursion
    (setq zk-tagline-regex "^#\\+ZKTAGS:.*$")
    (setq zk-tag-regex "#\\w+")
    (find-file f)
    (setq zk-tagline (car (s-match zk-tagline-regex (buffer-string))))
    (setq zk-tags-in-note (s-match-strings-all zk-tag-regex zk-tagline))
    (setq zk-tag-list (append zk-tag-list zk-tags-in-note))
    (kill-buffer (current-buffer))))

(defun zk-get-tag-list ()
  "gets all tags from all notes"
  (interactive)
  (setq zk-start-buffer buffer-file-name)
  (setq zk-scan-files (-remove (lambda (f) (string= zk-start-buffer f))
                                 (directory-files deft-directory t ".org$")))
  (setq zk-tag-list '(()))
  (mapc 'zk-match-tag-in-buffer zk-scan-files)
  (setq zk-tag-list (-distinct (flatten zk-tag-list)))
  )

(defun zk-complete-tag ()
  "completes tags from all previously used tags"
  (interactive)
  (insert (concat (ido-completing-read "Schlagwort? " zk-tag-list) " " )))

(defun zk-insert-tagline ()
  (interactive)
  (zk-get-tag-list)
  (backward-page)
  (open-line 2)
  (insert (concat "#+ZKTAGS: " (ido-completing-read "Schlagwort? " zk-tag-list) " " )))

(defun zk-find-similar ()
  (interactive)
  (setq zk-this-file (file-name-base))
  (setq zk-id-this-file (car (s-match "^[0-9]\\{12\\}" zk-this-file)))
  (setq zk-similar (shell-command-to-string (concat "~/.emacs.d/R/concordance " zk-id-this-file)))
  (other-window 1)
  (switch-to-buffer-other-window "*Ähnliche Notizen*")
  (insert "Ähnliche Notizen:\n" zk-similar)
  (zk-mode)
  )

(define-minor-mode zk-mode
  "Some functionality described by Sascha Fast and Christian
Tietze about their nvAlt Zettelkasten workflow."
  :lighter " zk"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c o") 'zk-follow-internal-link)
            (define-key map (kbd "C-c d") 'zk-new-with-timestamp)
            (define-key map (kbd "C-c l") 'zk-insert-timestamp-for-internal-link)
            (define-key map (kbd "C-c t") 'zk-insert-tagline)
            (define-key map (kbd "C-c #") 'zk-complete-tag)
            (define-key map (kbd "C-c s") 'zk-find-similar)
            map)
  (auto-fill-mode)
  )

(add-hook 'deft-mode-hook 'zk-mode)
(add-hook 'deft-mode-hook (lambda () (zk-get-tag-list)))
(add-hook 'markdown-mode-hook 'zk-mode)
