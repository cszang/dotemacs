(setq deft-directory "~/ownCloud/Notizen")
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)
(setq deft-extensions '("txt", "md", "org"))
(add-to-list 'auto-mode-alist '("/Notizen/.*\\.txt\\'" . markdown-mode))
(global-set-key (kbd "C-c C-d") 'deft)
(provide 'init-deft)

;; a minor mode for making working with deft a bit more like nvAlt

(require 's)
(require 'dash)

(defun nv-follow-internal-link ()
  (interactive)
  (setq nv-search-string (word-at-point))
  (deft)
  (setq deft-filter-regexp (list nv-search-string))
  (deft-filter-update)
  (deft-refresh-browser)
  )

(defun nv-insert-timestamp ()
  (interactive)
  (insert (format-time-string "%Y%m%d%k%M"))
  )

(defun nv-new-with-timestamp ()
  (interactive)
  (setq nv-time-string (format-time-string "%Y%m%d%k%M"))
  (deft)
  (setq deft-filter-regexp (list nv-time-string))
  (deft-filter-update)
  (deft-refresh-browser)
  )

(defun nv-insert-timestamp-for-internal-link ()
  (interactive)
  (setq nv-all-dated-files (directory-files deft-directory nil "^[0-9]\\{12\\}\\.*"))
  (setq nv-link-file (ido-completing-read "Link? " nv-all-dated-files))
  (insert (concat "§" (car (s-match "^[0-9]\\{12\\}" nv-link-file))))
  )

(defun flatten (mylist)
  (cond
   ((null mylist) nil)
   ((atom mylist) (list mylist))
   (t
    (append (flatten (car mylist)) (flatten (cdr mylist))))))

(defun nv-match-tag-in-buffer (f)
  "append all matches of tags in a buffer to a list"
  (save-excursion
    (setq nv-tagline-regex "^tags:.*$")
    (setq nv-tag-regex "#\\w+")
    (find-file f)
    (setq nv-tagline (car (s-match nv-tagline-regex (buffer-string))))
    (setq nv-tags-in-note (s-match-strings-all nv-tag-regex nv-tagline))
    (setq nv-tag-list (append nv-tag-list nv-tags-in-note))
    (kill-buffer (current-buffer))))

(defun nv-get-tag-list ()
  "gets all tags from all notes"
  (interactive)
  (setq nv-start-buffer buffer-file-name)
  (setq nv-scan-files (-remove (lambda (f) (string= nv-start-buffer f))
                                 (directory-files deft-directory t ".txt$")))
  (setq nv-tag-list '(()))
  (mapc 'nv-match-tag-in-buffer nv-scan-files)
  (setq nv-tag-list (-distinct (flatten nv-tag-list)))
  )

(defun nv-complete-tag ()
  "completes tags from all previously used tags"
  (interactive)
  (insert (concat (ido-completing-read "Schlagwort? " nv-tag-list) " " )))

(defun nv-insert-tagline ()
  (interactive)
  (nv-get-tag-list)
  (insert (concat "tags: " (ido-completing-read "Schlagwort? " nv-tag-list) " " )))

(define-minor-mode nvalt-mode
  "Some functionality described by Sascha Fast and Christian
Tietze about their nvAlt Zettelkasten workflow."
  :lighter " nv"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c o") 'nv-follow-internal-link)
            (define-key map (kbd "C-c d") 'nv-new-with-timestamp)
            (define-key map (kbd "C-c l") 'nv-insert-timestamp-for-internal-link)
            (define-key map (kbd "C-c t") 'nv-insert-tagline)
            (define-key map (kbd "C-c #") 'nv-complete-tag)
            map)
  )

(add-hook 'deft-mode-hook 'nvalt-mode)
(add-hook 'deft-mode-hook (lambda () (nv-get-tag-list)))
(add-hook 'markdown-mode-hook 'nvalt-mode)
