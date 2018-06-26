;; ZETTELKASTEN
;; a minor mode for making working with deft the ultimate zettelkasten
;; experience

(require 's)
(require 'dash)
(require 'thingatpt)

;; file to record Zettel statistics (currently only growth of the Zettelkasten)
(setq zk-stats-file (concat deft-directory "/000000000000 Zettel-Stats.txt"))

(setq zk-position-list ())
(setq zk-buffer-list ())
(setq zk-jump-position 0)

(defun zk-push-current-pos ()
  (interactive)
  (push (point) zk-position-list)
  (push (buffer-name) zk-buffer-list)
  )

(defun zk-save-current-position ()
  (interactive)
  (zk-push-current-pos)
  (message "Saved current position.")
  )

(defun zk-jump-back ()
  (interactive)
  (setq zk-jump-position (+ zk-jump-position 1))
  (switch-to-buffer (nth zk-jump-position zk-buffer-list))
  (goto-char (nth zk-jump-position zk-position-list))
  )

(defun zk-jump-forward ()
  (interactive)
  (setq zk-jump-position (- zk-jump-position 1))
  (switch-to-buffer (nth zk-jump-position zk-buffer-list))
  (goto-char (nth zk-jump-position zk-position-list))
  )

(defun zk-current-time ()
  (format-time-string "%Y%m%d%H%M")
  )

(defun zk-follow-internal-link ()
  (interactive)
  (zk-push-current-pos)
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
  (setq zk-time-string (zk-current-time))
  (deft)
  (setq deft-filter-regexp (list zk-time-string))
  (deft-filter-update)
  (deft-refresh-browser)
  )

(defun zk-insert-timestamp-for-internal-link ()
  (interactive)
  (setq zk-all-dated-files (directory-files deft-directory nil "^[0-9]\\{12\\}\\.*"))
  (setq zk-link-file (ido-completing-read "Link? " zk-all-dated-files))
  (insert (concat "[[" (car (s-match "^[0-9]\\{12\\}" zk-link-file)) "]]"))
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
    (setq zk-tagline-regex "^tags:.*$")
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
                                 (directory-files deft-directory t ".txt$")))
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
  (insert (concat "tags: " (ido-completing-read "Schlagwort? " zk-tag-list) " " )))

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

(defun zk-zettel-reference-at-point ()
  (interactive)
  (setq zk-all-dated-files (directory-files deft-directory nil "^[0-9]\\{12\\}\\.*"))
  (setq current-id (thing-at-point 'symbol))
  (message (seq-find (lambda (zettel)
                       (s-starts-with? current-id zettel))
                     zk-all-dated-files "No matching Zettel found."))
  )

(defun zk-goto-zettel-at-point ()
  (interactive)
  (zk-push-current-pos)
  (setq zk-all-dated-files (directory-files deft-directory nil "^[0-9]\\{12\\}\\.*"))
  (setq current-id (thing-at-point 'symbol))
  (setq zettel-at-point (seq-find (lambda (zettel)
                                    (s-starts-with? current-id zettel))
                                  zk-all-dated-files ""))
  (if (s-equals? "" zettel-at-point)
      (message "No matching Zettel found.")
    (find-file (concat deft-directory "/" zettel-at-point))
    )
  )

(defun zk-insert-reference-skeleton ()
  (interactive)
  (insert "---")
  (newline)
  (clipboard-yank)
  (newline)
  (insert "---")
  (newline)
  (insert "tags: #ref #todo")
  (newline)
  (insert "tldr:")
  (newline)
  )

(defun zk-count-zettels ()
  (length (directory-files deft-directory "\\.txt\\'"))
  )

(defun zk-write-stats ()
  (interactive)
  (append-to-file (concat (zk-current-time) " " (number-to-string (zk-count-zettels))) nil zk-stats-file)
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
            (define-key map (kbd "C-c i") 'zk-zettel-reference-at-point)
            (define-key map (kbd "M-.") 'zk-goto-zettel-at-point)
            (define-key map (kbd "M-_") 'zk-jump-back)
            (define-key map (kbd "M-*") 'zk-jump-forward)
            (define-key map (kbd "M-#") 'zk-save-current-position)
            (define-key map (kbd "M-§") 'zk-insert-reference-skeleton)
            map)
  (auto-fill-mode)
  )

(defun zk-minor-mode-on ()
  "Turn on `zk' mode."
  (interactive)
  (zk-mode 1))

(add-hook 'markdown-mode-hook 'zk-minor-mode-on)

(add-hook 'deft-mode-hook (lambda () (zk-get-tag-list)))
(add-hook 'deft-mode-hook (lambda () (deft-refresh)))
(add-hook 'deft-mode-hook (lambda () (zk-write-stats)))
