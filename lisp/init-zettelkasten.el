(setq zettelkasten-directory "~/ownCloud/Zettelkasten")

;; Zettelkasten is built on top of Deft mode for quick filtering, the
;; rest is custom made
(use-package deft
  :bind
  ("<f9>" . deft)
  ("C-c C-z" . zk-new-zettel)
  ("C-c n" . zk-new-zettel)
  :config
  (use-package deft)
  (setq deft-extensions '("zkn" "txt" "org" "md"))
  (setq deft-text-mode 'zettelkasten-mode)
  (setq deft-directory zettelkasten-directory)
  (setq deft-use-filename-as-title t))

(defun flatten (mylist)
  (cond
   ((null mylist) nil)
   ((atom mylist) (list mylist))
   (t
    (append (flatten (car mylist)) (flatten (cdr mylist))))))

(defun zk-match-tag-in-buffer (f)
  "append all matches of tags in a buffer to a list"
  (save-excursion
    (setq tag-regex "^\\(@\\|\\+\\|\\$\\).+$")
    (find-file f)
    (setq zk-tags-in-zettel (s-match-strings-all tag-regex (buffer-string)))
    (setq zk-tag-list (append zk-tag-list zk-tags-in-zettel))
    (kill-buffer (current-buffer))))

(defun zk-get-tag-list()
  "gets all tags from all zettelfiles"
  (interactive)
  (require 'dash)
  (setq zk-tag-list '(()))
  (mapc 'zk-match-tag-in-buffer
        (directory-files zettelkasten-directory t ".zkn$"))
  (setq zk-tag-list (-distinct (flatten zk-tag-list))))

(defun zk-complete-tag ()
  "completes zettelkasten tags from all previously used tags"
  (interactive)
  (insert (ido-completing-read "Schlagwort? " zk-tag-list))
  (newline-and-indent))

(defun zk-complete-structure ()
  "completes structural elements for Zettelkasten system"
  (interactive)
  (setq zk-structures (list "*Schlagwörter*" "*Literatur*" "*Verknüpfungen*"))
  (setq zk-structure (ido-completing-read "Struktur? " zk-structures))
  (insert zk-structure)
  (newline-and-indent)
  (cond ((string= "*Schlagwörter*" zk-structure) (zk-complete-tag))
        ((string= "*Literatur*" zk-structure) (yank))))

(defun zk-new-zettel ()
  "create a new zettelkasten-zettel and visit the file"
  (interactive)
  (require 'cl)
  (require 'f)
  (setq zk-new-number
        (+ 1
           (reduce #'max
                   (mapcar 'string-to-number
                           (mapcar 'file-name-sans-extension
                                   (directory-files zettelkasten-directory nil "^\[0-9\]\\{4\\}")
                                   )))))
  (setq new-number-preceeding-zeros
        (cond ((>= zk-new-number 1000 ) ""   )
              ((>= zk-new-number 100  ) "0"  )
              ((>= zk-new-number 10   ) "00" )
              ((>= zk-new-number 0    ) "000")))
  (setq new-zettel-file-name
        (concat (f-slash zettelkasten-directory) new-number-preceeding-zeros (number-to-string zk-new-number) ".zkn"))
  (find-file new-zettel-file-name)
  (zk-get-tag-list))

(setq zk-keywords
      '(("^\\*.+\\*" . font-lock-builtin-face)
        ("^@.+" . font-lock-constant-face)
        ("^\\+.+" . font-lock-keyword-face)
        ("^\\$.+" . font-lock-function-name-face)))

(define-minor-mode zettelkasten-mode
  "Zettelkasten management"
  :lighter " Zkn"
  :keymap (let ((map (make-sparse-keymap)))
            (progn
              (define-key map (kbd "@") 'zk-complete-tag)
              (define-key map (kbd "*") 'zk-complete-structure)
              (define-key map (kbd "C-c C-n") 'zk-new-zettel)
              )
            map)
  (auto-fill-mode)
  (turn-on-orgstruct)
  (font-lock-add-keywords nil zk-keywords)
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer))))
  )

(add-to-list 'auto-mode-alist '("\\.zkn" . zettelkasten-mode))
(global-set-key (kbd "C-c z") 'zk-new-zettel)

(provide 'init-zettelkasten)
