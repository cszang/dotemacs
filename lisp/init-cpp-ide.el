(add-hook 'c-mode-hook #'ggtags-mode)
(add-hook 'c++-mode-hook #'ggtags-mode)
(setq-default c-basic-offset 2)
(setq c-default-style "linux")
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(setq xah-lookup-browser-function 'eww)
(defun xah-lookup-cppreference (&optional word)
  "Lookup definition of current word or text selection in URL."
  (interactive)
  (xah-lookup-word-on-internet
   word
   "http://en.cppreference.com/mwiki/index.php?search=�"
   xah-lookup-browser-function))
(define-key c++-mode-map (kbd "C-c d") #'xah-lookup-cppreference)

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

(setq speedbar-show-unknown-files t)

(add-hook 'after-init-hook 'global-company-mode)
(delete 'company-semantic company-backends)
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)
(add-to-list 'company-backends 'company-c-headers)

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )
