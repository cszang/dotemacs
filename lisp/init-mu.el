(add-to-list 'load-path "/usr/local/Cellar/mu/0.9.16/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)
(require 'org-mu4e)

;; http://xahlee.blogspot.com/2010/09/elisp-read-file-content-in-one-shot.html
;; we’ll use this to read your different signatures from files
(defun get-string-from-file (filePath)
  "Return FILEPATH’s file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(setq mu4e-maildir "~/Mail"
      mu4e-drafts-folder "/Drafts"
      mu4e-sent-folder   "/Sent"
      mu4e-trash-folder  "/Trash"
      mu4e-get-mail-command "offlineimap"
      mu4e-headers-include-related t
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "posteo.de"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 587
      user-mail-address "christian.zang@wzw.tum.de"
      user-full-name "Christian S. Zang"
      ;; mu4e-use-fancy-chars t
      mu4e-attachment-dir "~/Downloads"
      message-kill-buffer-on-exit t
      mu4e-headers-date-format "%Y-%m-%d %H:%M"
      message-citation-line-format "%N @ %Y-%m-%d %H:%M %Z:\n"
      message-citation-line-function 'message-insert-formatted-citation-line
      )

(setq mu4e-user-mail-address-list
      '("christian.zang@wzw.tum.de"
        "zang@posteo.de"))

;; multi-account settings stolen from Charl Botha
;; https://vxlabs.com/2014/06/06/configuring-emacs-mu4e-with-nullmailer-offlineimap-and-multiple-identities/
(defun csz-mu4e-personal()
  (interactive)
  (message "personal mail account")
  (setq user-mail-address "zang@posteo.de"
        mu4e-compose-signature (get-string-from-file "~/.signature.personal"))
  )

(defun csz-mu4e-work()
  (interactive)
  (message "work mail account")
  (setq user-mail-address "christian.zang@wzw.tum.de"
        mu4e-compose-signature (get-string-from-file "~/.signature.work"))
  )

(define-key mu4e-main-mode-map (kbd "<f1>") 'csz-mu4e-personal)
(define-key mu4e-main-mode-map (kbd "<f2>") 'csz-mu4e-vxlabs)
(define-key mu4e-headers-mode-map (kbd "<f1>") 'csz-mu4e-personal)
(define-key mu4e-headers-mode-map (kbd "<f2>") 'csz-mu4e-vxlabs)

(defun csz-mu4e-is-message-to (msg rx)
  "Check if to, cc or bcc field in MSG has any address in RX."
  (or (mu4e-message-contact-field-matches msg :to rx)
      (mu4e-message-contact-field-matches msg :cc rx)
      (mu4e-message-contact-field-matches msg :bcc rx)))

(add-hook 'mu4e-compose-pre-hook
           (defun my-set-from-address ()
             "Set current identity based on to, cc, bcc of original."
             (let ((msg mu4e-compose-parent-message)) ;; msg is shorter…
               (if msg
                   (cond
                    ((csz-mu4e-is-message-to msg (list "zang@posteo.de"
                                                       "zang_konsum@posteo.de"
                                                       "zang_services@posteo.de"
                                                       "zang_news@posteo.de"))
                     (csz-mu4e-personal))
                    ((csz-mu4e-is-message-to msg (list "christian.zang@wzw.tum.de"
                                                       "christian.zang@tum.de"
                                                       "ga68tux@tum.de"))
                     (csz-mu4e-work)))))))

;; convenience function for starting the whole mu4e in its own frame
;; posted by the author of mu4e on the mailing list
(defun mu4e-in-new-frame ()
  "Start mu4e in new frame."
  (interactive)
  (select-frame (make-frame))
  (mu4e))

;; open html mails in browser on demand
(add-to-list 'mu4e-headers-actions
             '("in browser" . mu4e-action-view-in-browser) t)
(add-to-list 'mu4e-view-actions
             '("in browser" . mu4e-action-view-in-browser) t)

;; custom bookmarks
(add-to-list 'mu4e-bookmarks
             '("maildir:/sent" "Sent" ?s))
(add-to-list 'mu4e-bookmarks
             '("maildir:/inbox" "Inbox" ?i))

;; (setq mu4e-html2text-command
;;   "textutil -stdin -format html -convert txt -stdout")

(setq mu4e-html2text-command "html2text")

(setq mu4e-headers-fields
      '((:date . 25)
         (:flags . 6)
         (:from . 22)
         (:subject . nil)))

(provide 'init-mu)
