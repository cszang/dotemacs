(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)

;; the entire setup is shamelessly stolen from
;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
;; and
;; https://vxlabs.com/2017/02/07/mu4e-0-9-18-e-mailing-with-emacs-now-even-better/

(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "WZW"
           :match-func (lambda (msg) (when msg
                                  (string-prefix-p "/WZW" (mu4e-message-field msg :maildir))))
           :vars '(
                   (mu4e-trash-folder . "/WZW/Trash")
                   (mu4e-refile-folder . "/WZW/Sonstiges")
                   (mu4e-compose-signature  .
                                            (concat
                                             "Dr. Christian Zang\n"
                                             "\n"
                                             "Land Surface-Atmosphere Interactions\n"
                                             "Technische Universität München\n"
                                             "Hans-Carl-von-Carlowitz-Platz 2, 85354 Freising, Germany\n"
                                             "phone: +49 8161 714766, fax: +49 8161 714767\n"))
                   ))
         ,(make-mu4e-context
           :name "Posteo"
           :match-func (lambda (msg) (when msg
                                  (string-prefix-p "/Posteo" (mu4e-message-field msg :maildir))))
           :vars '(
                   (mu4e-trash-folder . "/Posteo/Trash")
                   (mu4e-refile-folder . "/Posteo/Sonstiges")
                   (mu4e-compose-signature  . nil)
                   ))
         ))

(setq mu4e-alert-interesting-mail-query
      (concat
       "flag:unread maildir:/WZW/INBOX "
       "OR "
       "flag:unread maildir:/Posteo/INBOX"
       ))
(mu4e-alert-enable-mode-line-display)
(defun cz-refresh-mu4e-alert-mode-line ()
  (interactive)
  (mu4e~proc-kill)
  (mu4e-alert-enable-mode-line-display)
  )
(run-with-timer 0 60 'cz-refresh-mu4e-alert-mode-line)

(setq mu4e-sent-folder "/sent"
      mu4e-drafts-folder "/drafts"
      user-mail-address "christian.zang@wzw.tum.de"
      smtpmail-default-smtp-server "mail.wzw.tum.de"
      smtpmail-smtp-server "mail.wzw.tum.de"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl
      )

(defvar my-mu4e-account-alist
  '(("WZW"
     (mu4e-sent-folder "/WZW/sent")
     (user-mail-address "christian.zang@wzw.tum.de")
     (smtpmail-smtp-user "chzang")
     (smtpmail-local-domain "mail.wzw.tum.de")
     (smtpmail-default-smtp-server "mail.wzw.tum.de")
     (smtpmail-smtp-server "mail.wzw.tum.de")
     )
    ("Posteo"
     (mu4e-sent-folder "/Posteo/sent")
     (user-mail-address "zang@posteo.de")
     (smtpmail-smtp-user "zang@posteo.de")
     (smtpmail-local-domain "posteo.de")
     (smtpmail-default-smtp-server "posteo.de")
     (smtpmail-smtp-server "posteo.de")
     )
    ))

(defun my-mu4e-set-account ()
  "Set the account for composing a message.
   This function is taken from: 
     https://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html"
  (let* ((account
    (if mu4e-compose-parent-message
        (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
    (string-match "/\\(.*?\\)/" maildir)
    (match-string 1 maildir))
      (completing-read (format "Compose with account: (%s) "
             (mapconcat #'(lambda (var) (car var))
            my-mu4e-account-alist "/"))
           (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
           nil t nil nil (caar my-mu4e-account-alist))))
   (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
  (mapc #'(lambda (var)
      (set (car var) (cadr var)))
        account-vars)
      (error "No email account found"))))
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
(setq mu4e-compose-context-policy 'ask)

;; trash instead of deleting
(defun remove-nth-element (nth list)
  (if (zerop nth) (cdr list)
    (let ((last (nthcdr (1- nth) list)))
      (setcdr last (cddr last))
      list)))
(setq mu4e-marks (remove-nth-element 5 mu4e-marks))
(add-to-list 'mu4e-marks
     '(trash
       :char ("d" . "▼")
       :prompt "dtrash"
       :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
       :action (lambda (docid msg target) 
                 (mu4e~proc-move docid
                                 (mu4e~mark-check-target target) "-N"))))

;; Include a bookmark to open all of my inboxes
(add-to-list 'mu4e-bookmarks
       (make-mu4e-bookmark
        :name "All Inboxes"
        :query "maildir:/WZW/INBOX OR maildir:/Posteo/INBOX"
        :key ?i))

;; Close message after sending
(setq message-kill-buffer-on-exit t)
;; Don't ask for a 'context' upon opening mu4e
(setq mu4e-context-policy 'ask)
;; Don't ask to quit
(setq mu4e-confirm-quit nil)
(setq mu4e-get-mail-command "offlineimap")
(setq mu4e-compose-format-flowed t)
(setq mu4e-compose-in-new-frame nil)
(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
(setq mu4e-view-show-addresses 't)
(setq mu4e-headers-fields
    '( (:human-date          .  25)
       (:flags               .   6)
       (:from                .  22)
       (:subject          .  nil)))

;; org-related
(require 'org-mu4e)
(setq org-mu4e-link-query-in-headers-mode nil)
(add-hook 'message-mode-hook 'turn-on-orgtbl)
(add-hook 'message-mode-hook 'turn-on-orgstruct++)

(provide 'init-mail)
