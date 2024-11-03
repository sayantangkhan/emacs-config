(use-package auth-source
  :no-require t
  :config (setq auth-sources '("~/.emacs.d/mu4e/authinfo.gpg")))

(setq epa-pinentry-mode 'loopback)

(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-drafts-folder "/umich-email/[Gmail]/Drafts")
(setq mu4e-sent-folder   "/umich-email/[Gmail]/Sent Mail")
(setq mu4e-trash-folder  "/umich-email/[Gmail]/Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; Don't include related messages, i.e. disable threads
(setq mu4e-headers-include-related nil)

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
    '( (:maildir "/umich-email/Inbox"              :key ?i)
       (:maildir "/umich-email/[Gmail]/Sent Mail"  :key ?s)
       (:maildir "/umich-email/[Gmail]/Trash"      :key ?t)
       (:maildir "/umich-email/[Gmail]/All Mail"   :key ?a)))

; get mail
(setq mu4e-get-mail-command "mbsync -c ~/.emacs.d/mu4e/mbsyncrc -a"
  ;; mu4e-html2text-command "w3m -T text/html" ;;using the default mu4e-shr2text
  mu4e-view-prefer-html t
  mu4e-update-interval nil
  mu4e-headers-auto-update t
  mu4e-compose-format-flowed t)

;; to view selected message in the browser, no signin, just html mail
(add-to-list 'mu4e-view-actions
  '("ViewInBrowser" . mu4e-action-view-in-browser) t)


;; something about ourselves
(setq
   user-mail-address "saykhan@umich.edu"
   user-full-name  "Sayantan Khan"
   mu4e-compose-signature
    (concat
      "Best,\n"
      "Sayantan\n"))

;; enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; every new email composition gets its own frame!
(setq mu4e-compose-in-new-frame t)

(add-hook 'mu4e-view-mode-hook #'visual-line-mode)

;; <tab> to navigate to links, <RET> to open them in browser
(add-hook 'mu4e-view-mode-hook
  (lambda()
;; try to emulate some of the eww key-bindings
(local-set-key (kbd "<RET>") 'mu4e~view-browse-url-from-binding)
(local-set-key (kbd "<tab>") 'shr-next-link)
(local-set-key (kbd "<backtab>") 'shr-previous-link)))

;; from https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/elgoumx
(add-hook 'mu4e-headers-mode-hook
      (defun my/mu4e-change-headers ()
    (interactive)
    (setq mu4e-headers-fields
	  `((:human-date . 25) ;; alternatively, use :date
	(:flags . 6)
	(:from . 22)
	(:subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
	(:size . 7)))))

;; ;; if you use date instead of human-date in the above, use this setting
;; ;; give me ISO(ish) format date-time stamps in the header list
;; (setq mu4e-headers-date-format "%Y-%m-%d %H:%M")

;; spell check
(add-hook 'mu4e-compose-mode-hook
    (defun my-do-compose-stuff ()
       "My settings for message composition."
       (visual-line-mode)
       (org-mu4e-compose-org-mode)
	   (use-hard-newlines -1)
       (flyspell-mode)))

;;rename files when moving
;;NEEDED FOR MBSYNC
(setq mu4e-change-filenames-when-moving t)

;;set up queue for offline email
;;use mu mkdir  ~/Maildir/acc/queue to set up first
(setq smtpmail-queue-mail nil)  ;; start in normal mode

;;from the info manual
(setq mu4e-attachment-dir  "~/Downloads")

(setq message-kill-buffer-on-exit t)
(setq mu4e-compose-dont-reply-to-self t)

(require 'org-mu4e)

;; convert org mode to HTML automatically
(setq org-mu4e-convert-to-html t)

;;from vxlabs config
;; show full addresses in view message (instead of just names)
;; toggle per name with M-RET
(setq mu4e-view-show-addresses 't)

;; don't ask when quitting
(setq mu4e-confirm-quit nil)

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

;; (require 'smtpmail)
(require 'smtpmail-async)

(setq message-send-mail-function 'async-smtpmail-send-it
      send-mail-function 'async-smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; alternatively, for emacs-24 you can use:
;; (setq message-send-mail-function 'smtpmail-send-it
;;     smtpmail-stream-type 'starttls
;;     smtpmail-default-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
