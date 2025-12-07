;;; ============================================================
;;; Mail - Sending Mail via msmtp
;;;
;;; msmtp is used as the send-only backend.
;;; This file configures message.el to integrate with msmtp
;;; ============================================================


;; --------------------------------------------------
;; :Load Private Mail Information:
;; (This file must not be committed to GitHub)
;; --------------------------------------------------
(load (expand-file-name "private/mail-private.el" user-emacs-directory) t)

;; If the private file defined a default email, use it.
(when (boundp 'my/default-email)
  (setq user-mail-address my/default-email))



;; --------------------------------------------------
;; :Mailing Engine:
;; 1) Use msmtp as the backend for sending email
;; --------------------------------------------------
(setq send-mail-function 'sendmail-send-it
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header)


;; --------------------------------------------------
;; :From Address:
;; 2) Default From address (fallback)
;; --------------------------------------------------
;; If private email is not loaded, you may set a fallback:
;; (setq user-mail-address "hogefuga@example.com")
;; Normally left unset - the private file should define it.


;; --------------------------------------------------
;; :Default Header
;; 3) Insert Cc/Bcc lines automatically in new message
;; --------------------------------------------------
(setq message-default-mail-headers
      "Cc: \nBcc: \n")


;; --------------------------------------------------
;; :Saving Draft:
;; 4) Drafts
;; --------------------------------------------------
(setq message-auto-save-directory "~/.emacs.d/maildrafts")


;; --------------------------------------------------
;; :Mail Agent:
;; 5) Message as the default Mail User Agent
;; --------------------------------------------------
(setq mail-user-agent 'message-user-agent)
