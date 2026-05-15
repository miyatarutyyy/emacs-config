;;; init.el --- Main configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal Emacs configuration for miyatarutyyy.

;;; Code:

;;; ==========================================================
;;; Basic Settings
;;; ==========================================================

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Disable backup files and else
(setq make-backup-files nil
      create-lockfiles nil)

;; Put auto-save files in ~/.emacs.d/auto-save-list/
(make-directory (locate-user-emacs-file "auto-save-list/") t)

(setq auto-save-default t
      auto-save-file-name-transforms
      `((".*" ,(locate-user-emacs-file "auto-save-list/") t)))

;; Disable ring bell alert sound
(setq ring-bell-function #'ignore)

;; Use space instead of tabs
(setq-default indent-tabs-mode nil
	          tab-width 4)

;; Save the cursor's previous position
(save-place-mode 1)

;; Save previous opening files
(recentf-mode 1)

;; Reload automatically when the file has been changed
(global-auto-revert-mode 1)

;; Ask Question through Emacs mini buffer
(setq use-dialog-box nil)

;; y-or-n instead of yes-or-no
(setq use-short-answers t)

;;; ==========================================================
;;; straight.el bootstrap
;;; ==========================================================

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

(require 'use-package)
