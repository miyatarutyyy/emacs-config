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

;;; ==========================================================
;;; UI
;;; ==========================================================

;; Load my original theme
(add-to-list 'custom-theme-load-path
             (locate-user-emacs-file "theme/"))

(load-theme 'my-theme t)

(set-fringe-mode 10)

(global-display-line-numbers-mode 1)

;; Change line-number color depending on whether the buffer is editable.
(defvar-local my/editable-line-number-face-cookie nil
  "Face remap cookie for line numbers in editable buffers.")

(defvar-local my/editable-current-line-number-face-cookie nil
  "Face remap cookie for the current line number in editable buffers.")

(defun my/update-line-number-color-by-read-only ()
  "Use subdued line numbers in read-only buffers, vivid ones in editable buffers."
  ;; Remove previous temporary overrides.
  (when my/editable-line-number-face-cookie
    (face-remap-remove-relative my/editable-line-number-face-cookie)
    (setq my/editable-line-number-face-cookie nil))

  (when my/editable-current-line-number-face-cookie
    (face-remap-remove-relative my/editable-current-line-number-face-cookie)
    (setq my/editable-current-line-number-face-cookie nil))

  (if buffer-read-only
      (progn
        (setq my/editable-line-number-face-cookie
              (face-remap-add-relative
               'line-number
               '(:foreground "#3A3A3A")))

        (setq my/editable-current-line-number-face-cookie
              (face-remap-add-relative
               'line-number-current-line
               '(:foreground "#5A5A5A" :weight bold))))
    (setq my/editable-line-number-face-cookie
          (face-remap-add-relative
           'line-number
           '(:foreground "#6F7A1F")))

    (setq my/editable-current-line-number-face-cookie
          (face-remap-add-relative
           'line-number-current-line
           '(:foreground "#39FF14" :weight bold)))))

(add-hook 'read-only-mode-hook
          #'my/update-line-number-color-by-read-only)

(add-hook 'find-file-hook
          #'my/update-line-number-color-by-read-only)

(add-hook 'after-change-major-mode-hook
          #'my/update-line-number-color-by-read-only)

;; Do not visually wrap long lines
(setq-default truncate-lines t)

;; Disable line numbers in special buffers
(dolist (mode '(term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook
                help-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode 0))))

;; Smooth scrolling (keyboard, mouse, trackpad).
(setq scroll-conservatively 3
      scroll-margin 0
      scroll-step 1
      scroll-preserve-screen-position t
      pixel-scroll-precision-interpolate-page t)
(pixel-scroll-precision-mode 1)

(use-package ultra-scroll
  :demand t
  :config
  (ultra-scroll-mode 1))

(use-package scroll-on-jump
  :demand t
  :custom
  (scroll-on-jump-duration 0.12)
  (scroll-on-jump-smooth t)
  :config
  (dolist (command '(next-line
                     previous-line
                     recenter-top-bottom
                     beginning-of-buffer
                     end-of-buffer))
    (advice-add command :around #'scroll-on-jump-advice--wrapper))
  (dolist (command '(scroll-up-command
                     scroll-down-command))
    (advice-add command :around #'scroll-on-jump-advice--with-scroll-wrapper)))

;; Set the default font size
(set-face-attribute 'default nil :height 100)
