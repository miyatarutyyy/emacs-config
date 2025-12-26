;;; ==========================================================
;;; UI / Appearance
;;; ==========================================================

;; ----------------------------------------------------------
;; 1) Frame / Window basic UI
;; ----------------------------------------------------------

(tool-bar-mode -1) ; disable tool-bar on current frame
(add-to-list 'default-frame-alist '(tool-bar-lines . 0)) ; disable toolbar for frames

(tab-bar-mode -1)        ; disable Emacs tab-bar (top)
(global-tab-line-mode 1) ; enable simple tab-line (bottom)

(global-display-line-numbers-mode 1) ; line number in this buffer
(line-number-mode 1)                 ; show current line in modeline
(column-number-mode 1)               ; show current column in modeline

(setq default-input-method nil) ; disable default input method



;; ----------------------------------------------------
;; 2) font & frame_paramater
;; ----------------------------------------------------

;; Font
(set-frame-font "PlemolJP Console-18" t t)
(add-to-list 'default-frame-alist '(font . "PlemolJP Console-18"))

;; Transparency
(when (display-graphic-p)
  (set-frame-parameter nil 'alpha-background 92)
  (add-to-list 'default-frame-alist '(alpha-background . 92)))

;; ----------------------------------------------------
;; 3) Theme
;; ----------------------------------------------------

(add-to-list 'custom-theme-load-path
	     (expand-file-name "themes" user-emacs-directory))

(load-theme 'mytheme t)

;; ----------------------------------------------------
;; 4) Crosshair (row and column highlight)
;; ----------------------------------------------------

(require 'hl-line)

(global-hl-line-mode 1)

(require 'display-fill-column-indicator)
(display-fill-column-indicator-mode 1)

(defun my-column-highlight-update ()
  (setq-local display-fill-column-indicator-column (current-column)))

(add-hook 'post-command-hook #'my-column-highlight-update)

(custom-set-faces
 '(hl-line ((t (:background "#421a01")))))


;; ----------------------------------------------------
;; 5) Other UI Aids
;; ----------------------------------------------------

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(setq debug-on-error t)

(blink-cursor-mode 0)

