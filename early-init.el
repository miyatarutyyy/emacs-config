;;; early-init.el --- Early init -*- lexical-binding: t; -*-

;; 1) Reduce startup UI noise
(setq inhibit-startup-screen t)

;; 2) Prevent frame resize flicker during startup (GUI)
(setq frame-inhibit-implied-resize t)

;; 3) If you plan to call (package-initialize) in init.el yourself,
;;    disable Emacs' automatic package initialization.
(setq package-enable-at-startup nil)

;; 4) (Optional) Speed up startup by raising GC threshold temporarily
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

(provide 'early-init)
;;; early-init.el ends here
