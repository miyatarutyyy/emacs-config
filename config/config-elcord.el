;;; ========================================================
;;; Discord Rich Presence — elcord Configuration
;;; ========================================================

(leaf elcord
  :ensure t
  :config
  (setq elcord-quiet nil
	elcord-editor-name "GNU/Emacs"
	elcord-use-major-mode-as-icon t
	elcord-idle-timer 600
	elcord-idle-message "Training my pinky to make it muscular")
  (add-hook 'after-init-hook #'elcord-mode))

(provide 'config-elcord)
