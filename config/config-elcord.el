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
	elcord-idle-message "Emacs is SHIT")
  (elcord-mode 1))

(provide 'config-elcord)
