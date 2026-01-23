;;; ========================================================
;;; emacs-libvterm.el
;;; ========================================================
(add-to-list 'load-path
	     (expand-file-name "~/.emacs.d/config/emacs-libvterm"))

(leaf vterm
  :require t
  :config
  (setq vterm-shell (executable-find "zsh")
	vterm-max-scrollback 10000))


(provide 'config-emacs-libvterm)
