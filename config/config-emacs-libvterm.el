;;; ========================================================
;;; emacs-libvterm.el
;;; ========================================================
(add-to-list 'load-path
	     (expand-file-name "config/emacs-libvterm" user-emacs-directory))

(leaf vterm
  :commands (vterm vterm-other-window)
  :config
  (setq vterm-shell (executable-find "zsh")
	vterm-max-scrollback 10000))


(provide 'config-emacs-libvterm)
