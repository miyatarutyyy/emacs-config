;;; ========================================================
;;; Undo-tree Configuration
;;; ========================================================

(leaf undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil)
  (add-hook 'magit-mode-hook
	    (lambda ()
	      (when (bound-and-true-p undo-tree-mode)
		(undo-tree-mode -1)))))
				      

(provide 'config-undo-tree)
