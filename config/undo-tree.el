;;; ========================================================
;;; Undo-tree Configuration
;;; ========================================================

(leaf undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil)
  (add-hook 'magit-mode-hook #'undo-tree-mode-off))

(provide 'config-undo-tree)
