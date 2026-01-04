;;; =====================================
;;; org-mode
;;; =====================================

(leaf org
  :hook
  ((org-mode-hook . visual-line-mode))
  :custom
  (org-src-fontify-natively         . t)
  (org-src-tab-acts-natively        . t)
  (org-edit-src-content-indentation . 0)
  (org-src-preserve-indentation     . t)
  (org-src-window-setup             . 'split-window-below)

(provide 'config-org)
  
