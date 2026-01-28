;;; =========================================================
;;; GitHub Copilot (copilot.el)
;;; =========================================================

(leaf copilot
  :ensure t
  :commands (copilot-mode)
  :config
  (add-hook 'prog-mode-hook #'copilot-mode)
  (with-eval-after-load 'copilot
    (add-to-list 'copilot-major-mode-alist '(tsx-ts-mode . "typescriptreact"))
    (add-to-list 'copilot-major-mode-alist '(typescript-ts-mode . "typescriptreact"))))

(provide 'config-copilot)
