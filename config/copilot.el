;;; =========================================================
;;; GitHub Copilot (copilot.el)
;;; =========================================================

(leaf copilot
  :ensure t
  :hook (prog-mode-hook . copilot-mode)
  :bind
  (:copilot-completion-map
   ("<tab>" . copilot-accept-completion)
   ("M-n"   . copilot-next-completion)
   ("M-p"   . copilot-previous-completion))
  :config
  (with-eval-after-load 'copilot
    (add-to-list 'copilot-major-mode-alist '(tsx-ts-mode . "typescriptreact"))
    (add-to-list 'copilot-major-mode-alist '(typescript-ts-mode . "typescriptreact"))))

(provide 'config-copilot)
