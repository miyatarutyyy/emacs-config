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
   ("M-p"   . copilot-previous-completion)))

(provide 'config-copilot)
