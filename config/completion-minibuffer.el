;;; completion-minibuffer.el --- Mini buffer completion stack

;; Minibuffer UI: vertico + marginalia
;; Matching: orderless

(leaf vertico
  :ensure t
  :init
  (vertico-mode 1))

(leaf marginalia
  :ensure t
  :init
  (marginalia-mode 1))

(leaf orderless
  :ensure t
  :init
  ;; Keep 'basic' as a safety net for edge cases
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides
	'((file (styles . (partial-completion basic))))))

(provide 'completion-minibuffer)
;;; completion-minibuffer.el ends here
  
