;;; ========================================================
;;; pdf-tools configuration
;;; ========================================================

(leaf pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  :hook
  (pdf-view-mode-hook . (lambda ()
                          (display-line-numbers-mode -1))))

(provide 'config-pdf-tools)
