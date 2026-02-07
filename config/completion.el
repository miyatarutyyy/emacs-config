;;; completion.el --- In-buffer completion stack

(leaf corfu
  :ensure t
  :custom
  ((corfu-auto . t)
   (corfu-auto-prefix . 1)
   (corfu-auto-delay . 0.10)
   (corfu-preselect . 'prompt))
  :bind (:corfu-map
         ("C-n" . corfu-next)
         ("C-p" . corfu-previous)
         ("M-n" . corfu-next)
         ("M-p" . corfu-previous))
  :init
  (global-corfu-mode 1)
  :config
  ;; enable popupinfo (bundled with corfu)
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode 1))

(leaf cape
  :ensure t
  :require t
  :config
  (defun my/capf-append (capf)
    "Append CAPF to `completion-at-point-functions` locally."
    (setq-local completion-at-point-functions
                (append completion-at-point-functions (list capf))))

  (defun my/capf-prepend (capf)
    "Prepend CAPF to `completion-at-point-functions` locally, removing duplicates."
    (setq-local completion-at-point-functions
                (cons capf (remove capf completion-at-point-functions))))

  (defun my/setup-prog-capf ()
    "Completion sources for programming buffers (non-destructive)."
    (my/capf-append #'cape-file)
    (my/capf-append #'cape-dabbrev))

  (defun my/setup-eglot-capf ()
    "Prefer Eglot CAPF when Eglot manages the buffer (non-destructive)."
    (when (bound-and-true-p eglot--managed-mode)
      (my/capf-prepend #'eglot-completion-at-point)))

  (add-hook 'prog-mode-hook #'my/setup-prog-capf)
  (add-hook 'prog-mode-hook #'my/setup-eglot-capf)

  (add-hook 'astro-ts-mode-hook #'my/setup-prog-capf)
  (add-hook 'astro-ts-mode-hook #'my/setup-eglot-capf))

(provide 'completion)
;;; completion.el ends here
