;;; ==========================================================
;;; 0) Bootstrap / Early Init
;;; ==========================================================

;; When byte-compiling or loading from a file, set user-emacs-directory here.
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

;; Prioritize UTF-8 (Unix) and fix the default for new saves to UTF-8 (Unix)
(prefer-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)

;;; =============================================================
;;; Package archives & Leaf bootstrap
;;; -------------------------------------------------------------
;;; - set GNU ELPA and MELPA as a Package Distributor
;;; - install leaf and leaf-keywords when emacs dosen't have them
;;; - enable leaf and allow to be written on leaf style
;;; - enable hydra and blackout
;;; =============================================================
;; Package archives
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(require 'package)
(package-initialize)

;; Using &rest allows me to pass as many arguments as I like.
(defun my/ensure-packages (&rest pkgs)
  "Ensure PKGS are installed. Refresh content at most once."
  (let ((refreshed nil))
    (dolist (p pkgs)
      (unless (package-installed-p p)
	(unless refreshed
	  (package-refresh-contents)
	  (setq refreshed t))
	(condition-case err
	    (package-install p)
	  (error
	   (message "[init] Failed to install %s: %s"
		    p (error-message-string err))
	   (signal (car err) (cdr err))))))))

;; Bootstrap leaf stack (runtime only)
(my/ensure-packages 'leaf 'leaf-keywords 'hydra 'blackout)

(require 'leaf)
(require 'leaf-keywords)
(leaf-keywords-init)
(leaf hydra)
(leaf blackout)



;; Keep Customize output separate (and load it if exists)
(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el")))
  :config
  (unless (file-exists-p custom-file)
    (with-temp-buffer (write-file custom-file)))
  (load custom-file 'noerror))

(require 'treesit)
;; always use python tree-sitter
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(setq treesit-font-lock-level 4)	     


;;; ==========================================================
;;; 2) Convenience Functions / Keybind
;;; ==========================================================
(leaf treemacs
  :ensure t)

;; Delete selection (delsel): replace active region by typing
(leaf delsel
  :tag "builtin"
  :init
  (delete-selection-mode 1))

;;; =========================================================
;;; 3) Packages — Development Aids
;;; ---------------------------------------------------------
;;; Enhances editing, navigation, completion UI and LSP support
;;; =========================================================

;; Macro expansion helper
(leaf macrostep
  :ensure t)


;; Popup completion UI (CAPF front-end)
;; Integrates seamlessly with Eglot, Cape, dabbrev, etc.
;; Keys: C-n / C-p navigate, TAB to confirm
(leaf corfu
  :ensure t
  :custom ((corfu-auto . t)
	   (corfu-auto-prefix . 1)
	   (corfu-auto-delay . 0.02)
	   (corfu-quit-no-match . t)
	   (corfu-preselect . 'prompt))
  :bind (:corfu-map
         ("C-n" . corfu-next)
	 ("C-p" . corfu-previous)
         ("M-n" . corfu-next)
         ("M-p" . corfu-previous))
  :init
  (global-corfu-mode 1))


;; Show documentation and signature for completion candidates
(leaf corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode))


;; Built-in LSP client
;; Automatically starts LSP servers for supported languages
;; Examples: M-. go to definition, M-, go back, eglot-rename
(leaf eglot
  :tag "builtin"
  :hook ((python-ts-mode     . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode        . eglot-ensure)
         (js-ts-mode         . eglot-ensure)
         (astro-ts-mode      . eglot-ensure))
  :custom
  (eglot-autoshutdown . t)
  :config
  (add-to-list 'eglot-server-programs
               '((js-ts-mode typescript-ts-mode tsx-ts-mode)
                 . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(astro-ts-mode . ("astro-ls" "--stdio"))))


(leaf web-mode
  :ensure t
  :mode ("\\.html\\'" "\\.css\\'")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-indentation nil
        indent-tabs-mode nil))

;; Astro (Tree-sitter)
(add-to-list 'treesit-language-source-alist
             '(astro "https://github.com/virchau13/tree-sitter-astro"))
(add-to-list 'auto-mode-alist '("\\.astro\\'" . astro-ts-mode))


;; Enable Tree-sitter based TypeScript mode
(leaf typescript-ts-mode
  :ensure nil ;; built-in
  :config
  (add-to-list 'auto-mode-alist '("\\.ts\\'"  . typescript-ts-mode)))
(leaf tsx-ts-mode
  :ensure nil ;; built-in
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))



;; Extra completion sources to supplement LSP
;; - dabbrev: buffer-based word completion
;; - file: file path completion
(leaf cape
  :ensure t
  :require t
  :config
  (defun my/eglot-super-capf ()
    "Use Eglot CAPF only in managed buffers."
    (setq-local
     completion-at-point-functions
     (if (bound-and-true-p eglot--managed-mode)
         '(eglot-completion-at-point
           cape-file
           cape-dabbrev)
       '(cape-file
         cape-dabbrev))))
  (add-hook 'prog-mode-hook #'my/eglot-super-capf)
  (add-hook 'astro-ts-mode-hook #'my/eglot-super-capf))



(leaf magit
  :ensure t
  :commands (magit-status))

;;; =========================================================
;;; 6) Common Lisp (SLY + SBCL)
;;; =========================================================

(leaf sly
  :ensure t
  :hook
  (lisp-mode . sly-mode)
  :custom
  (inferior-lisp-program . "sbcl"))


;;; =========================================================
;;; 7) Optional Themes Package (kept if you use doom-themes elsewhere)
;;; =========================================================

;; Kept disabled (no direct dependency for transparency); enable if you use it.
;; (leaf doom-themes :ensure t)

;;; =========================================================
;;; Footer
;;; =========================================================

(define-key global-map (kbd "C-c /") 'comment-or-uncomment-region)

;;; =========================================================
;;; Config loader (safe, explicit order)
;;; =========================================================

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(defun my/require-config (feature)
  "Require FEATURE safely; log failures without aborting init."
  (condition-case err
      (require feature)
    (error
     (message "[init] Failed to load %S: %s" feature (error-message-string err)))))

;; Load order matters: keep it explicit.
(dolist (feat '(completion-minibuffer
		completion
		config-ui
		config-mail
		config-elcord
		config-vim-jp-radio
                config-undo-tree
                config-copilot
                config-which-key
                config-ace-window
                config-emacs-libvterm
                config-keybind
                config-org
		config-browse
		config-wiki-rencontre))
  (my/require-config feat))

(provide 'init)
;;; init.el ends here.
