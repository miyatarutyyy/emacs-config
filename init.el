;;; ==========================================================
;;; 0) Bootstrap / Early Init
;;; ==========================================================

;; When byte-compiling or loading from a file, set user-emacs-directory here.
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))


;; Package archives & Leaf bootstrap
(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf blackout :ensure t)
    :config
    (leaf-keywords-init)))



;; Keep Customize output separate (and load it if exists)
(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el")))
  :config
  (unless (file-exists-p custom-file)
    (with-temp-buffer (write-file custom-file)))
  (load custom-file 'noerror))


;; Startup behavior (suppress splash & prevent resizing flicker)(
(setq inhibit-startup-screen t
      frame-inhibit-implied-resize t)

(require 'treesit)
;; always use python tree-sitter
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(setq treesit-font-lock-level 4)	     


;;; ==========================================================
;;; 2) Convenience Functions / Keybinds
;;; ==========================================================
(leaf treemacs
  :ensure t)


;;; =========================================================
;;; 3) Packages — Development Aids
;;; ---------------------------------------------------------
;;; Enhances editing, navigation, completion UI and LSP support
;;; =========================================================

;; Macro expansion helper
(leaf macrostep
  :ensure t)


;; Vertico: minibuffer completion UI
;; - Replaces the default *Completion* window with a vertical list
;;   shown directly in the minibuffer.
;; - Affects all minibuffer-based commands
;;   (M-x, find-file, switch-to-buffer, etc.)
;; - Only changes how candidates are displayed;
;;   it does not change the matching algorithm itself.
(leaf vertico
  :ensure t
  :init (vertico-mode 1))


;; Completion style: flexible matching
;; Orderless enables space-separated pattern matching
;; Partial completion for file paths only
(leaf orderless
  :ensure t
  :init
  (setq completion-styles'(orderless)
	completion-category-defaults nil
	completion-category-overrides
	'((file (styles . (partial-completion))))))


;; Adds annotations to minibuffer completion candidates
;; (e.g., file type, variable/function info)
(leaf marginalia
  :ensure t
  :init (marginalia-mode 1))


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
	     . ("typescript-language-server" "--stdio"))))


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
  (add-hook 'prog-mode-hook #'my/eglot-super-capf))



(leaf magit
  :ensure t
  :commands (magit-status))

;;; =========================================================
;;; 6) Common Lisp (SLY + SBCL)
;;; =========================================================

(leaf sly
  :ensure t
  :hook
  (lisp-mode-hook . sly-mode)
  :custom
  (inferior-lisp-program . "sbcl"))



;;; ========================================================
;;; 6.5) JOKE
;;; ========================================================


(leaf w3m
  :ensure t
  :commands (w3m w3m-browse-url)
  :config
  (setq w3m-display-inline-images t)
  (setq browse-url-browser-function 'w3m-browse-url))


;;; =========================================================
;;; 7) Optional Themes Package (kept if you use doom-themes elsewhere)
;;; =========================================================


;; Kept disabled (no direct dependency for transparency); enable if you use it.
;; (leaf doom-themes :ensure t)



;;; ========================================================
;;; 8) org-mode
;;; ========================================================

(leaf org
  :hook
  ((org-mode-hook . visual-line-mode))
  :custom
  (org-src-fontify-natively         . t)
  (org-src-tab-acts-natively        . t)
  (org-edit-src-content-indentation . 0)
  (org-src-preserve-indentation     . t)
  (org-src-window-setup             . 'split-window-below)
  :config
  (setq org-todo-keywords
	'((sequence "TODO(t)" "DOING(o)" "|" "DONE(d)" "CANCEL(c)")))
  (add-to-list 'org-src-lang-modes '("python" . python-ts)))
  
  ;; org-capture settings
  ;; Todo capture is now available
  (setq org-capture-templates
	'(("t" "Todo" entry
	   (file+headline "~/org/inbox.org" "Tasks")
	   "* TODO %?\n  %U\n")))
  
  ;; display png file on current buffer automatically
  (add-hook 'org-babel-after-execute-hook
	    'org-display-inline-images)
  (setq org-startup-with-inline-images t)

;; make UI modern 
(leaf org-modern
  :ensure t
  :hook (org-mode-hook . org-modern-mode)
  :custom
  (org-modern-hide-stars . t))



;; Personal Knowledge Management System
(leaf org-roam
  :ensure t
  :after org
  :preface
  ;; Org-roam prefix (C-c n)
  (define-prefix-command 'org-roam-prefix)
  :bind
  (("C-c n" . org-roam-prefix))
  :config
  (setq org-roam-directory (expand-file-name "~/org/roam/" org-directory))

  (define-key org-roam-prefix (kbd "f") #'org-roam-node-find)
  (define-key org-roam-prefix (kbd "i") #'org-roam-node-insert)
  (define-key org-roam-prefix (kbd "c") #'org-roam-capture)

  (define-prefix-command 'org-roam-dailies-prefix)
  (define-key org-roam-prefix (kbd "d") 'org-roam-dailies-prefix)
  (define-key org-roam-dailies-prefix (kbd "d") #'org-roam-dailies-goto-today)
  (define-key org-roam-dailies-prefix (kbd "t") #'org-roam-dailies-capture-today)

  (org-roam-db-autosync-mode 1))



;; Graphviz providing visualized graph structure
(leaf org-roam-graph
  :after org-roam
  :config
  (setq org-roam-graph-viewer nil
	org-roam-graph-executable "dot"
	org-roam-graph-filetype "svg"
	org-roam-graph-extra-config
	'(("overlap" . "false")
          ("splines" . "true")
          ("rankdir" . "LR")   
          ("nodesep" . "0.35")
          ("ranksep" . "0.6")
          ("dpi" . "150")))
  (setq org-roam-graph-node-extra-config
        '((shape . "box") (style . "rounded,filled")
          (fillcolor . "white") (color . "gray30")))
  (setq org-roam-graph-edge-extra-config
        '((color . "gray40") (penwidth . "1.1"))))



;;; =========================================================
;;; Reveal.js
;;; =========================================================

(leaf ox-reveal
  :ensure t
  :config
  (require 'ox-reveal))

(setq org-reveal-root "file:///home/trt-ryzen7/reveal.js")

;(setq org-reveal-highlight "zenburn")
;(setq org-src-fontify-natively t)
;(setq org-src-tab-acts-natively t)



;;; =========================================================
;;; Footer
;;; =========================================================

;;; =========================================================
;;; Load Custom Configs
;;; =========================================================
;;; In the future, make load setup like this
;;; (dolist (file '("ui" "mail"))
;;;   (load (expand-file-name (format "config/%s.el" file)
;;;                           user-emacs-directory)))
;;; ========================================================

(load (expand-file-name "config/ui.el" user-emacs-directory))
(load (expand-file-name "config/mail.el" user-emacs-directory))
(load (expand-file-name "config/elcord.el" user-emacs-directory))

(load (expand-file-name "config/vim-jp-radio.el" user-emacs-directory))
(load (expand-file-name "config/undo-tree.el" user-emacs-directory))
(load (expand-file-name "config/copilot.el" user-emacs-directory))
(load (expand-file-name "config/which-key.el" user-emacs-directory))
(load (expand-file-name "config/pdf-tools.el" user-emacs-directory))
(load (expand-file-name "config/ace-window.el" user-emacs-directory))
(load (expand-file-name "config/emacs-libvterm.el" user-emacs-directory))
(load (expand-file-name "config/keybind.el" user-emacs-directory))

(require 'config-copilot)
 
(provide 'init)
;;; init.el ends here.
