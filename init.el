;;; init.el --- Main configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal Emacs configuration for miyatarutyyy.

;;; Code:

;;; ==========================================================
;;; Basic Settings
;;; ==========================================================

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Disable backup files and else
(setq make-backup-files nil
      create-lockfiles nil)

;; Put auto-save files in ~/.emacs.d/auto-save-list/
(make-directory (locate-user-emacs-file "auto-save-list/") t)

(setq auto-save-default t
      auto-save-file-name-transforms
      `((".*" ,(locate-user-emacs-file "auto-save-list/") t)))

;; Disable ring bell alert sound
(setq ring-bell-function #'ignore)

;; Use space instead of tabs
(setq-default indent-tabs-mode nil
	          tab-width 4)

;; Save the cursor's previous position
(save-place-mode 1)

;; Save previous opening files
(recentf-mode 1)

;; Reload automatically when the file has been changed
(global-auto-revert-mode 1)

;; Ask Question through Emacs mini buffer
(setq use-dialog-box nil)

;; y-or-n instead of yes-or-no
(setq use-short-answers t)

;;; ==========================================================
;;; straight.el bootstrap
;;; ==========================================================

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

(require 'use-package)

;;; ==========================================================
;;; Org publishing
;;; ==========================================================

(use-package ox-hub
  :straight (:type git
                   :host github
                   :repo "miyatarutyyy/my-ox-hub"
                   :branch "master"
                   :files ("ox-hub.el"))
  :commands
  (ox-hub-new-article
   ox-hub-export-current-buffer
   ox-hub-export-current-buffer-to-zenn
   ox-hub-export-current-buffer-to-qiita))

;;; ==========================================================
;;; UI
;;; ==========================================================

;; Load my original theme
(add-to-list 'custom-theme-load-path
             (locate-user-emacs-file "theme/"))

(load-theme 'my-theme t)

(set-fringe-mode 10)

(global-display-line-numbers-mode 1)

;; Change line-number color depending on whether the buffer is editable.
(defvar-local my/editable-line-number-face-cookie nil
  "Face remap cookie for line numbers in editable buffers.")

(defvar-local my/editable-current-line-number-face-cookie nil
  "Face remap cookie for the current line number in editable buffers.")

(defun my/update-line-number-color-by-read-only ()
  "Use subdued line numbers in read-only buffers, vivid ones in editable buffers."
  ;; Remove previous temporary overrides.
  (when my/editable-line-number-face-cookie
    (face-remap-remove-relative my/editable-line-number-face-cookie)
    (setq my/editable-line-number-face-cookie nil))

  (when my/editable-current-line-number-face-cookie
    (face-remap-remove-relative my/editable-current-line-number-face-cookie)
    (setq my/editable-current-line-number-face-cookie nil))

  (if buffer-read-only
      (progn
        (setq my/editable-line-number-face-cookie
              (face-remap-add-relative
               'line-number
               '(:foreground "#3A3A3A")))

        (setq my/editable-current-line-number-face-cookie
              (face-remap-add-relative
               'line-number-current-line
               '(:foreground "#5A5A5A" :weight bold))))
    (setq my/editable-line-number-face-cookie
          (face-remap-add-relative
           'line-number
           '(:foreground "#6F7A1F")))

    (setq my/editable-current-line-number-face-cookie
          (face-remap-add-relative
           'line-number-current-line
           '(:foreground "#39FF14" :weight bold)))))

(add-hook 'read-only-mode-hook
          #'my/update-line-number-color-by-read-only)

(add-hook 'find-file-hook
          #'my/update-line-number-color-by-read-only)

(add-hook 'after-change-major-mode-hook
          #'my/update-line-number-color-by-read-only)

;; Do not visually wrap long lines
(setq-default truncate-lines t)

;; Disable line numbers in special buffers
(dolist (mode '(term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook
                help-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode 0))))

;; Keep the mode line compact and put Git state before the buffer name.
(require 'subr-x)

(defun my/mode-line-git-branch ()
  "Return the current Git branch for `mode-line-format'."
  (when-let* ((raw (and vc-mode
                        (string-trim
                         (if (stringp vc-mode)
                             vc-mode
                           (format-mode-line vc-mode)))))
              (_ (not (string-empty-p raw))))
    (when (string-match-p "\\`Git[:-]?" raw)
      (setq raw (string-trim (replace-regexp-in-string "\\`Git[:-]?" "" raw)))
      (unless (string-empty-p raw)
        (concat "Git: " raw)))))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                " | "
                (:eval
                 (when-let ((branch (my/mode-line-git-branch)))
                   (concat branch " | ")))
                mode-line-buffer-identification
                " | "
                mode-line-modes
                (:eval
                 (unless (string-empty-p
                          (string-trim (format-mode-line mode-line-misc-info)))
                   " | "))
                mode-line-misc-info
                mode-line-end-spaces))

;; Browser-like buffer tabs.
(use-package tab-line
  :straight nil
  :demand t
  :bind
  (("C-x C-f" . tab-line-switch-to-next-tab)
   ("C-x C-b" . tab-line-switch-to-prev-tab))
  :custom
  (tab-line-close-button-show t)
  (tab-line-new-button-show nil)
  (tab-line-switch-cycling t)
  (tab-line-tab-name-truncated-max 24)
  (tab-line-tabs-function #'my/tab-line-tabs-project-buffers)
  (tab-line-exclude-modes
   '(completion-list-mode
     help-mode
     treemacs-mode
     vterm-mode
     term-mode
     shell-mode
     eshell-mode
     magit-status-mode
     magit-diff-mode
     magit-log-mode))
  :init
  (require 'project)

  (defvar my/tab-line-hidden-buffer-name-regexp
    (rx string-start (or "*" " "))
    "Regexp for buffer names hidden from the tab line.")

  (defun my/tab-line-buffer-project-root (buffer)
    "Return BUFFER's project root, or nil when BUFFER is outside a project."
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((project (project-current nil)))
          (when project
            (expand-file-name (project-root project)))))))

  (defun my/tab-line-buffer-visible-p (buffer)
    "Return non-nil when BUFFER should be shown as a tab."
    (and (buffer-live-p buffer)
         (not (string-match-p my/tab-line-hidden-buffer-name-regexp
                              (buffer-name buffer)))
         (with-current-buffer buffer
           (and (not (minibufferp))
                (not (memq major-mode tab-line-exclude-modes))
                (not tab-line-exclude)
                (or buffer-file-name
                    (derived-mode-p 'prog-mode 'text-mode))))))

  (defun my/tab-line-tabs-project-buffers ()
    "Return visible buffers from the current project for `tab-line'."
    (if (not (my/tab-line-buffer-visible-p (current-buffer)))
        nil
      (let* ((current-root (my/tab-line-buffer-project-root (current-buffer)))
             (buffers
              (seq-filter
               (lambda (buffer)
                 (and (my/tab-line-buffer-visible-p buffer)
                      (if current-root
                          (equal (my/tab-line-buffer-project-root buffer)
                                 current-root)
                        (not (my/tab-line-buffer-project-root buffer)))))
               (tab-line-tabs-buffer-list))))
        (if (memq (current-buffer) buffers)
            buffers
          (cons (current-buffer) buffers)))))
  :config
  (global-tab-line-mode 1))

;; Smooth scrolling (keyboard, mouse, trackpad).
(setq scroll-conservatively 3
      scroll-margin 0
      scroll-step 1
      scroll-preserve-screen-position t
      pixel-scroll-precision-interpolate-page t)
(pixel-scroll-precision-mode 1)

(use-package ultra-scroll
  :demand t
  :config
  (ultra-scroll-mode 1))

(use-package scroll-on-jump
  :demand t
  :custom
  (scroll-on-jump-duration 0.12)
  (scroll-on-jump-smooth t)
  :config
  (dolist (command '(next-line
                     previous-line
                     recenter-top-bottom
                     beginning-of-buffer
                     end-of-buffer))
    (advice-add command :around #'scroll-on-jump-advice--wrapper))
  (dolist (command '(scroll-up-command
                     scroll-down-command))
    (advice-add command :around #'scroll-on-jump-advice--with-scroll-wrapper)))

;; Set the default font size
(set-face-attribute 'default nil :height 100)

;;; ==========================================================
;;; Keybindings
;;; ==========================================================

;; Use C-h as Backspace
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; Move Help from C-h to C-c h
(define-key global-map (kbd "C-c h") help-map)

;;; ===========================================================================
;;; Completion minibuffer: vertico / orderless / marginalia / consult / embark
;;; ===========================================================================

(use-package vertico
  :init
  (vertico-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package consult
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("M-y" . consult-yank-pop)
   ("M-g g" . consult-goto-line)
   ("M-g i" . consult-imenu)
   ("M-g r" . consult-ripgrep)))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-c e b" . embark-bindings)))

(use-package embark-consult
  :after (embark consult))

;;; ==========================================================
;;; In-buffer completion: corfu / cape
;;; ==========================================================

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preview-current nil)
  (corfu-on-exact-match nil)
  :bind
  (:map corfu-map
        ("TAB" . corfu-complete)
        ([tab] . corfu-complete)
        ("RET" . nil))
  :init
  (global-corfu-mode 1))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file t))

;;; ==========================================================
;;; Project
;;; ==========================================================

(use-package project
  :straight nil
  :bind
  (("C-c p f" . project-find-file)
   ("C-c p b" . project-switch-to-buffer)
   ("C-c p p" . project-switch-project)
   ("C-c p g" . project-find-regexp)
   ("C-c p d" . project-dired)))

;;; ==========================================================
;;; Programming support: eglot / flymake / treesit / apheleia
;;; ==========================================================

;; ----------------------------------------------------------
;; LSP client: Eglot
;; ----------------------------------------------------------
;; Eglot is built into Emacs 29+.
;; Language servers themselves must be installed separately.
(use-package eglot
  :straight nil
  :hook
  ((html-ts-mode . eglot-ensure)
   (css-ts-mode . eglot-ensure)
   (js-ts-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)
   (tsx-ts-mode . eglot-ensure)
   (python-mode . eglot-ensure)
   (python-ts-mode . eglot-ensure)
   (c-ts-mode . eglot-ensure)
   (java-ts-mode . eglot-ensure)
   (json-ts-mode . eglot-ensure)
   (yaml-ts-mode . eglot-ensure)
   (yaml-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t))

;; ----------------------------------------------------------
;; Diagnostics
;; ----------------------------------------------------------
(use-package flymake
  :straight nil
  :hook
  (prog-mode . flymake-mode)
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error)))

;; ----------------------------------------------------------
;; Formatter runner
;; ----------------------------------------------------------
(use-package apheleia
  :config
  (apheleia-global-mode 1))

;; YAML support.
;; yaml-ts-mode is used when available.
;; yaml-mode is installed as a fallback.
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; ----------------------------------------------------------
;; Tree-sitter major mode remapping
;; ----------------------------------------------------------
;; Emacs 29+ has many built-in tree-sitter major modes.
;; This helper only adds remaps when the target mode exists,
;; so the config does not break on older Emacs builds.

(defun my/remap-major-mode-if-available (from-mode to-mode)
  "Remap FROM-MODE to TO-MODE when TO-MODE is available."
  (when (fboundp to-mode)
    (add-to-list 'major-mode-remap-alist
                 (cons from-mode to-mode))))

;; ----------------------------------------------------------
;; Tree-sitter grammars
;; ----------------------------------------------------------
(use-package treesit
  :straight nil
  :init
  ;; Avoid `customize-set-variable' here: reapplying this via `eval-buffer'
  ;; refontifies existing tree-sitter buffers, and python-ts-mode currently
  ;; trips a query incompatibility in Emacs 30.2 + libtree-sitter 0.26.
  (setq treesit-font-lock-level 4)
  :config
  (setq treesit-language-source-alist
        '((html       "https://github.com/tree-sitter/tree-sitter-html")
          (css        "https://github.com/tree-sitter/tree-sitter-css")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (python     "https://github.com/tree-sitter/tree-sitter-python")
          (c          "https://github.com/tree-sitter/tree-sitter-c")
          (cpp        "https://github.com/tree-sitter/tree-sitter-cpp")
          (java       "https://github.com/tree-sitter/tree-sitter-java")
          (json       "https://github.com/tree-sitter/tree-sitter-json")
          (yaml       "https://github.com/ikatyang/tree-sitter-yaml"))))

(my/remap-major-mode-if-available 'html-mode       'html-ts-mode)
(my/remap-major-mode-if-available 'css-mode        'css-ts-mode)
(my/remap-major-mode-if-available 'js-mode         'js-ts-mode)
(my/remap-major-mode-if-available 'javascript-mode 'js-ts-mode)
(my/remap-major-mode-if-available 'typescript-mode 'typescript-ts-mode)
;; Remove any python-ts-mode remap left behind by earlier `eval-buffer' runs.
;; The built-in python-ts-mode queries are currently incompatible with this
;; Emacs/libtree-sitter combination.
(setq major-mode-remap-alist
      (assq-delete-all 'python-mode major-mode-remap-alist))
(my/remap-major-mode-if-available 'c-mode          'c-ts-mode)
(my/remap-major-mode-if-available 'java-mode       'java-ts-mode)
(my/remap-major-mode-if-available 'json-mode       'json-ts-mode)
(my/remap-major-mode-if-available 'yaml-mode       'yaml-ts-mode)

;; File associations for modes that are not always covered by default.
(add-to-list 'auto-mode-alist '("\\.ts\\'"    . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'"   . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'"   . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

;; Extra highlighting for JavaScript/TypeScript property accesses that are not
;; covered by the compatible tree-sitter feature set below.
(defconst my/typescript-extra-font-lock-keywords
  '(("\\.\\s-*\\([[:alpha:]_$][[:alnum:]_$]*\\)" 1 'font-lock-property-use-face)
    ("\\_<[0-9]+\\(?:\\.[0-9]+\\)?\\_>" 0 'font-lock-number-face)))

;; Work around Emacs 30.2 + libtree-sitter 0.26 query incompatibility.
;; Keep this scoped to TypeScript/TSX so other tree-sitter modes can use
;; the global `treesit-font-lock-level' setting.  The bundled TypeScript
;; `constant' and `number' queries are rejected by this libtree-sitter
;; version, but the remaining level-4 features are useful and stable.
(defun my/typescript-ts-font-lock-workaround ()
  "Use full TypeScript/TSX font-lock except incompatible query features."
  (setq-local treesit-font-lock-level 4)
  (setq-local treesit-font-lock-feature-list
              (mapcar
               (lambda (features)
                 (let ((filtered (copy-sequence features)))
                   (dolist (feature '(constant number) filtered)
                     (setq filtered (delq feature filtered)))))
               treesit-font-lock-feature-list))
  (treesit-font-lock-recompute-features)
  (font-lock-add-keywords nil my/typescript-extra-font-lock-keywords 'append)
  (font-lock-flush))

(add-hook 'typescript-ts-mode-hook #'my/typescript-ts-font-lock-workaround)
(add-hook 'tsx-ts-mode-hook #'my/typescript-ts-font-lock-workaround)

;;; ==========================================================
;;; Git
;;; ==========================================================

(use-package magit
  :bind
  (("C-x g" . magit-status)))

(use-package diff-hl
  :hook
  ((prog-mode . diff-hl-mode)
   (text-mode . diff-hl-mode)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode 1))

;;; ==========================================================
;;; File tree
;;; ==========================================================

(use-package treemacs
  :bind
  (("C-c t t" . treemacs)))

(use-package treemacs-magit
  :after (treemacs magit))

;;; ==========================================================
;;; Go to my init.el
;;; ==========================================================

(defun my/open-init-el ()
  "Open the current Emacs init file."
  (interactive)
  (find-file user-init-file))

;;; ==========================================================
;;; Clipboard
;;; ==========================================================

(setq select-enable-clipboard t
      save-interprogram-paste-before-kill t)

(defun my/wayland-clipboard-available-p ()
  "Return non-nil when Wayland clipboard commands are available."
  (and (getenv "WAYLAND_DISPLAY")
       (getenv "XDG_RUNTIME_DIR")
       (executable-find "wl-copy")
       (executable-find "wl-paste")))

(defun my/wayland-clipboard-copy (text)
  "Copy TEXT to the Wayland clipboard using wl-copy."
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max) "wl-copy" nil nil nil)))

(defun my/wayland-clipboard-paste ()
  "Return text from the Wayland clipboard using wl-paste."
  (with-timeout (1 nil)
    (with-temp-buffer
      (when (zerop (call-process "wl-paste" nil t nil "--no-newline"))
        (let ((text (buffer-string)))
          (unless (string-empty-p text)
            text))))))

(when (my/wayland-clipboard-available-p)
  (setq interprogram-cut-function #'my/wayland-clipboard-copy
        interprogram-paste-function #'my/wayland-clipboard-paste))

;;; ==========================================================
;;; Terminal
;;; ==========================================================

(use-package vterm
  :commands
  (vterm
   vterm-other-window)
  :bind
  (("C-c w c" . my/codex-workspace-layout))
  :custom
  (vterm-always-compile-module t)
  (vterm-kill-buffer-on-exit t)
  :config
  (require 'project)
  (require 'seq)

  (with-eval-after-load 'vterm
    ;; paste
    (define-key vterm-mode-map (kbd "C-y") #'vterm-yank)
    (define-key vterm-mode-map (kbd "M-y") #'vterm-yank-pop)

    ;; copy mode
    (define-key vterm-mode-map (kbd "C-c C-y") #'vterm-copy-mode)
    (define-key vterm-mode-map (kbd "C-c C-c") #'vterm-send-C-c)

    ;; Emacs-like keys inside vterm-copy-mode
    (define-key vterm-copy-mode-map (kbd "M-w") #'kill-ring-save)
    (define-key vterm-copy-mode-map (kbd "C-w") #'kill-region)
    (define-key vterm-copy-mode-map (kbd "C-y") #'yank)
    (define-key vterm-copy-mode-map (kbd "q") #'vterm-copy-mode))

  (defun my/vterm-switch (buffer-name directory)
    "Switch to vterm BUFFER-NAME, creating it in DIRECTORY when needed."
    (let ((default-directory directory)
          (vterm-buffer-name buffer-name))
      (vterm)))

  (defun my/buffer-project-directory (buffer)
    "Return BUFFER's project root or `default-directory'."
    (with-current-buffer buffer
      (if-let ((project (project-current nil)))
          (expand-file-name (project-root project))
        default-directory)))

  (defun my/workspace-edit-buffer ()
    "Return the buffer that should stay in the editor window."
    (if (not (derived-mode-p 'vterm-mode))
        (current-buffer)
      (or (seq-find #'my/tab-line-buffer-visible-p (buffer-list))
          (current-buffer))))

  (defun my/codex-workspace-layout ()
    "Arrange a Codex vterm, a CLI vterm, and the current edit buffer."
    (interactive)
    (let* ((edit-buffer (my/workspace-edit-buffer))
           (directory (my/buffer-project-directory edit-buffer))
           (left-top (selected-window))
           (right (progn
                    (delete-other-windows)
                    (split-window left-top nil 'right)))
           (cli-height (max 10 (floor (/ (window-total-height left-top) 3))))
           (left-bottom (split-window left-top (- cli-height) 'below)))
      (select-window left-top)
      (my/vterm-switch "*vterm-codex*" directory)
      (select-window left-bottom)
      (my/vterm-switch "*vterm-cli*" directory)
      (set-window-buffer right edit-buffer)
      (select-window right))))

;;; ==========================================================
;;; AI
;;; ==========================================================

(require 'auth-source)

(defun my/set-iniad-openai-api-key-from-auth-source ()
  "Set INIAD_OPENAI_API_KEY from auth-source.

Expected entry in ~/.authinfo.gpg:

  machine iniad-openai login apikey password YOUR_API_KEY
"
  (let* ((found (car (auth-source-search
                      :host "iniad-openai"
                      :user "apikey"
                      :require '(:secret))))
         (secret (plist-get found :secret)))
    (when secret
      (setenv "INIAD_OPENAI_API_KEY"
              (if (functionp secret)
                  (funcall secret)
                secret)))))

(condition-case err
    (my/set-iniad-openai-api-key-from-auth-source)
  (error
   (message "Could not load INIAD_OPENAI_API_KEY from auth-source: %s"
            (error-message-string err))))

(use-package gptel
  :ensure t
  :config
  (defun my/gptel-iniad-api-key ()
    (or (getenv "INIAD_OPENAI_API_KEY")
        (user-error "INIAD_OPENAI_API_KEY is not set")))

  ;; INIAD OpenAI-compatible API
  ;; GPT-5.4 / o4-mini などはこちら
  (defvar my/gptel-iniad-openai-backend
    (gptel-make-openai "INIAD OpenAI"
      :host "api.openai.iniad.org"
      :endpoint "/api/v1/chat/completions"
      :protocol "https"
      :key #'my/gptel-iniad-api-key
      :stream t
      :models '("o4-mini"
                "gpt-5.4"
                "gpt-5.4-mini"
                "gpt-5.4-nano")))

  ;; INIAD Anthropic-compatible API
  ;; Claude Sonnet 4.6 はこちら
  (defvar my/gptel-iniad-anthropic-backend
    (gptel-make-anthropic "INIAD Anthropic"
      :host "api.anthropic.iniad.org"
      :endpoint "/api/v1/messages"
      :protocol "https"
      :key #'my/gptel-iniad-api-key
      :stream t
      :models '("claude-sonnet-4-6")))

  ;; 既定は軽めのモデルにする
  (setq gptel-backend my/gptel-iniad-openai-backend
        gptel-model "o4-mini"))

;;; ==========================================================
;;; EIN Emacs IPython Notebook
;;; ==========================================================

(use-package ein
  :commands
  (ein:notebooklist-open
   ein:notebook-open
   ein:login)
  :mode
  ("\\.ipynb\\'" . ein:notebook-mode)
  :config
  (setq ein:output-area-inlined-images t))

;;; ==========================================================
;;; Kaggle / Jupyter / Org-Babel
;;; ==========================================================

(use-package org
  :straight nil
  :custom
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t))))

(defconst my/kaggle-project-root "/home/trt-ryzen7/kaggle-project/"
  "Default Kaggle project directory.")

(defconst my/kaggle-venv-bin
  (expand-file-name ".venv/bin" my/kaggle-project-root)
  "Virtualenv bin directory for Kaggle/Jupyter client tools.")

(defun my/prepend-to-path (directory)
  "Prepend DIRECTORY to `exec-path' and PATH when it exists."
  (when (file-directory-p directory)
    (add-to-list 'exec-path directory)
    (setenv "PATH" (concat directory path-separator (getenv "PATH")))))

;; Emacs daemon / systemd から jupyter と kaggle を見つけられるようにする
(my/prepend-to-path my/kaggle-venv-bin)

(use-package jupyter
  :ensure t
  :after org
  :demand t
  :commands
  (jupyter-connect-repl
   jupyter-run-repl
   jupyter-server-list-kernels)
  :config
  (require 'ob-jupyter)

  (setq jupyter-api-authentication-method 'token)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (jupyter . t)))

  ;; `jupyter-python' is an alias generated from kernelspecs, not a separate
  ;; executor.  Create the aliases during daemon startup as well as in Org
  ;; buffers so `M-: (fboundp 'org-babel-execute:jupyter-python)' is meaningful.
  (with-demoted-errors "Error creating Jupyter Org aliases: %S"
    (org-babel-jupyter-aliases-from-kernelspecs t))

  (with-eval-after-load 'jupyter-repl
    (defun my/jupyter-repl-sync-execution-state-safely (orig &rest args)
      "Demote non-critical Jupyter REPL prompt state sync errors."
      (with-demoted-errors "Error syncing Jupyter REPL state: %S"
        (apply orig args)))

    (advice-add 'jupyter-repl-sync-execution-state
                :around #'my/jupyter-repl-sync-execution-state-safely))

  (setq org-babel-default-header-args:jupyter-python
        '((:session . "kaggle")
          (:kernel . "python3")
          (:async . "yes")
          (:results . "both")
          (:exports . "both"))))

(defun my/kaggle-project-directory ()
  "Return a directory suitable for running Kaggle CLI commands."
  (or (locate-dominating-file default-directory "kernel-metadata.json")
      (locate-dominating-file default-directory "pyproject.toml")
      (and (file-directory-p my/kaggle-project-root)
           my/kaggle-project-root)
      default-directory))

(defun my/kaggle-command-string (args)
  "Return a shell command string for running kaggle with ARGS."
  (let ((kaggle (executable-find "kaggle")))
    (unless kaggle
      (user-error "kaggle command was not found in exec-path"))
    (string-join (mapcar #'shell-quote-argument (cons kaggle args)) " ")))

(defun my/kaggle-compile (args &optional confirm-p)
  "Run Kaggle CLI with ARGS via `compile'.
When CONFIRM-P is non-nil, ask before running the command."
  (let ((command (my/kaggle-command-string args))
        (default-directory (my/kaggle-project-directory)))
    (when (or (not confirm-p)
              (yes-or-no-p (format "Run `%s' in %s? " command default-directory)))
      (compile command))))

(defun my/kaggle-version ()
  "Show Kaggle CLI version."
  (interactive)
  (my/kaggle-compile '("--version")))

(defun my/kaggle-competitions-list ()
  "List Kaggle competitions."
  (interactive)
  (my/kaggle-compile '("competitions" "list")))

(defun my/kaggle-kernels-list-mine ()
  "List the current user's Kaggle kernels."
  (interactive)
  (my/kaggle-compile '("kernels" "list" "--mine")))

(defun my/kaggle-kernels-status (kernel)
  "Show status for Kaggle KERNEL, e.g. user/kernel-slug."
  (interactive "sKernel slug (user/kernel): ")
  (my/kaggle-compile (list "kernels" "status" kernel)))

(defun my/kaggle-kernels-pull (kernel directory)
  "Pull Kaggle KERNEL into DIRECTORY."
  (interactive
   (list (read-string "Kernel slug (user/kernel): ")
         (read-directory-name "Pull into directory: " (my/kaggle-project-directory))))
  (my/kaggle-compile (list "kernels" "pull" kernel "-p" directory) t))

(defun my/kaggle-kernels-push (directory)
  "Push a Kaggle kernel from DIRECTORY."
  (interactive
   (list (read-directory-name "Kernel directory: " (my/kaggle-project-directory))))
  (my/kaggle-compile (list "kernels" "push" "-p" directory) t))

(defun my/kaggle-kernels-output (kernel directory)
  "Download output for Kaggle KERNEL into DIRECTORY."
  (interactive
   (list (read-string "Kernel slug (user/kernel): ")
         (read-directory-name "Output directory: " (my/kaggle-project-directory))))
  (my/kaggle-compile (list "kernels" "output" kernel "-p" directory) t))

(defun my/kaggle-competitions-submit (competition file message)
  "Submit FILE to COMPETITION with MESSAGE."
  (interactive
   (list (read-string "Competition: ")
         (read-file-name "Submission file: " (my/kaggle-project-directory) nil t)
         (read-string "Submission message: ")))
  (my/kaggle-compile
   (list "competitions" "submit" "-c" competition "-f" file "-m" message)
   t))

(defun my/kaggle-jupyter-list-kernels (&optional reset-server)
  "List live kernels on a Kaggle Jupyter Server.
With prefix RESET-SERVER, prompt for the server URL again."
  (interactive "P")
  (require 'jupyter-server)
  (let ((jupyter-api-authentication-method 'token)
        (current-prefix-arg reset-server))
    (call-interactively #'jupyter-server-list-kernels)))

(defun my/kaggle-jupyter-session-name (url name)
  "Copy an Org :session value for Jupyter server URL and kernel NAME.
The token is entered interactively by emacs-jupyter and is not stored here."
  (interactive "sJupyter Server URL: \nsSession name: ")
  (require 'jupyter-tramp)
  (let* ((root (jupyter-tramp-file-name-from-url url))
         (session (concat (file-remote-p root) "/" name)))
    (kill-new session)
    (message "Copied Org :session value: %s" session)))

;;; ==========================================================
;;; Session restore
;;; ==========================================================

(use-package desktop
  :straight nil
  :demand t
  :init
  (setq desktop-dirname user-emacs-directory
        desktop-path (list user-emacs-directory)
        desktop-save t
        desktop-load-locked-desktop 'check-pid
        desktop-auto-save-timeout 30
        desktop-restore-eager 5)
  :config
  (desktop-save-mode 1))

;;; ==========================================================
;;; Emacs client frames
;;; ==========================================================

(use-package server
  :straight nil
  :demand t
  :config
  (defun my/server-switch-to-restored-buffer ()
    "Show the most recent restored normal buffer in a new client frame."
    (when (member (buffer-name) '("*scratch*" "*GNU Emacs*"))
      (when-let ((buffer (seq-find #'my/tab-line-buffer-visible-p
                                   (buffer-list))))
        (switch-to-buffer buffer))))

  (add-hook 'server-after-make-frame-hook
            #'my/server-switch-to-restored-buffer))

;;; ==========================================================
;;; Restart Emacs daemon
;;; ==========================================================

(defun my/restart-emacs-daemon ()
  "Restart the systemd-managed Emacs daemon, then open a new frame."
  (interactive)
  (when (yes-or-no-p "Restart Emacs daemon? ")
    (save-some-buffers t)
    (call-process
     "sh" nil 0 nil
     "-lc"
     "systemd-run --user --collect --unit=restart-emacs-from-emacs sh -lc 'systemctl --user restart emacs.service; sleep 0.5; emacsclient -c'")))

(global-set-key (kbd "C-c r e") #'my/restart-emacs-daemon)

(provide 'init)
;;; init.el ends here
