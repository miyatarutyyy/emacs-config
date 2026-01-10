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
  (org-src-window-setup             . 'split-window-below))

;; Org basic directory
(setq org-directory (expand-file-name "~/org/"))

;; Destination for the general inbox
(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))

;; Margnalia path
(setq my-org-marginalia-dir
      (expand-file-name "marginalia/" org-directory))
(setq my-org-reading-file
      (expand-file-name "reading-memo.org" my-org-marginalia-dir))

;; Capture templates
(setq my-org-reading-template
      (string-join
       '("#+title: %^{Title}"
	 "#+date: %U"
         ""
         ":PROPERTIES:"
         ":AUTHOR: "
         ":PUBLISHER: "
         ":YEAR: "
         ":STATUS: "
         ":END:"
         ""
	 "- 価値 :: "
         "- 感想 :: "
	 "- 疑問 :: "
	 "- 次回 :: "
	 ""
	 "- 位置 :: "
         "- 抜粋 :: "
	 "- 感想 :: "
	 ""
	 "%?")
       "\n"))

(defun my-org--slugify (s)
  (let* ((s (downcase s))
         (s (replace-regexp-in-string "[^[:alnum:][:space:]-]" "" s))
         (s (replace-regexp-in-string "[[:space:]]+" "-" s))
         (s (replace-regexp-in-string "-+" "-" s)))
    (string-trim s "-+" "-+")))

(defun my-org-capture-reading-path ()
  (unless (and (stringp my-org-marginalia-dir)
	       (not (string= my-org-marginalia-dir "")))
          (user-error "my-org-marginalia-dir is undefined"))
  (make-directory my-org-marginalia-dir t)
  (expand-file-name
   (format-time-string "%Y%m%d-%H%M%S.org")
   my-org-marginalia-dir))

(setq org-capture-window-setup 'current-window)

(setq org-capture-templates
      `(
	("r" "Marginalia note" plain
         (file ,(function my-org-capture-reading-path))
	 ,my-org-reading-template
	 :jump-to-captured t
         :empty-lines 1)
        ("i" "Inbox" entry
         (file+headline ,org-default-notes-file "Inbox")
         "* TODO %?\n%U\n"
         :empty-lines 1)))

(provide 'config-org)
  
