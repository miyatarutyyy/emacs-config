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
       '("* %^{Title} :marginalia:%^{TopicTags}:"
	 "%U"
         ""
         ":PROPERTIES:"
         ":AUTHOR: %^{Author}"
         ":PUBLISHER: %^{Publisher}"
         ":YEAR: %^{Year}"
         ":STATUS: %^{Status|READING|FINISHED}"
         ":END:"
         ""
	 "- 価値 :: %^{Weight|important|reference|once}"
         "- 感想 :: %^{Impression}"
	 "- 疑問 :: %^{Question}"
	 "- 次回 :: %^{Next}"
	 ""
	 "- 位置 :: %^{Location}"
         "- 抜粋 :: %^{Excerpt}"
	 "- 感想 :: %^{ExcerptImpression}"
	 ""
	 "%?")
       "\n"))
	 

	 
(setq org-capture-templates
      `(("r" "Marginalia note" entry
         (file+headline ,my-org-reading-file "Inbox")
	 ,my-org-reading-template
         :empty-lines 1)
        ("i" "Inbox" entry
         (file+headline ,org-default-notes-file "Inbox")
         "* %?\n%U\n"
         :empty-lines 1)))

(provide 'config-org)
  
