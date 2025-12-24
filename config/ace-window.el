(leaf ace-window
  :ensure t
  :bind (("C-x o" . ace-window))
  :config
  (setopt aw-keys '(?a ?s ?d ?f ?g ?h ?i ?j ?k ?l))
  :custom-face
  (aw-leading-char-face . '((t (:height 3.0)))))
