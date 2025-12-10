(deftheme mytheme "Simple theme: black background, orange text, red warnings.")

(let ((bg "#000000")
      (fg "#F66E25")
      (warn "#FF3333"))
  (custom-theme-set-faces
   'mytheme
   ;; basic
   `(default ((t (:background ,bg :foreground ,fg))))
   `(cursor ((t (:background ,fg))))
   `(region ((t (:background "#222222"))))
   `(fringe ((t (:background ,bg))))
   `(minibuffer-prompt ((t (:foreground ,fg :weight bold))))
   `(vertical-border ((t (:foreground "#222222"))))
   ;; mode line
   `(mode-line ((t (:background ,fg :foreground ,bg :bold t))))
   `(mode-line-inactive ((t (:background "#1a1a1a" :foreground "#444444"))))
   ;; warning
   `(warning ((t (:foreground ,warn :weight bold))))
   `(error ((t (:foreground ,warn :weight bold))))
   `(success ((t (:foreground "#44FF44" :weight bold))))))
(provide-theme 'mytheme)
