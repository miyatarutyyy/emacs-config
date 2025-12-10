;;; fleury-theme.el --- The fleury color theme

;; Copyright (C) 2025 Shams Parvez Arka
;; See end of file for extended copyright information

;; Author    : Shams Parvez Arka <parvez6826@gmail.com>
;; URL       : https://github.com/ShamsParvezArka/fleury-theme.el
;; Version   : 0.5
;; Commentary: "Coming up with an original idea in 21st century
;;             is tough, even my dreams aren't original anymore!"


(deftheme fleury "The fleury color theme")

;; Color palette
(let ((rich-black         "#000000")
      (light-bronze       "#F66E25")
      (charcoal-gray      "#212121")
      (charcoal-gray-lite "#212121")
      (gunmetal-blue      "#212121")
      (dark-slate         "#212121")
      (amber-gold         "#f66e25")
      (medium-gray        "#212121")
      (jet-black          "#000000")
      (dim-gray           "#666666")
      (goldenrod          "#0000ff")
      (bright-orange      "#ffaa00")
      (dusty-rose         "#b51919")
      (sunflower-yellow   "#ffaa00")
      (burnt-orange       "#de451f")
      (sky-blue           "#0000ff")
      (sky-blue-lite      "#212121")
      (bright-red         "#ff0000")
      (fresh-green        "#00ff00")
      (lime-green         "#00ff00")      
      (vivid-vermilion    "#f0500c")
      (golden-yellow      "#f0500c")
      (pure-black         "#000000")
      (aqua-ice           "#0000ff")
      (dusty-sage         "#666666")
      (coffee-brown       "#212121")
      
      (mode-line-foreground-active "#000000")
      (mode-line-background-active "#F66E25")
      (mode-line-border            "#000000")
      )

  (custom-theme-set-faces
   'fleury

   ;; UI Elements
   `(default ((t (:background ,rich-black :foreground ,light-bronze))))
   `(cursor ((t (:background ,fresh-green))))
   `(region ((t (:background ,lime-green))))
   `(highlight ((t (:background ,charcoal-gray-lite))))
   `(fringe ((t (:background ,dark-slate))))
   `(vertical-border ((t (:foreground ,dark-slate))))
   `(minibuffer-prompt ((t (:foreground ,amber-gold :weight bold))))

   ;; Line Numbers
   `(line-number ((t (:foreground ,medium-gray :background ,rich-black))))
   `(line-number-current-line ((t (:background ,charcoal-gray-lite :foreground ,light-bronze))))

   ;; Font Lock Faces
   `(font-lock-comment-face ((t (:foreground ,dim-gray))))
   `(font-lock-keyword-face ((t (:foreground ,goldenrod))))
   `(font-lock-string-face ((t (:foreground ,bright-orange))))
   `(font-lock-constant-face ((t (:foreground ,bright-orange))))
   `(font-lock-builtin-face ((t (:foreground ,dusty-rose))))
   `(font-lock-preprocessor-face ((t (:foreground,dusty-rose))))
   `(font-lock-type-face ((t (:foreground ,sunflower-yellow))))
   `(font-lock-function-name-face ((t (:foreground ,burnt-orange))))
   `(font-lock-variable-name-face ((t (:foreground ,light-bronze))))
   `(font-lock-variable-use-face ((t (:foreground ,sky-blue))))   
   `(font-lock-preprocessor-face ((t (:foreground ,dusty-rose))))
   `(font-lock-warning-face ((t (:foreground ,bright-red :weight bold))))
   `(font-lock-doc-face ((t (:foreground ,fresh-green))))

   ;; Mode Line
   `(mode-line ((t (:background ,mode-line-background-active
                                :foreground ,mode-line-foreground-active
                                :box (:line-width 1 :color ,mode-line-border :style nil)))))
   `(mode-line-inactive ((t (:background ,rich-black
                                         :foreground ,mode-line-foreground-active
                                         :box (:line-width 1 :color ,mode-line-border :style nil)))))

   ;; Search
   `(isearch ((t (:background ,vivid-vermilion :foreground ,pure-black))))
   `(lazy-highlight ((t (:background ,golden-yellow :foreground ,pure-black))))

   ;; Custom Elements
   `(show-paren-match ((t (:background ,sky-blue-lite))))
   `(show-paren-mismatch ((t (:background ,dusty-sage))))

   ;; Tooltip and Popup
   `(tooltip ((t (:background ,coffee-brown :foreground ,amber-gold))))

   ;; Compilation
   `(flycheck-error ((t (:underline (:color ,bright-red :style wave)))))

   
   `(compilation-info ((t ,(list :foreground fresh-green
                                 :inherit 'unspecified))))
   `(compilation-warning ((t ,(list :foreground coffee-brown
                                    :bold t
                                    :inherit 'unspecified))))
   `(compilation-error ((t (:foreground ,bright-red))))
   `(compilation-mode-line-fail ((t ,(list :foreground bright-red
                                           :weight 'bold
                                           :inherit 'unspecified))))
   `(compilation-mode-line-exit ((t ,(list :foreground fresh-green
                                           :weight 'bold
                                           :inherit 'unspecified))))
   ))

(add-hook 'prog-mode-hook 'hl-line-mode)
(setq-default cursor-type 'box)
(defun custom/update-cursor-type ()
  (setq cursor-type
        (if (derived-mode-p 'prog-mode 'text-mode)
            '(bar . 2)  
          'box)))
(add-hook 'post-command-hook 'custom/update-cursor-type)

(provide-theme 'fleury)


;; MIT License

;; Copyright (c) 2025 Shams Parvez Arka

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
  
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
