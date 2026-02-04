;;; config-wiki-rencontre.el --- Random Wikipedia encounter

(defgroup wiki-rencontre nil
  "Encounter a random Wikipedia page."
  :group 'applications)

(defcustom wiki-rencontre-language "ja"
  "wikipedia language code (e.g., \"ja\", \"en\")."
  :type 'string
  :group 'wiki-rencontre)

(defun wiki-rencontre--random-url ()
  "Return Special:Random URL for current `wiki-rencontre-language`."
  (format "https://%s.wikipedia.org/wiki/Special:Random"
	  wiki-rencontre-language))

;; Autoload cookie for autoload generation.
;;;###autoload
(defun wiki-rencontre ()
  "Open a random Wikipedia page."
  (interactive)
  (require 'eww)
  (eww (wiki-rencontre--random-url)))

(provide 'config-wiki-rencontre)
;;; config-wiki-rencontre.el ends here       
