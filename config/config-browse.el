;;; config-browse.el --- browse-url policy
;;; (eww default, w3m on demand)

;; Default: use eww for browse-url
(setq browse-url-browser-function 'eww-browse-url)

(defun my/browse-url-w3m (url &optional new-window)
  "Browse URL with w3m (manual override)."
  (interactive (browse-url-interactive-arg "URL (w3m): "))
  (require 'w3m)
  (w3m-browse-url url new-window))

(defun my/browse-url-at-point-w3m ()
  "Browse URL at point with w3m."
  (interactive)
  (require 'w3m)
  (let ((url (or (thing-at-point 'url t)
		 (when (fboundp 'browse-url-url-at-point)
		   (browse-url-url-at-point)))))
    (unless url (user-error "No URL at point"))
    (w3m-browse-url url)))

(provide 'config-browse)
;;; config-browse.el ends here
