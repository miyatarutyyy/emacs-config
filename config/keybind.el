;; C-h sets delete
;; C-? sets help
(keymap-global-set "C-h" #'delete-backward-char)
(keymap-global-set "C-?" #'help-command)

;; ace-window
(keymap-global-set "M-o" #'ace-window)

;; delete-word / backward-delete-word
(defun delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  (interactive "p")
  (delete-word (- arg)))

(keymap-global-set "M-h" #'backward-delete-word)

;; bs: buffer cycle
(require 'bs)
(keymap-global-set "M-]" #'bs-cycle-next)
(keymap-global-set "M-[" #'bs-cycle-previous)

(provide 'config-keybind)
