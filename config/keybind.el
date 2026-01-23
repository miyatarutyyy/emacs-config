;; C-h sets delete
;; C-? sets help
(keymap-global-set "C-h" #'delete-backward-char)
(keymap-global-set "C-?" #'help-command)

;; ace-window
(keymap-global-set "M-o" #'ace-window)

(provide 'config-keybind)
