;;; ==========================================================
;;; vim-jp-radio and required podcaster settings
;;; ==========================================================

(leaf podcaster
  :ensure t
  :custom
  ((podcaster-mp3-player . "mpv")
   (podcaster-mp3-player-extra-params
    . '("--no-video" "--really-quiet" "--force-window=no" "--idle=no"))))

(leaf vim-jp-radio
  :vc (:url "https://github.com/vim-jp-radio/vim-jp-radio.el"))

(provide 'config-vim-jp-radio)
