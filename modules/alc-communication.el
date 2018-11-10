(use-package erc
  :config
  (setq erc-track-position-in-mode-line 'before-modes
        erc-track-shorten-names nil
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477")
        erc-hide-list '("JOIN" "PART" "QUIT")))

(use-package jabber
  :ensure t)

(provide 'alc-communication)
