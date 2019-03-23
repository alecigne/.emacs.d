(use-package erc
  :config
  (define-key erc-mode-map (kbd "C-c C-x") nil)
  (setq erc-track-position-in-mode-line 'before-modes
        erc-track-shorten-names nil
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477")
        erc-hide-list '("JOIN" "PART" "QUIT")
        erc-autojoin-channels-alist '((".*\\.freenode.net"
                                         "#stumpwm"
                                         "#lisp"
                                         "#emacs"))))

(use-package jabber
  :ensure t)

(provide 'swanemacs-communication)
