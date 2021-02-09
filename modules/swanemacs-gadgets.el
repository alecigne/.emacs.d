(use-package google-this
  :defer 5)

(use-package writeroom-mode
  :bind ("H-<f10>" . writeroom-mode)
  :config
  (setq writeroom-restore-window-config t))

(use-package emojify)

(use-package fireplace)

(use-package lorem-ipsum)

(use-package youtube-dl
  :unless (eq system-type 'windows-nt)
  :load-path "~/.emacs.d/lisp/youtube-dl-emacs/"
  :ensure nil
  :demand t)

(use-package theme-magic)

(use-package all-the-icons)

(provide 'swanemacs-gadgets)
