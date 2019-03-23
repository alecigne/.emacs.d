(use-package google-this
  :ensure t
  :defer 5)

(use-package writeroom-mode
  :ensure t
  :bind ("H-<f10>" . writeroom-mode)
  :config
  (setq writeroom-restore-window-config t))

(use-package emojify
  :ensure t)

(use-package fireplace
  :ensure t)

(use-package lorem-ipsum
  :ensure t)

(provide 'swanemacs-gadgets)
