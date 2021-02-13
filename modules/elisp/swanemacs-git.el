(use-package gitignore-mode
  :mode ".gitignore$")

(use-package gitconfig-mode
  :mode ".gitconfig$")

(use-package git-gutter
  :hook (prog-mode)
  :delight
  :config
  (global-git-gutter-mode)
  (setq git-gutter:hide-gutter t))

(use-package git-timemachine)

(use-package git-messenger)

(provide 'swanemacs-git)
