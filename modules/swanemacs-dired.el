(define-prefix-command 'swanemacs-dired-hacks-map)

(use-package dired
  :ensure nil
  :bind
  (:map dired-mode-map ("h" . swanemacs-dired-hacks-map))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-listing-switches "-lha"))

(use-package dired+
  :ensure nil
  :load-path "~/.emacs.d/lisp/dired+/"
  :demand t
  :after dired
  :config
  (diredp-toggle-find-file-reuse-dir 1))

(use-package dired-narrow
  :after dired
  :bind (:map swanemacs-dired-hacks-map
              ("n" . dired-narrow)
              ("f" . dired-narrow-fuzzy)
              ("r" . dired-narrow-regexp)))

(use-package dired-collapse
  :bind (:map swanemacs-dired-hacks-map ("c" . dired-collapse-mode)))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'swanemacs-dired)
