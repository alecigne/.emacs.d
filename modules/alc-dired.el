(define-prefix-command 'alc-dired-hacks-map)

(use-package dired
  :bind
  (:map dired-mode-map
        ("h" . alc-dired-hacks-map))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-listing-switches "-lha"))

(use-package dired+
  :ensure t
  :after dired
  :config
  (diredp-toggle-find-file-reuse-dir 1))

(use-package dired-narrow
  :ensure t
  :after dired
  :bind (:map alc-dired-hacks-map
              ("n" . dired-narrow)
              ("f" . dired-narrow-fuzzy)
              ("r" . dired-narrow-regexp)))

(use-package dired-collapse
  :ensure t
  :bind (:map alc-dired-hacks-map
              ("c" . dired-collapse-mode)))

(provide 'alc-dired)
