(use-package company
  :demand
  :delight
  :config
  (global-company-mode))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

(use-package yasnippet
  :delight yas-minor-mode
  :demand
  :config
  (yas-global-mode 1))

(provide 'swanemacs-completion)
