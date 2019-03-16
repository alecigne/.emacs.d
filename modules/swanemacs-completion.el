(use-package company
  :ensure t
  :demand
  :delight
  :config
  (global-company-mode))

(use-package company-quickhelp
  :ensure t
  :hook (company-mode . company-quickhelp-mode))

(use-package yasnippet
  :ensure t
  :delight yas-minor-mode
  :demand
  :config
  (yas-global-mode 1))

(provide 'swanemacs-completion)
