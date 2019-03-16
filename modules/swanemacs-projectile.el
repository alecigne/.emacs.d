(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (when (require 'helm nil 'noerror)
    (setq projectile-completion-system 'helm))
  (setq projectile-mode-line '(:eval (if (condition-case nil
                                             (and projectile-require-project-root
                                                  (projectile-project-root))
                                           (error nil))
                                         (format " π[%s]"
                                                 (projectile-project-name))
                                       "")))
  (projectile-mode))

(provide 'swanemacs-projectile)
