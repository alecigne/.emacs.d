(use-package projectile
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

(use-package ibuffer-projectile
  :init
  (defun swanemacs-ibuffer-customization ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))

  (add-hook 'ibuffer-hook #'swanemacs-ibuffer-customization))

(provide 'swanemacs-projectile)
