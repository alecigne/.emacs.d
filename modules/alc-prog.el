(delight '((lisp-mode "λ" :major)
           (lisp-interaction-mode "λ" :major)
           (emacs-lisp-mode "el" :major)))

(use-package slime
  :ensure t)

(use-package geiser
  :ensure t)

(use-package elpy
  :ensure t
  :init
  (with-eval-after-load 'python (elpy-enable))
  :config
  (setq elpy-rpc-python-command "python3"
        python-shell-interpreter "python3"))

(use-package pyvenv
  :ensure t)

(use-package company-jedi
  :ensure t
  ;; :after company
  :init
  (defun alc-prog-add-company-jedi ()
    (add-to-list 'company-backends 'company-jedi))

  (add-hook 'python-mode-hook 'alc-prog-add-company-jedi)
  (add-hook 'python-mode-hook 'jedi:setup)
  :config
  (setq jedi:complete-on-dot t))

(provide 'alc-prog)
