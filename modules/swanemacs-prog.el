(delight '((lisp-mode "λ" :major)
           (lisp-interaction-mode "λ" :major)
           (emacs-lisp-mode "el" :major)))

(use-package slime
  :ensure t
  :config
  (when (equal alc-current-system "laptop-linux")
    (setq slime-contribs '(slime-fancy)
          slime-protocol-version 'ignore)
    (setq inferior-lisp-program "sbcl"))

  (defun alc-swank-listening-p ()
    (ignore-errors
      (let ((p (open-network-stream "SLIME Lisp Connection Test" nil "localhost" 4005)))
        (when p
          (delete-process p)
          t))))

  (defun alc-swank-autoconnect (&rest args)
    (if (and (not (slime-connected-p))
             (alc-swank-listening-p))
        (ignore-errors (slime-connect "localhost" 4005))))

  (alc-swank-autoconnect))

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
  (defun swanemacs-prog-add-company-jedi ()
    (add-to-list 'company-backends 'company-jedi))

  (add-hook 'python-mode-hook 'swanemacs-prog-add-company-jedi)
  (add-hook 'python-mode-hook 'jedi:setup)
  :config
  (setq jedi:complete-on-dot t))

(provide 'swanemacs-prog)
