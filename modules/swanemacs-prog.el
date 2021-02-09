(use-package flycheck
  :defer 2
  :delight)

(use-package plantuml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (setq org-plantuml-jar-path (expand-file-name "bin/plantuml.jar"
                                                swanemacs-root-dir)))

(delight '((lisp-mode "λ" :major)
           (lisp-interaction-mode "λ" :major)
           (emacs-lisp-mode "el" :major)))

(use-package slime
  :config
  (when (eq system-type 'gnu/linux)
    (setq slime-contribs '(slime-fancy)
          slime-protocol-version 'ignore
          inferior-lisp-program "sbcl"))

  (defun swanemacs-swank-listening-p ()
    (ignore-errors
      (let ((p (open-network-stream "SLIME Lisp Connection Test" nil "localhost" 4005)))
        (when p
          (delete-process p)
          t))))

  (defun swanemacs-swank-autoconnect (&rest args)
    (if (and (not (slime-connected-p))
             (swanemacs-swank-listening-p))
        (ignore-errors (slime-connect "localhost" 4005))))

  (swanemacs-swank-autoconnect))

(use-package geiser)

(use-package elpy
  :init
  (with-eval-after-load 'python (elpy-enable))
  :config
  (setq elpy-rpc-python-command "python3"
        python-shell-interpreter "python3"))

(use-package pyvenv)

(use-package company-jedi
  ;; :after company
  :init
  (defun swanemacs-prog-add-company-jedi ()
    (add-to-list 'company-backends 'company-jedi))

  (add-hook 'python-mode-hook 'swanemacs-prog-add-company-jedi)
  (add-hook 'python-mode-hook 'jedi:setup)
  :config
  (setq jedi:complete-on-dot t))

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

(provide 'swanemacs-prog)
