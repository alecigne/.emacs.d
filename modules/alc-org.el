(use-package org
  :ensure nil ; correct version already installed by init.el
  :delight org-mode "org"
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c f" . org-footnote-new)
         ("C-c c" . org-capture))
  :config
  (delight '((org-src-mode " org-src" "org-src")
             (org-indent-mode nil "org")))
  
  (setq org-link-frame-setup
        '((vm . vm-visit-folder-other-frame)
          (vm-imap . vm-visit-imap-folder-other-frame)
          (gnus . org-gnus-no-new-news)
          (file . find-file)
          (wl . wl-other-frame)))
  
  (setq org-link-file-path-type 'relative)
  (defun alc-org-replace-link-by-link-description ()
    "Replace an org link by its description or if empty its address"
    (interactive)
    (if (org-in-regexp org-bracket-link-regexp 1)
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description (if (match-end 3) 
                               (org-match-string-no-properties 3)
                             (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description))))
  (setq org-treat-S-cursor-todo-selection-as-state-change nil
        org-treat-insert-todo-heading-as-state-change t
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil
        org-src-tab-acts-natively t
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-agenda-skip-deadline-if-done nil
        org-agenda-skip-scheduled-if-done nil
        org-agenda-start-on-weekday 1
        org-use-fast-todo-selection t)
  (setq org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies nil
        org-agenda-dim-blocked-tasks t)
  (setq org-log-into-drawer t)
  (setq org-lowest-priority ?C)
  (setq org-deadline-warning-days 14)
  (setq org-archive-location "%s_archive::")
  
  (defadvice org-archive-subtree
      (before add-inherited-tags-before-org-archive-subtree activate)
    "add inherited tags before org-archive-subtree"
    (org-set-tags-to (org-get-tags-at)))
  (setq org-hide-emphasis-markers nil)
  (setq org-icalendar-use-deadline nil
        org-icalendar-use-scheduled nil
        org-icalendar-include-body nil)
  ;; (with-eval-after-load 'org
  ;;   (set-face-attribute 'org-meta-line nil :height 0.7 :slant 'normal))
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (lisp . t)
     (python . t)
     (R . t)
     (ruby . t)
     (shell . t)))
  (setq org-use-speed-commands t
        org-speed-commands-user '(("a" org-archive-subtree)))
  (setq org-startup-indented t)
  (delight 'org-indent-mode nil "org-indent"))

(use-package org-inlinetask
  :commands org-inlinetask-insert-task
  :bind (:map org-mode-map
              ("C-c C-x t" . org-inlinetask-insert-task))
  :after org)

(use-package org-bullets
  :ensure t
  :after org
  :init
  (when (eq system-type 'gnu/linux)
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

(use-package org-crypt
  :ensure nil ; in contrib
  :bind (("C-c z" . org-decrypt-entry))
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt"))))

(use-package org-pomodoro
  :ensure t
  :after org)

(use-package org-sticky-header
  :ensure t
  :config
  (setq org-sticky-header-full-path 'full))

(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-enable))

(use-package org-tree-slide
  :ensure t)

(use-package demo-it
  :ensure t)

(provide 'alc-org)
