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
  
  (setq org-M-RET-may-split-line t)
  (defun alc-org-insert-drawer-note ()
    (interactive)
    (org-insert-drawer nil "NOTES"))
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
  (setq org-provide-todo-statistics '("TODO" "DOING" "WAITING" "TODO?"))
  (setq org-hierarchical-todo-statistics nil)
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
  (setq org-structure-template-alist
        '(("C" "#+BEGIN_COMMENT\n?\n#+END_COMMENT")
          ("E" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC")
          ("elt" "#+BEGIN_SRC emacs-lisp :tangle yes\n?\n#+END_SRC")
          ("s" "#+BEGIN_SRC ?\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>")
          ("e" "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE" "<example>\n?\n</example>")
          ("q" "#+BEGIN_QUOTE\n?\n#+END_QUOTE" "<quote>\n?\n</quote>")
          ("v" "#+BEGIN_VERSE\n?\n#+END_VERSE" "<verse>\n?\n</verse>")
          ("V" "#+BEGIN_VERBATIM\n?\n#+END_VERBATIM" "<verbatim>\n?\n</verbatim>")
          ("c" "#+BEGIN_CENTER\n?\n#+END_CENTER" "<center>\n?\n</center>")
          ("l" "#+BEGIN_LaTeX\n?\n#+END_LaTeX" "<literal style=\"latex\">\n?\n</literal>")
          ("L" "#+LaTeX: " "<literal style=\"latex\">?</literal>")
          ("h" "#+BEGIN_HTML\n?\n#+END_HTML" "<literal style=\"html\">\n?\n</literal>")
          ("H" "#+HTML: " "<literal style=\"html\">?</literal>")
          ("a" "#+BEGIN_ASCII\n?\n#+END_ASCII" "")
          ("A" "#+ASCII: " "")
          ("i" "#+INDEX: ?" "#+INDEX: ?")
          ("I" "#+INCLUDE: %file ?" "<include file=%file markup=\"?\">")))
  (setq org-use-speed-commands t
        org-speed-commands-user '(("a" org-archive-subtree)))
  (setq org-startup-indented t)
  (delight 'org-indent-mode nil "org-indent")
  (defun alc-org-insert-cookie-end-of-heading ()
    "Insert a cookie at the end of the current heading and update
      it, unless it is already here. In that case, delete it."
    (interactive)
    (save-excursion
      (unless (org-at-heading-p)
        (org-back-to-heading))
      (beginning-of-line)
      (if (not (looking-at ".* \\[[0-9]*/[0-9]*\\]"))
          (progn
            (org-end-of-line)
            (insert " [/]")
            (org-update-statistics-cookies nil)
            (alc-org-change-tags-column))
        (progn
          (replace-regexp "\\(.*\\) \\[[0-9]*/[0-9]*\\]\\(.*\\)" "\\1\\2" nil (point) (save-excursion (end-of-line) (point)))
          (alc-org-change-tags-column)))))
  
  (defun alc-org-schedule-if-doing-or-waiting ()
    "Schedule when the task is marked DOING or WAITING, unless the
  item is already scheduled."
    (when (and (or (string= org-state "DOING")
                   (string= org-state "WAITING"))
               (not (string= org-last-state org-state))
               (not (org-get-scheduled-time (point))))
      (org-schedule nil "")))
  
  (add-hook 'org-after-todo-state-change-hook
            'alc-org-schedule-if-doing-or-waiting)
  
  ;; https://emacs.stackexchange.com/a/9588
  (require 'cl-lib)
  (require 'dash)
  
  (defun alc-todo-to-int (todo)
    (cl-first (-non-nil
            (mapcar (lambda (keywords)
                      (let ((todo-seq
                             (-map (lambda (x) (cl-first (split-string  x "(")))
                                   (cl-rest keywords)))) 
                        (cl-position-if (lambda (x) (string= x todo)) todo-seq)))
                    org-todo-keywords))))
  
  (defun alc-org-sort-key ()
    (let* ((todo-max (apply #'max (mapcar #'length org-todo-keywords)))
           (todo (org-entry-get (point) "TODO"))
           (todo-int (if todo (alc-todo-to-int todo) todo-max))
           (priority (org-entry-get (point) "PRIORITY"))
           (priority-int (if priority (string-to-char priority) org-default-priority)))
      (format "%03d %03d" todo-int priority-int)
      ))
  
  (defun alc-org-sort-entries ()
    (interactive)
    (org-sort-entries nil ?f #'alc-org-sort-key)))

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
