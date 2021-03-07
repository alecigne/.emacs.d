;; * Org

(use-package org
  :pin org
  :ensure org-plus-contrib
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c f" . org-footnote-new)
         ("C-c c" . org-capture)
         ("<f7>" . org-agenda)
         :map org-mode-map
         ("C-c C" . alc-org-insert-cookie))
  :config

;; ** Basics

  (setq org-use-speed-commands t
        org-speed-commands-user '(("a" org-archive-subtree))
        org-startup-indented t
        org-special-ctrl-a/e t
        org-special-ctrl-k t)

  (delight 'org-indent-mode nil "org-indent")

  (defun alc-org-insert-cookie ()
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
            (org-update-statistics-cookies nil))
        (progn
          (re-search-forward "\\(.*\\) \\[[0-9]*/[0-9]*\\]\\(.*\\)" nil t)
          (replace-match "\\1\\2"))
        (org-align-tags))))

;; ** Hyperlinks

  ;; https://emacs.stackexchange.com/a/10714
  (defun alc-org-replace-link-by-link-description ()
    "Replace an org link by its description or if empty its address."
    (interactive)
    (if (org-in-regexp org-link-bracket-re 1)
        (save-excursion
          (let ((remove (list (match-beginning 0) (match-end 0)))
                (description
                 (if (match-end 2)
                     (org-match-string-no-properties 2)
                   (org-match-string-no-properties 1))))
            (apply 'delete-region remove)
            (insert description)))))

;; ** Gettings Things Done: todos, agenda, capture

  (setq org-treat-S-cursor-todo-selection-as-state-change nil
        org-treat-insert-todo-heading-as-state-change t
        org-confirm-babel-evaluate nil
        org-use-fast-todo-selection 'expert
        org-log-into-drawer t
        ;; recursive statistics cookies
        org-hierarchical-todo-statistics nil
        ;; block DONE state on parent if a child isn't DONE
        org-enforce-todo-dependencies t
        org-provide-todo-statistics '("TODO" "DOING" "WAITING" "TODO?"))

  (setq org-todo-keywords
        '((sequence "TODO(t!)"
                    "TODO?(m!)"
                    "DOING(D!)"
                    "WAITING(w@/!)"
                    "HOLD(h@/!)"
                    "|"
                    "DONE(d!)"
                    "CANCELED(x@)")
          (sequence "TOCOMPLETE(T!)"
                    "COMPLETING(C!)"
                    "TOCOMPLETE?(M!)"
                    "|"
                    "COMPLETED(c!)"
                    "ABORTED(X@)")))

  (defface alc-org-todo-kwd
    '((t (:weight bold :foreground "red")))
    "Face used to display tasks yet to be worked on.")

  (defface alc-org-in-progress-kwd
    '((t (:weight bold :foreground "orange")))
    "Face used to display tasks in progress.")

  (defface alc-org-someday-kwd
    '((t (:weight bold :foreground "dark red")))
    "Face used to display tasks that might be done someday.")

  (defface alc-org-done-kwd
    '((t (:weight bold :foreground "forest green")))
    "Face used to display org state DONE.")

  (setq org-todo-keyword-faces
        '(("TODO" . alc-org-todo-kwd)
          ("TOCOMPLETE" . alc-org-todo-kwd)
          ("TODO?" . alc-org-someday-kwd)
          ("TOCOMPLETE?" . alc-org-someday-kwd)
          ("DOING" . alc-org-in-progress-kwd)
          ("COMPLETING" . alc-org-in-progress-kwd)
          ("WAITING" . alc-org-in-progress-kwd)
          ("HOLD" . alc-org-in-progress-kwd)
          ("DONE" . alc-org-done-kwd)
          ("COMPLETED" . alc-org-done-kwd)
          ("CANCELED" . alc-org-done-kwd)
          ("ABORTED" . alc-org-done-kwd)))

  (setq org-capture-templates
        '(;; New task in inbox
          ("t" "Capture [t]ask"
           entry
           (file+olp alc-org-todo-file "Inbox")
           "* TODO %?"
           :prepend t
           :kill-buffer t)))

  (defun alc-org-auto-schedule ()
    "Schedule when the task is marked DOING or WAITING, unless the
  item is already scheduled."
    (when (and (or (string= org-state "DOING")
                   (string= org-state "WAITING"))
               (not (string= org-last-state org-state))
               (not (org-get-scheduled-time (point))))
      (org-schedule nil "")))

  (add-hook 'org-after-todo-state-change-hook 'alc-org-auto-schedule)

;; *** Agenda

  (setq org-agenda-files (list alc-org-todo-file)
        org-agenda-format-date "%Y-%m-%d %a")

  ;; For agenda commands, I only use the format for composite buffers:
  ;; (key desc ((type match settings)) settings files)
  ;;
  ;; see the docs for the `org-agenda-custom-commands' variable.
  ;;
  ;; This allows me to declare multiple (type match settings)
  ;; structures and to reuse them afterwards. I call these structures
  ;; 'views' below.
  ;;
  ;; Example custom filter:
  ;;
  ;; (defun my-custom-filter ()
  ;;   (let ((subtree-end (save-excursion (org-end-of-subtree t))))
  ;;     (when (member (org-get-todo-state) '("WAITING" "HOLD" "DONE" "CANCELED"))
  ;;         subtree-end)))

;; **** Views

  ;; events today
  (setq alc-org-agenda-view-events-today
        '(agenda ""
                 ((org-agenda-overriding-header "Events today")
                  (org-agenda-entry-types '(:timestamp :sexp))
                  (org-agenda-span 'day))))

  ;; events this month
  (setq alc-org-agenda-view-events-month
        '(agenda ""
                 ((org-agenda-overriding-header "Events this month")
                  (org-agenda-entry-types '(:timestamp :sexp))
                  (org-agenda-span 'month))))

  ;; tasks in the inbox
  (setq alc-org-agenda-view-inbox
        '(tags-todo "inbox" ((org-agenda-overriding-header "Tasks in the inbox\n"))))

  ;; deadlines
  (setq alc-org-agenda-view-deadlines
        '(agenda ""
                 ((org-agenda-overriding-header "Deadlines")
                  (org-agenda-span 'day)
                  (org-agenda-entry-types '(:deadline))
                  (org-deadline-warning-days 365)
                  (org-agenda-time-grid nil)
                  (org-agenda-sorting-strategy '(deadline-up)))))

  ;; waiting
  (setq alc-org-agenda-view-waiting
        '(todo "WAITING"
               ((org-agenda-overriding-header "Waiting for something\n"))))

  (setq alc-org-agenda-view-scheduled-today
        '(agenda ""
                 ((org-agenda-overriding-header "Scheduled today")
                  (org-agenda-entry-types '(:scheduled))
                  (org-agenda-span 'day)
                  (org-agenda-sorting-strategy '(priority-down time-down))
                  (org-agenda-start-on-weekday nil)
                  (org-agenda-time-grid nil)
                  (org-agenda-skip-function
                   '(org-agenda-skip-entry-if 'todo '("WAITING" "HOLD" "DONE" "CANCELED"))))))

;; **** Custom commands

  (setq alc-org-agenda-custom-command-daily-digest
        `(;; key and desc
          "d" "Today"
          ;; views
          (,alc-org-agenda-view-events-today
           ,alc-org-agenda-view-scheduled-today
           ,alc-org-agenda-view-inbox
           ,alc-org-agenda-view-deadlines
           ,alc-org-agenda-view-waiting)
          ;; global settings
          ((org-agenda-format-date ""))
          ;; files
          nil))

  (setq alc-org-agenda-custom-command-events-month
        `(;; key and desc
          "v" "Events this month"
          ;; views
          (,alc-org-agenda-view-events-month)
          ;; global settings
          nil
          ;; files
          nil))

  ;; wrapping up
  (setq org-agenda-custom-commands
        `(;; daily digest
          ,alc-org-agenda-custom-command-daily-digest
          ;; events this month
          ,alc-org-agenda-custom-command-events-month))

;; ** Babel

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (lisp . t)
     (plantuml . t)
     (python . t)
     (shell . t))))

;; * Org-related packages

(use-package outline
  :delight outline-minor-mode)

(use-package outshine
  :delight
  :config (setq outshine-use-speed-commands t)
  :hook (emacs-lisp-mode . outshine-mode))

(use-package helm-org-rifle
  :bind
  (:map org-mode-map ("C-c s" . helm-org-rifle-current-buffer)))

(alc-with-system gnu/linux
  (use-package org-bullets
    :hook (org-mode . (lambda () (org-bullets-mode 1)))))

(use-package org-inlinetask
  :ensure nil
  :init
  (put 'org-inlinetask-min-level 'safe-local-variable #'numberp)
  :commands org-inlinetask-insert-task
  :bind (:map org-mode-map ("C-c C-x t" . org-inlinetask-insert-task)))

(use-package org-crypt
  :ensure nil ; in contrib
  :bind (("C-c z" . org-decrypt-entry))
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt"))
        (setq org-crypt-key "F62FE7A4")))

(use-package org-sticky-header
  :config
  (setq org-sticky-header-full-path 'full))

(use-package toc-org
  :hook (org-mode . toc-org-enable))

(use-package org-tree-slide
  :bind (:map org-tree-slide-mode-map
              ("<f9>" . org-tree-slide-move-previous-tree)
              ("<f10>" . org-tree-slide-move-next-tree)))

(use-package demo-it)

(provide 'alc-org)