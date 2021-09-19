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
        org-special-ctrl-k t
        org-startup-folded t)

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

  (add-to-list 'org-modules 'org-id)
  (setq org-id-link-to-org-use-id 'create-if-interactive
        org-link-file-path-type 'relative)

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
        org-provide-todo-statistics '("TODO" "PROG" "WAIT")
        ;; do not dim DONE items
        org-fontify-done-headline nil)

  ;; exclude the 'project' tag from inheritance
  (add-to-list 'org-tags-exclude-from-inheritance "project")

  (setq org-todo-keywords
        '((sequence "TODO(t!)"
                    "IDEA(i!)"
                    "PROG(p!)"
                    "WAIT(w@/!)"
                    "HOLD(h@/!)"
                    "|"
                    "DONE(d!)"
                    "GIVN(g@)"
                    "CNCL(c@)")))

  (defface alc-org-todo-kwd
    '((t (:weight bold :foreground "red")))
    "Face used to display tasks yet to be worked on.")

  (defface alc-org-in-progress-kwd
    '((t (:weight bold :foreground "orange")))
    "Face used to display tasks in progress.")

  (defface alc-org-idea-kwd
    '((t (:weight bold :foreground "deep sky blue")))
    "Face used to display tasks that might be done someday.")

  (defface alc-org-done-kwd
    '((t (:weight bold :foreground "forest green")))
    "Face used to display org state DONE.")

  (setq org-todo-keyword-faces
        '(("TODO" . alc-org-todo-kwd)
          ("IDEA" . alc-org-idea-kwd)
          ("PROG" . alc-org-in-progress-kwd)
          ("WAIT" . alc-org-in-progress-kwd)
          ("HOLD" . alc-org-in-progress-kwd)
          ("DONE" . alc-org-done-kwd)
          ("GIVN" . alc-org-done-kwd)
          ("CNCL" . alc-org-done-kwd)))

  (setq org-capture-templates
        `(;; New task
          ("t" "Capture [t]ask"
           entry
           (file alc-org-inbox-file)
           "* TODO %?"
           :kill-buffer t)
          ;; New note
          ("n" "Capture [n]ote"
           entry
           (file alc-org-inbox-file)
           ,(concat "* Note (taken from %a)\n" "\n" "%?\n")
           :kill-buffer t)))

;; *** Agenda

  (setq org-agenda-files (list alc-org-todo-file alc-org-inbox-file alc-org-almanac-file)
        org-agenda-format-date "%Y-%m-%d %a")

  (alc-with-system-type personal
    (add-to-list 'org-agenda-files alc-org-entourage-file))

  ;; https://www.reddit.com/r/emacs/comments/jjrk2o/hide_empty_custom_agenda_sections/gaeh3st
  (defun alc-org-agenda-delete-empty-blocks ()
    "Remove empty agenda blocks.

A block is identified as empty if there are fewer than 2
non-empty lines in the block (excluding the line with
`org-agenda-block-separator' characters)."
    (when org-agenda-compact-blocks
      (user-error "Cannot delete empty compact blocks"))
    (setq buffer-read-only nil)
    (save-excursion
      (goto-char (point-min))
      (let* ((blank-line-re "^\\s-*$")
             (content-line-count (if (looking-at-p blank-line-re) 0 1))
             (start-pos (point))
             (block-re (format "%c\\{10,\\}" org-agenda-block-separator)))
        (while (and (not (eobp)) (forward-line))
          (cond
           ((looking-at-p block-re)
            (when (< content-line-count 2)
              (delete-region start-pos (1+ (point-at-bol))))
            (setq start-pos (point))
            (forward-line)
            (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
           ((not (looking-at-p blank-line-re))
            (setq content-line-count (1+ content-line-count)))))
        (when (< content-line-count 2)
          (delete-region start-pos (point-max)))
        (goto-char (point-min))
        ;; The above strategy can leave a separator line at the beginning
        ;; of the buffer.
        (when (looking-at-p block-re)
          (delete-region (point) (1+ (point-at-eol))))))
    (setq buffer-read-only t))

  (add-hook 'org-agenda-finalize-hook #'alc-org-agenda-delete-empty-blocks)

  ;; Some diary entries are displayed in the agenda

  (defun diary-sunrise ()
    (replace-regexp-in-string
     "\\(Sunrise [^ ]*\\).*" "\\1"
     (diary-sunrise-sunset)))

  (defun diary-sunset ()
    (replace-regexp-in-string
     ".*\\(unset [^ ]*\\).*(\\(.*\\)" "S\\1 (after \\2"
     (diary-sunrise-sunset)))

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
        '(todo "WAIT"
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
                   '(org-agenda-skip-entry-if 'todo '("WAIT" "HOLD" "DONE" "CNCL"))))))

;; **** Custom commands

  (setq alc-org-agenda-custom-command-daily-digest
        `(;; key and desc
          "d" "Today"
          ;; views
          (,alc-org-agenda-view-events-today
           ,alc-org-agenda-view-scheduled-today
           ,alc-org-agenda-view-deadlines
           ,alc-org-agenda-view-waiting
           ,alc-org-agenda-view-inbox)
          ;; global settings
          nil
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

;; ** Contrib

;; TODO useful?
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-expiry))

(use-package org-expiry
  :ensure nil
  :after org
  :demand t  ; no autoloads
  :config
  ;; Track heading creation in a PROPERTIES drawer. It also works for
  ;; TODO headings since `org-insert-todo-heading' calls
  ;; `org-insert-heading'.
  ;;
  ;; `org-expiry-insinuate' would do this for an editing session (and
  ;; a few more things). I don't really use it that way.
  (defadvice org-insert-heading (after alc-org-insert-heading-created-advice activate)
    (org-expiry-insert-created))
  (ad-activate 'org-insert-heading)

  ;; Do the same for org-capture since `org-insert-heading' is not
  ;; called explicitely.
  (add-hook 'org-capture-before-finalize-hook #'org-expiry-insert-created)

  ;; Don't show these timestamps in the agenda.
  (setq org-expiry-inactive-timestamps t))

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
  (add-to-list 'org-tags-exclude-from-inheritance "crypt")
  (setq org-crypt-key "F62FE7A4"))

;; ** Others

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

(use-package org-edna
  :after org
  :config
  (org-edna-mode))

;; * Wrapping up

(provide 'alc-org)
