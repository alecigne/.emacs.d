;; * Org

(use-package org
  ;; Outline-based notes management and organizer.
  ;; https://orgmode.org/
  :ensure t
  :pin gnu
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
        org-startup-folded t
        org-image-actual-width nil)

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

  (setq alc-org-insert-created-timestamp-format "[%Y-%m-%d %a %H:%M]"
        alc-org-insert-created-property "CREATED")

  (defun alc-org-insert-created ()
    "Insert a CREATED property with the current date."
    (interactive)
    (let ((existing-date (org-entry-get (point) alc-org-insert-created-property))
          (now (format-time-string alc-org-insert-created-timestamp-format)))
      (when (null existing-date)
        (save-excursion
	  (org-entry-put
	   (point) alc-org-insert-created-property now)))))

  (dolist (fn '(org-insert-heading org-toggle-heading))
    (advice-add fn :after #'(lambda (&rest _) (alc-org-insert-created))))

  ;; Do the same for org-capture since `org-insert-heading' is not called
  ;; explicitely.
  (add-hook 'org-capture-before-finalize-hook #'alc-org-insert-created)

;; ** Hyperlinks

  (add-to-list 'org-modules 'org-id)
  (setq org-id-link-to-org-use-id 'create-if-interactive
        org-link-file-path-type 'relative)

  (defun alc-org-replace-link-by-link-description ()
    "Replace an org link by its description or if empty its
address."
    ;; https://emacs.stackexchange.com/a/10714
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
        ;; Recursive statistics cookies
        org-hierarchical-todo-statistics nil
        ;; Block DONE state on parent if a child isn't DONE
        org-enforce-todo-dependencies t
        org-provide-todo-statistics '("TODO" "PROG" "WAIT")
        ;; Do not dim DONE items
        org-fontify-done-headline nil)

  ;; Exclude the 'project' tag from inheritance
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

  ;; The default value for `org-directory', `~/org', will be used. On my
  ;; individual machines, I usually add an advice around `org-capture' so it
  ;; lets me choose a project in which the capture will happen. See the README
  ;; for more info.
  (setq org-capture-templates
        `(;; New task
          ("t" "Capture [t]ask"
           entry
           (file "inbox.org")
           "* TODO %?"
           :kill-buffer t)
          ;; New note
          ("n" "Capture [n]ote"
           entry
           (file "inbox.org")
           ,(concat "* Note (taken from %a)\n" "\n" "%?\n")
           :kill-buffer t)))

  ;; TODO This doesn't set the correct date in the logbook.
  (defun alc-org-todo-custom-date (&optional arg)
    "Like org-todo-yesterday, but prompt the user for a date. The
time of change will be 23:59 on that day"
    (interactive "P")
    (let* ((hour (nth 2 (decode-time (org-current-time))))
           (daysback (- (date-to-day (current-time-string))
                        (org-time-string-to-absolute (org-read-date))))
           (org-extend-today-until (+ 1 (* 24 (- daysback 1)) hour))
           (org-use-effective-time t)) ; use the adjusted timestamp for logging
      (if (eq major-mode 'org-agenda-mode)
          (org-agenda-todo arg)
        (org-todo arg))))

;; *** Agenda

  (setq org-agenda-format-date "%Y-%m-%d %a")

  (defun alc-org-agenda-delete-empty-blocks ()
    "Remove empty agenda blocks.

A block is identified as empty if there are fewer than 2
non-empty lines in the block (excluding the line with
`org-agenda-block-separator' characters)."
    ;; https://www.reddit.com/r/emacs/comments/jjrk2o/hide_empty_custom_agenda_sections/gaeh3st
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
        ;; The above strategy can leave a separator line at the beginning of the
        ;; buffer.
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

  ;; For agenda commands, I only use the format for composite buffers: (key desc
  ;; ((type match settings)) settings files)
  ;;
  ;; See the docs for the `org-agenda-custom-commands' variable.
  ;;
  ;; This allows me to declare multiple (type match settings) structures and to
  ;; reuse them afterwards. I call these structures 'views' below.
  ;;
  ;; Example custom filter:
  ;;
  ;; (defun my-custom-filter ()
  ;;   (let ((subtree-end (save-excursion (org-end-of-subtree t))))
  ;;     (when (member (org-get-todo-state)
  ;;                   '("WAITING" "HOLD" "DONE" "CANCELED"))
  ;;       subtree-end)))

;; **** Views

  ;; Events today
  (setq alc-org-agenda-view-events-today
        '(agenda ""
                 ((org-agenda-overriding-header "Events today")
                  (org-agenda-entry-types '(:timestamp :sexp))
                  (org-agenda-span 'day))))

  ;; Events this month
  (setq alc-org-agenda-view-events-month
        '(agenda ""
                 ((org-agenda-overriding-header "Events this month")
                  (org-agenda-entry-types '(:timestamp :sexp))
                  (org-agenda-span 'month))))

  ;; Tasks in the inbox
  (setq alc-org-agenda-view-inbox
        '(tags-todo "inbox"
                    ((org-agenda-overriding-header "Tasks in the inbox\n"))))

  ;; deadlines
  (setq alc-org-agenda-view-deadlines
        '(agenda ""
                 ((org-agenda-overriding-header "Deadlines")
                  (org-agenda-span 'day)
                  (org-agenda-entry-types '(:deadline))
                  (org-deadline-warning-days 28)
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
                   '(org-agenda-skip-entry-if
                     'todo '("WAIT" "HOLD" "DONE" "CNCL"))))))

;; **** Custom commands

  (setq alc-org-agenda-custom-command-daily-digest
        `(;; Key and desc
          "d" "Today"
          ;; Views
          (,alc-org-agenda-view-events-today
           ,alc-org-agenda-view-scheduled-today
           ,alc-org-agenda-view-deadlines
           ,alc-org-agenda-view-waiting
           ,alc-org-agenda-view-inbox)
          ;; Global settings
          nil
          ;; Files
          nil))

  (setq alc-org-agenda-custom-command-events-month
        `(;; Key and desc
          "v" "Events this month"
          ;; Views
          (,alc-org-agenda-view-events-month)
          ;; Global settings
          nil
          ;; Files
          nil))

  ;; Wrapping up
  (setq org-agenda-custom-commands
        `(;; Daily digest
          ,alc-org-agenda-custom-command-daily-digest
          ;; Events this month
          ,alc-org-agenda-custom-command-events-month))

;; ** Babel

  (setq org-babel-clojure-backend 'cider)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (lisp . t)
     (plantuml . t)
     (python . t)
     (shell . t)
     (clojure . t))))

;; * Org-related packages

;; ** Contrib

(use-package org-contrib
  ;; Unmaintained add-ons for Org-mode.
  ;; https://elpa.nongnu.org/nongnu/org-contrib.html
  :ensure t
  :pin nongnu)

;; TODO useful?
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-expiry))

(use-package org-expiry
  ;; Expiry mechanism for Org entries.
  ;; https://git.sr.ht/~bzg/org-contrib/tree/master/item/lisp/org-expiry.el
  :after org
  :demand t  ; no autoloads
  :disabled
  :config
  ;; Track heading creation in a PROPERTIES drawer. It also works for TODO
  ;; headings since `org-insert-todo-heading' calls `org-insert-heading'.
  ;;
  ;; `org-expiry-insinuate' would do this for an editing session (and a few more
  ;; things). I don't really use it that way.
  (defadvice org-insert-heading
      (after alc-org-insert-heading-created-advice activate)
    (org-expiry-insert-created))
  (ad-activate 'org-insert-heading)

  ;; Do the same for org-capture since `org-insert-heading' is not called
  ;; explicitely.
  (add-hook 'org-capture-before-finalize-hook #'org-expiry-insert-created)

  ;; Don't show these timestamps in the agenda.
  (setq org-expiry-inactive-timestamps t))

(use-package org-inlinetask
  ;; Tasks independent of outline hierarchy.
  ;; https://git.savannah.gnu.org/cgit/emacs/org-mode.git/tree/lisp/org-inlinetask.el
  :init
  (put 'org-inlinetask-min-level 'safe-local-variable #'numberp)
  :commands org-inlinetask-insert-task
  :bind (:map org-mode-map ("C-c C-x t" . org-inlinetask-insert-task)))

(use-package org-crypt
  ;; Public Key Encryption for Org Entries.
  ;; https://git.savannah.gnu.org/cgit/emacs/org-mode.git/tree/lisp/org-crypt.el
  :bind (("C-c z" . org-decrypt-entry))
  :config
  (org-crypt-use-before-save-magic)
  (add-to-list 'org-tags-exclude-from-inheritance "crypt")
  (setq org-crypt-key "F62FE7A4"))

;; ** Others

(use-package org-roam
  ;; Org-roam
  ;; https://github.com/org-roam/org-roam
  ;; https://www.orgroam.com/manual.html
  :ensure t
  :after org
  :init
  (setq org-roam-v2-ack t)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle))))
  :config
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:50}" 'face 'org-tag)))
  (org-roam-setup))

(use-package org-roam-ui
  ;; A graphical frontend for exploring your org-roam Zettelkasten.
  ;; https://github.com/org-roam/org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-ql
  ;; This package provides a query language for Org files.
  ;; https://github.com/alphapapa/org-ql
  :ensure t)

(use-package outline
  ;; Outline mode is a major mode derived from Text mode, which is specialized
  ;; for editing outlines.
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Outline-Mode.html
  :delight outline-minor-mode)

(use-package outshine
  ;; Org-mode for non-Org buffers.
  ;; https://github.com/alphapapa/outshine
  :ensure t
  :delight
  :config (setq outshine-use-speed-commands t)
  :hook (emacs-lisp-mode . outshine-mode))

(alc-with-system gnu/linux
  (use-package org-bullets
    ;; UTF-8 bullets for org-mode.
    ;; https://github.com/integral-dw/org-bullets
    :ensure t
    :hook (org-mode . (lambda () (org-bullets-mode 1)))))

(use-package org-sticky-header
  ;; Show off-screen Org heading at top of window.
  ;; https://github.com/alphapapa/org-sticky-header
  :ensure t
  :disabled
  :config
  (setq org-sticky-header-full-path 'full))

(use-package toc-org
  ;; `toc-org' is an Emacs utility to have an up-to-date table of contents in
  ;; the org files without exporting (useful primarily for readme files on
  ;; GitHub).
  ;; https://github.com/snosov1/toc-org
  :ensure t
  :hook (org-mode . toc-org-enable))

(use-package org-tree-slide
  ;; A presentation tool for org-mode based on the visibility of outline trees.
  ;; https://github.com/takaxp/org-tree-slide
  :ensure t
  :custom
  (org-tree-slide-activate-message "Presentation started...")
  (org-tree-slide-deactivate-message "Presentation finished.")
  :init
  (defun alc-org-tree-slide-setup ()
    (hide-mode-line-mode 1)
    (org-display-inline-images)
    (let ((text-scale-mode-amount 2))
      (text-scale-mode 1)))

  (defun alc-org-tree-slide-end ()
    (hide-mode-line-mode 0)
    (text-scale-mode 0))

  (add-hook 'org-tree-slide-play-hook 'alc-org-tree-slide-setup)
  (add-hook 'org-tree-slide-stop-hook 'alc-org-tree-slide-end)
  :bind (:map org-tree-slide-mode-map
              ("<f9>" . org-tree-slide-move-previous-tree)
              ("<f10>" . org-tree-slide-move-next-tree)))

(use-package demo-it
  ;; An Emacs package for running demonstrations, screencasts and presentations
  ;; from within Emacs.
  ;; https://github.com/howardabrams/demo-it
  :ensure t)

(use-package org-edna
  ;; Edna provides an extensible means of specifying conditions which must be
  ;; fulfilled before a task can be completed and actions to take once it is.
  ;; https://savannah.nongnu.org/projects/org-edna-el/
  :ensure t
  :after org
  :config
  (org-edna-mode))

(use-package org-transclusion
  ;; Org-transclusion lets you insert a copy of text content via a file link or
  ;; ID link within an Org file.
  ;; https://github.com/nobiot/org-transclusion
  :ensure t)

(use-package orgit
  ;; Support for Org links to Magit buffers
  ;; https://github.com/magit/orgit
  :ensure t)

;; * Wrapping up

(provide 'alc-org)
