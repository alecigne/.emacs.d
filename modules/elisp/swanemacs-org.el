(use-package org
  :ensure org-plus-contrib
  :delight org-mode "org"
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c f" . org-footnote-new)
         ("C-c c" . org-capture)
         ("<f7>" . org-agenda)
         :map org-mode-map
         ("C-c C-x D" . swanemacs-org-insert-drawer-note)
         ("C-c C" . swanemacs-org-insert-cookie-end-of-heading)
         ("C-c s" . helm-org-in-buffer-headings))
  :config
  (setq org-M-RET-may-split-line t)
  (defun swanemacs-org-insert-drawer-note ()
    (interactive)
    (org-insert-drawer nil "NOTES"))
  (delight '((org-src-mode " org-src" "org-src")
             (org-indent-mode nil "org")))
  
  (setq org-link-frame-setup
        '((vm . vm-visit-folder-other-frame)
          (vm-imap . vm-visit-imap-folder-other-frame)
          (gnus . org-gnus-no-new-news)
          (file . find-file)
          (wl . wl-other-frame)))
  
  (setq org-link-file-path-type 'relative)
  (defun swanemacs-org-replace-link-by-link-description ()
    "Replace an org link by its description or if empty its address"
    (interactive)
    (if (org-in-regexp org-bracket-link-regexp 1)
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description (if (match-end 3) 
                               (org-match-string-no-properties 3)
                             (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description))))
  (setq org-agenda-files
        (delq nil
              (mapcar (lambda (x) (when (file-exists-p x) x))
                      (list swanemacs-org-todo-file swanemacs-org-entourage-file))))
  (setq org-agenda-include-diary nil
        org-agenda-todo-ignore-with-date nil
        org-agenda-skip-scheduled-if-done nil
        org-agenda-skip-deadline-if-done nil
        org-agenda-sorting-strategy '((agenda habit-down time-up category-keep priority-down)
                                      (todo priority-down category-keep)
                                      (tags priority-down category-keep)
                                      (search category-keep))
        org-agenda-start-with-follow-mode nil
        org-agenda-format-date "\n%Y-%m-%d %a\n")
  
  (defun swanemacs-org-place-agenda-tags ()
    "Put the agenda tags by the right border of the agenda window."
    (setq org-agenda-tags-column (- 4 (window-width)))
    (org-agenda-align-tags))
  
  (add-hook 'org-finalize-agenda-hook 'swanemacs-org-place-agenda-tags)
  (setq swanemacs--org-cleaning-tag "cleaning")
  
  (defun swanemacs-org-add-option (view option)
    (list (car view)
          (cadr view)
          (cons option (nth 2 view))))
  
  (setq org-agenda-custom-commands nil)
  
  (defconst swanemacs-org-completed-date-regexp
    (concat "\\("
            "CLOSED: \\[%Y-%m-%d"
            "\\|"
            "- State \"\\(DONE\\|CANCELED\\)\" * from .* \\[%Y-%m-%d"
            "\\|"
            "- State .* ->  *\"\\(DONE\\|CANCELED\\)\" * \\[%Y-%m-%d"
            "\\) ")
    "Matches any completion time stamp.")
  
  ;; Simple views
  
  ;; Events today
  (setq swanemacs-org-acc-events-today
        '(agenda ""
                 ((org-agenda-overriding-header "Events today")
                  (org-agenda-entry-types '(:timestamp :sexp))
                  (org-agenda-span 'day))))
  
  ;; Events this week
  (setq swanemacs-org-acc-events-week
        '(agenda ""
                 ((org-agenda-overriding-header "Events this week")
                  (org-agenda-entry-types '(:timestamp :sexp))
                  (org-agenda-span 'week))))
  
  ;; Events this month
  (setq swanemacs-org-acc-events-month
        '(agenda ""
                 ((org-agenda-overriding-header "Events this month")
                  (org-agenda-entry-types '(:timestamp :sexp))
                  (org-agenda-span 'month))))
  
  ;; Deadlines
  (setq swanemacs-org-acc-deadlines
        '(agenda ""
                 ((org-agenda-overriding-header "Deadlines")
                  (org-agenda-span 'day)
                  (org-agenda-entry-types '(:deadline))
                  (org-deadline-warning-days 365)
                  (org-agenda-time-grid nil)
                  (org-agenda-sorting-strategy '(deadline-up)))))
  
  ;; Scheduled today
  (setq swanemacs-org-acc-scheduled-today
        '(agenda ""
                 ((org-agenda-overriding-header "Scheduled today")
                  (org-agenda-entry-types '(:scheduled))
                  (org-agenda-span 'day)
                  (org-agenda-sorting-strategy
                   '(priority-down time-down))
                  (org-agenda-start-on-weekday nil)
                  (org-agenda-time-grid nil))))
  
  ;; Scheduled this month
  (setq swanemacs-org-acc-scheduled-fortnight
        '(agenda ""
                 ((org-agenda-overriding-header "Scheduled these next 2 weeks")
                  (org-agenda-entry-types '(:scheduled))
                  (org-agenda-span 14)
                  (org-agenda-skip-function
                   (lambda ()
                     (let ((subtree-end (save-excursion (org-end-of-subtree t))))
                       (if (member swanemacs--org-cleaning-tag (org-get-tags-at))
                           subtree-end
                         nil))))
                  (org-agenda-sorting-strategy
                   '(priority-down time-down))
                  (org-agenda-start-on-weekday nil)
                  (org-agenda-time-grid nil))))
  
  ;; Waiting
  (setq swanemacs-org-acc-waiting
        '(todo "WAITING"
               ((org-agenda-overriding-header "Waiting for something\n"))))
  
  ;; Cleaning tasks today
  (setq swanemacs-org-acc-cleaning-today
        '(agenda ""
                 ((org-agenda-overriding-header "Cleaning today")
                  (org-agenda-entry-types '(:scheduled))
                  (org-agenda-span 'day)
                  (org-agenda-skip-function
                   'swanemacs-org-acc-cleaning-today-filter)
                  (org-agenda-sorting-strategy
                   '(priority-down time-down))
                  (org-agenda-start-on-weekday nil)
                  (org-agenda-time-grid nil)
                  (org-agenda-format-date ""))))
  
  (defun swanemacs-org-acc-cleaning-today-filter ()
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member swanemacs--org-cleaning-tag (org-get-tags-at))
          nil		; do no skip
        subtree-end)))	; skip
  
  ;; High priority
  (setq swanemacs-org-acc-high-priority
        '(tags-todo "PRIORITY={A}"
                    ((org-agenda-overriding-header "Important\n"))))
  
  ;; Medium priority
  (setq swanemacs-org-acc-medium-priority
        '(tags-todo "PRIORITY={B}"
                    ((org-agenda-overriding-header "Somewhat important\n")
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'notregexp "\\=.*\\[#B\\]"
                                                 'timestamp)))))
  
  ;; Low priority
  (setq swanemacs-org-acc-low-priority
        '(tags-todo "PRIORITY={C}"
                    ((org-agenda-overriding-header "Not important\n"))))
  
  ;; No priority
  (setq swanemacs-org-acc-no-priority
        '(todo ""
               ((org-agenda-overriding-header "No priority\n")
                (org-agenda-skip-function
                 '(org-agenda-skip-entry-if 'regexp "\\=.*\\[#[A-D]\\]"
                                            'todo '("TOCOMPLETE" "COMPLETING"))))))
  
  ;; Tasks in the inbox
  (setq swanemacs-org-acc-inbox
        '(tags-todo "inbox"
                    ((org-agenda-overriding-header "Tasks in the inbox\n"))))
  
  ;; Block views
  
  ;; Daily digest
  (setq swanemacs-org-acc-block-today
        `((,swanemacs-org-acc-events-today
           ,(swanemacs-org-add-option
             swanemacs-org-acc-scheduled-today
             '(org-agenda-skip-function 'swanemacs-org-acc-scheduled-today-filter))
           ,swanemacs-org-acc-inbox
           ,swanemacs-org-acc-deadlines
           ,swanemacs-org-acc-waiting)
          ((org-agenda-format-date ""))))
  
  (defun swanemacs-org-acc-scheduled-today-filter ()
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (or (member swanemacs--org-cleaning-tag (org-get-tags-at))
              (member (org-get-todo-state) '("WAITING" "HOLD" "DONE" "CANCELED")))
          subtree-end	; skip
        nil)))		; don't skip
  
  ;; No timestamp (by priority)
  (setq swanemacs-org-acc-block-priority
        `((,swanemacs-org-acc-high-priority
           ,swanemacs-org-acc-medium-priority
           ,swanemacs-org-acc-low-priority
           ,swanemacs-org-acc-no-priority)
          ((org-agenda-skip-function
            '(org-agenda-skip-entry-if 'timestamp)))))
  
  ;; Wrapping up
  (setq org-agenda-custom-commands
        `(;; Daily digest
          ("d" "To[d]ay" ,@swanemacs-org-acc-block-today)
          ;; No timestamp
          ("n" "[N]o timestamp" ,@swanemacs-org-acc-block-priority)
          ;; Events
          ("v" . "E[v]ents...")
          ("vt" "Events [t]oday" ,@swanemacs-org-acc-events-today)     
          ("vw" "Events this [w]eek" ,@swanemacs-org-acc-events-week)
          ("vm" "Events this [m]onth" ,@swanemacs-org-acc-events-month)
          ;; Scheduled tasks
          ("h" . "Sc[h]eduled tasks...")
          ("hd" "Scheduled to[d]ay" ,@swanemacs-org-acc-scheduled-today)
          ("hf" "Scheduled for the next fortnight" ,@swanemacs-org-acc-scheduled-fortnight)
          ;; Cleaning
          ("c" "[C]leaning" ,@swanemacs-org-acc-cleaning-today)))
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
  (setq org-todo-keywords
        '((sequence "TODO(t!)"
                    "DOING(D!)"
                    "WAITING(w@/!)"
                    "HOLD(h@/!)"
                    "TODO?(m!)"
                    "|"
                    "DONE(d!)"
                    "CANCELED(x@)")
          (sequence "TOCOMPLETE(T!)"
                    "COMPLETING(C!)"
                    "TOCOMPLETE?(M!)"
                    "|"
                    "COMPLETED(c!)"
                    "ABORTED(X@)")))
  (defface swanemacs-org-todo-kwd
    '((t (:weight bold :foreground "red")))
    "Face used to display tasks yet to be worked on.")
  
  (defface swanemacs-org-in-progress-kwd
    '((t (:weight bold :foreground "orange")))
    "Face used to display tasks in progress.")
  
  (defface swanemacs-org-someday-kwd
    '((t (:weight bold :foreground "dark red")))
    "Face used to display tasks that might be done someday.")
  
  (defface swanemacs-org-done-kwd
    '((t (:weight bold :foreground "forest green")))
    "Face used to display org state DONE.")
  
  (setq org-todo-keyword-faces
        '(("TODO" . swanemacs-org-todo-kwd)
          ("TOCOMPLETE" . swanemacs-org-todo-kwd)
          ("TODO?" . swanemacs-org-someday-kwd)
          ("TOCOMPLETE?" . swanemacs-org-someday-kwd)
          ("DOING" . swanemacs-org-in-progress-kwd)
          ("COMPLETING" . swanemacs-org-in-progress-kwd)
          ("WAITING" . swanemacs-org-in-progress-kwd)
          ("HOLD" . swanemacs-org-in-progress-kwd)
          ("DONE" . swanemacs-org-done-kwd)
          ("COMPLETED" . swanemacs-org-done-kwd)
          ("CANCELED" . swanemacs-org-done-kwd)
          ("ABORTED" . swanemacs-org-done-kwd)))
  (setq org-provide-todo-statistics '("TODO" "DOING" "WAITING" "TODO?"))
  (setq org-hierarchical-todo-statistics nil)
  (setq org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies nil
        org-agenda-dim-blocked-tasks t)
  (setq org-log-into-drawer t)
  (setq org-lowest-priority ?C)
  (setq org-deadline-warning-days 14)
  (setq org-capture-templates
        '(;; Tâches
          ("t" "Nouvelle tâche"
           entry
           (file+olp swanemacs-org-todo-file "Todo" "Inbox")
           "* TODO %?"
           :prepend t :kill-buffer t)
          ))
  (setq org-archive-location "%s_archive::")
  
  (defadvice org-archive-subtree
      (before add-inherited-tags-before-org-archive-subtree activate)
    "add inherited tags before org-archive-subtree"
    (org-set-tags-to (org-get-tags-at)))
  (setq org-hide-emphasis-markers nil)
  (setq org-ascii-links-to-notes t
        org-html-validation-link nil)
  (setq org-icalendar-use-deadline nil
        org-icalendar-use-scheduled nil
        org-icalendar-include-body nil)
  ;; (with-eval-after-load 'org
  ;;   (set-face-attribute 'org-meta-line nil :height 0.7 :slant 'normal))
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (lisp . t)
     (plantuml . t)
     (python . t)
     (R . t)
     (ruby . t)
     (shell . t)))
  (setq org-use-speed-commands t
        org-speed-commands-user '(("a" org-archive-subtree)))
  (setq org-startup-indented t)
  (delight 'org-indent-mode nil "org-indent")
  (defun swanemacs-org-insert-cookie-end-of-heading ()
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
            (swanemacs-org-change-tags-column))
        (progn
          (replace-regexp "\\(.*\\) \\[[0-9]*/[0-9]*\\]\\(.*\\)" "\\1\\2" nil (point) (save-excursion (end-of-line) (point)))
          (swanemacs-org-change-tags-column)))))
  
  (defun swanemacs-org-schedule-if-doing-or-waiting ()
    "Schedule when the task is marked DOING or WAITING, unless the
  item is already scheduled."
    (when (and (or (string= org-state "DOING")
                   (string= org-state "WAITING"))
               (not (string= org-last-state org-state))
               (not (org-get-scheduled-time (point))))
      (org-schedule nil "")))
  
  (add-hook 'org-after-todo-state-change-hook
            'swanemacs-org-schedule-if-doing-or-waiting)
  
  ;; https://emacs.stackexchange.com/a/9588
  (require 'cl-lib)
  (require 'dash)
  
  (defun swanemacs-todo-to-int (todo)
    (cl-first (-non-nil
            (mapcar (lambda (keywords)
                      (let ((todo-seq
                             (-map (lambda (x) (cl-first (split-string  x "(")))
                                   (cl-rest keywords)))) 
                        (cl-position-if (lambda (x) (string= x todo)) todo-seq)))
                    org-todo-keywords))))
  
  (defun swanemacs-org-sort-key ()
    (let* ((todo-max (apply #'max (mapcar #'length org-todo-keywords)))
           (todo (org-entry-get (point) "TODO"))
           (todo-int (if todo (swanemacs-todo-to-int todo) todo-max))
           (priority (org-entry-get (point) "PRIORITY"))
           (priority-int (if priority (string-to-char priority) org-default-priority)))
      (format "%03d %03d" todo-int priority-int)
      ))
  
  (defun swanemacs-org-sort-entries ()
    (interactive)
    (org-sort-entries nil ?f #'swanemacs-org-sort-key)))

(use-package org-inlinetask
  :ensure nil
  :commands org-inlinetask-insert-task
  :bind (:map org-mode-map
              ("C-c C-x t" . org-inlinetask-insert-task))
  :after org)

(use-package org-bullets
  :after org
  :init
  (when (eq system-type 'gnu/linux)
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

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

(provide 'swanemacs-org)
