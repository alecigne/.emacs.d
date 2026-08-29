;;; alc-experimental.el --- Experimental and in-progress configuration -*- lexical-binding: t; -*-

;; Author: Anthony Le Cigne

;;; Commentary:

;; This file is an incubator for experimental and in-progress configuration,
;; i.e. new ideas and packages.
;;
;; Anything here is subject to frequent change, removal, or promotion to the
;; core or tools modules.

;;; Code:

;; * Small tweaks

;; * Avy

(use-package avy
  :ensure t
  :bind (("M-j" . avy-goto-char-timer)
         ;; ("C-:"   . avy-goto-char)
         ;; ("C-,"   . avy-goto-char-2)
         ;; ("M-g w" . avy-goto-word-1)
         ("M-g M-g" . avy-goto-line)
         )
  :custom
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (avy-background t)
  (avy-style 'de-bruijn)
  :config
  (setq avy-all-windows 'all-frames)
  (setq avy-all-windows-alt t)
  (setq avy-dispatch-alist
        '((?m . avy-action-mark)
          (?c . avy-action-copy)
          (?y . avy-action-yank)
          (?k . avy-action-kill-move)
          (?K . avy-action-kill-stay)
          (?t . avy-action-teleport)
          (?w . avy-action-ispell)
          (?z . avy-action-zap-to-char))))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?m))
  (aw-background t))

;; * Pulsar

(use-package pulsar
  ;; Emacs package to pulse the current line after running select functions.
  ;; https://github.com/protesilaos/pulsar
  :ensure t
  :disabled t
  :init
  (pulsar-global-mode 1)
  :custom
  (pulsar-pulse-functions
   '(;; Consult
     consult-line
     ;; Outline
     outline-backward-same-level
     outline-forward-same-level
     outline-next-visible-heading
     outline-previous-visible-heading
     outline-up-heading))
  :bind ("<f8>" . pulsar-pulse-line)
  :config
  (with-eval-after-load 'consult
    (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
    (add-hook 'consult-after-jump-hook #'pulsar-pulse-line)))

;; * tab-bar-mode

;; The idea here is to have a tab bar in every frame (I usually have one). I
;; will use tab bars as workspaces for projects, and this would replace
;; perspective (which I don't use...). Work in progress!

(use-package tab-bar
  ;; The Tab Bar is a row of tabs—buttons that you can click to switch between
  ;; window configurations.
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Tab-Bars.html
  :preface
  (defun alc-tab-name ()
    "Tab name = project name when available, otherwise the default tab name."
    (let* ((proj (cond
                  ((fboundp 'projectile-project-name)
                   (projectile-project-name))
                  ((fboundp 'project-current)
                   (when-let ((p (project-current nil)))
                     (file-name-nondirectory
                      (directory-file-name (project-root p)))))
                  (t nil))))
      (if (or (null proj) (string-empty-p proj) (string= proj "-"))
          (tab-bar-tab-name-current)
        proj)))
  :custom
  (tab-bar-border 2)
  (tab-bar-tab-name-function #'alc-tab-name)
  ;; TODO Check if dashboard is available!
  (tab-bar-new-tab-choice "*dashboard*")
  :init
  (tab-bar-mode))

;; * consult-projectile

(use-package consult-projectile
  ;; A package to incorporate projectile into consult.
  ;; https://gitlab.com/OlMon/consult-projectile
  :ensure t)

;; * org-roam-latte

(use-package org-roam-latte
  ;; Org-roam Latte is a minor mode that automatically highlights unlinked
  ;; references to existing
  ;; https://github.com/yad-tahir/org-roam-latte
  :after org-roam
  :ensure t)

;; * jinja2-mode

(use-package jinja2-mode
  ;; Jinja2 mode for Emacs.
  ;; https://github.com/paradoxxxzero/jinja2-mode
  :ensure t)

;; * Denote

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (:prefix-map denote-prefix-map
   :prefix-docstring "Prefix map for Denote."
   :prefix "C-c o"
   ("n" . denote)
   ("r" . denote-rename-file)
   ("l" . denote-link)
   ("b" . denote-backlinks))
  :config
  (setq denote-file-type 'org)
  (setq denote-prompts '(title))
  (denote-rename-buffer-mode 1))

(use-package consult-denote
  :ensure t
  :after denote consult
  :bind
  (("C-c o f" . consult-denote-find)
   ("C-c o s" . consult-denote-grep))
  :config
  (setq consult-denote-grep-command #'consult-ripgrep)
  (consult-denote-mode 1))

;; * "Consult Org titles"
;; This lists all Org files in a project and display their titles with consult.

;; Quick, fast, dirty. In particular, this doesn't load Org mode in every file
;; (which could be necessary e.g. with org-collect-keywords)
(defun alc-org-project-pages--title (file)
  "Return FILE's Org title, or its base name."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((case-fold-search t))
      (goto-char (point-min))
      (if (re-search-forward "^[ \t]*#\\+title:[ \t]*\\(.*\\)$" nil t)
          (string-trim (match-string-no-properties 1))
        (file-name-base file)))))

(defun alc-org-project-pages--files ()
  "Return absolute paths of .org files in the current project."
  (let* ((project (or (project-current) (user-error "Not inside a project")))
         (root (project-root project)))
    (mapcar (lambda (f)
              (if (file-name-absolute-p f) f (expand-file-name f root)))
            (seq-filter
             (lambda (f) (string-match-p "\\.org\\(?:_archive\\)?\\'" f))
             (project-files project)))))

(defun alc-consult-org-project-page ()
  "Open an Org file from the current project, completing on Org title."
  (interactive)
  (let ((file
         (consult--read
          (mapcar
           (lambda (file)
             (cons (alc-org-project-pages--title file) file))
           (alc-org-project-pages--files))
          :prompt "Org page: "
          :sort nil
          :require-match t
          :lookup #'consult--lookup-cdr
          :category 'file
          :history 'file-name-history)))
    (find-file file)))

;; * PowerOrg views

;; This is an experimental alternative to the agenda views in `alc-org.el'. It
;; deliberately reuses SwanEmacs's Org workflow, agenda files, calendar events,
;; solar-event switch, and agenda finalizers.

;; ** Machinery

(defun alc-powerorg-view-definition (name)
  "Return the resolved PowerOrg view definition named NAME."
  (let* ((definition (copy-sequence
                      (or (alist-get name alc-powerorg-view-definitions)
                          (error "Unknown PowerOrg view: %s" name))))
         (block-function (plist-get definition :block-function)))
    (when block-function
      (setq definition
            (plist-put definition :block (funcall block-function))))
    definition))

(defun alc-powerorg-open-view (name)
  "Open the named PowerOrg view NAME with its configured renderer."
  (let ((definition (alc-powerorg-view-definition name)))
    (pcase (plist-get definition :renderer)
      ('org-ql
       (require 'org-ql)
       (org-ql-search (org-agenda-files)
         (plist-get definition :query)
         :title (plist-get definition :title)
         :sort (plist-get definition :sort)))
      ('agenda
       (let ((org-agenda-custom-commands
              (list (list "P"
                          (plist-get definition :title)
                          (list (plist-get definition :block))))))
         (org-agenda nil "P")))
      (renderer
       (error "Unknown PowerOrg view renderer: %s" renderer)))))

(defun alc-powerorg-view-agenda-block (name)
  "Return an Org agenda block for the PowerOrg view NAME."
  (let ((definition (alc-powerorg-view-definition name)))
    (pcase (plist-get definition :renderer)
      ('org-ql
       `(org-ql-block ',(plist-get definition :query)
                      ((org-ql-block-header
                        ,(plist-get definition :title)))))
      ('agenda (plist-get definition :block))
      (renderer
       (error "Unknown PowerOrg view renderer: %s" renderer)))))

(defun alc-powerorg-open-composite-view (title view-names)
  "Open TITLE as an agenda composed from the named VIEW-NAMES."
  (require 'org-ql)
  (let ((org-agenda-custom-commands
         `(("P" ,title
            ,(mapcar #'alc-powerorg-view-agenda-block view-names)))))
    (org-agenda nil "P")))

;; ** Definitions

(defun alc-powerorg-planning-this-month-block ()
  "Return an agenda block for the current calendar month."
  (let* ((decoded (decode-time))
         (month (nth 4 decoded))
         (year (nth 5 decoded))
         (first-day (format "%04d-%02d-01" year month))
         (days (calendar-last-day-of-month month year)))
    `(agenda ""
             ((org-agenda-overriding-header "Planning this month")
              (org-agenda-span ,days)
              (org-agenda-start-day ,first-day)
              (org-agenda-show-all-dates nil)
              (alc-org-agenda-include-solar nil)))))

(defun alc-powerorg-review-past-week-block ()
  "Return a sparse review block covering the past seven days."
  '(agenda ""
           ((org-agenda-overriding-header "Past seven days")
            (org-agenda-start-day "-6d")
            (org-agenda-span 7)
            (org-agenda-show-all-dates nil)
            (org-agenda-start-with-log-mode t)
            (org-agenda-log-mode-items '(closed state))
            (org-deadline-warning-days 0)
            (alc-org-agenda-include-solar nil))))

(defun alc-powerorg-review-next-four-weeks-block ()
  "Return a sparse review block covering the next four weeks."
  '(agenda ""
           ((org-agenda-overriding-header "Next four weeks")
            (org-agenda-start-day "0d")
            (org-agenda-span 28)
            (org-agenda-show-all-dates nil)
            (org-agenda-entry-types '(:timestamp :sexp :scheduled))
            (alc-org-agenda-include-solar nil))))

(defconst alc-powerorg-view-definitions
  '((review-past-week
     :title "Past seven days"
     :renderer agenda
     :block-function alc-powerorg-review-past-week-block)
    (purchases
     :title "Purchases"
     :renderer org-ql
     :query (and (todo)
                 (property "TYPE" "purchase")))
    (in-progress
     :title "In progress"
     :renderer agenda
     :block (tags-todo "-project/PROG"
                       ((org-agenda-overriding-header "In progress"))))
    (next-actions
     :title "Next actions"
     :renderer agenda
     :block (tags-todo "next/TODO"
                       ((org-agenda-overriding-header "Next actions")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'scheduled)))))
    (current-projects
     :title "Current projects"
     :renderer agenda
     :block (tags-todo "project"
                       ((org-agenda-overriding-header "Current projects"))))
    (waiting
     :title "Waiting for something"
     :renderer agenda
     :block (todo "WAIT"
                  ((org-agenda-overriding-header "Waiting for something"))))
    (on-hold
     :title "On hold"
     :renderer agenda
     :block (todo "HOLD"
                  ((org-agenda-overriding-header "On hold"))))
    (maybe
     :title "Maybe"
     :renderer agenda
     :block (todo "MAYB"
                  ((org-agenda-overriding-header "Maybe"))))
    (inbox
     :title "Inbox"
     :renderer agenda
     :block (tags "inbox"
                  ((org-agenda-overriding-header "Inbox"))))
    (planning-today
     :title "Planning today"
     :renderer agenda
     :block (agenda ""
                    ((org-agenda-overriding-header "Planning today")
                     (org-agenda-span 'day))))
    (events-today
     :title "Events today"
     :renderer agenda
     :block (agenda ""
                    ((org-agenda-overriding-header "Events today")
                     (org-agenda-span 'day)
                     (org-agenda-use-time-grid nil)
                     (org-agenda-entry-types '(:timestamp :sexp))
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if
                        'deadline
                        'todo '("WAIT" "HOLD" "DONE" "GIVN" "CNCL"))))))
    (scheduled-today
     :title "Scheduled today"
     :renderer agenda
     :block (agenda ""
                    ((org-agenda-overriding-header "Scheduled today")
                     (org-agenda-span 'day)
                     (org-agenda-entry-types '(:scheduled))
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if
                        'todo '("WAIT" "HOLD" "DONE" "GIVN" "CNCL"))))))
    (upcoming-deadlines
     :title "Upcoming deadlines"
     :renderer agenda
     :block (agenda ""
                    ((org-agenda-overriding-header "Upcoming deadlines")
                     (org-agenda-format-date "")
                     (org-agenda-span 'day)
                     (org-agenda-entry-types '(:deadline))
                     (org-deadline-warning-days 28)
                     (org-agenda-time-grid nil)
                     (org-agenda-sorting-strategy '(deadline-up))
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if
                        'todo '("DONE" "GIVN" "CNCL"))))))
    (planning-this-month
     :title "Planning this month"
     :renderer agenda
     :block-function alc-powerorg-planning-this-month-block)
    (review-next-four-weeks
     :title "Next four weeks"
     :renderer agenda
     :block-function alc-powerorg-review-next-four-weeks-block))
  "Named experimental PowerOrg views available in SwanEmacs.")

;; ** Commands

(defun alc-powerorg-view-gtd (&optional _match)
  "Show the current sections of the evolving PowerOrg GTD view."
  (interactive)
  (alc-powerorg-open-composite-view
   "Get Things Done"
   '(events-today
     scheduled-today
     in-progress
     next-actions
     upcoming-deadlines
     waiting
     inbox)))

(defun alc-powerorg-view-weekly-review (&optional _match)
  "Review the full task inventory without recording review state."
  (interactive)
  (alc-powerorg-open-composite-view
   "Weekly review"
   '(review-past-week
     inbox
     in-progress
     current-projects
     next-actions
     waiting
     on-hold
     maybe
     upcoming-deadlines
     review-next-four-weeks)))

(defun alc-powerorg-view-purchases (&optional _match)
  "Show unfinished purchase tasks in a dedicated Org QL view."
  (interactive)
  (alc-powerorg-open-view 'purchases))

(defun alc-powerorg-view-current-projects (&optional _match)
  "Show unfinished tasks explicitly tagged as projects."
  (interactive)
  (alc-powerorg-open-view 'current-projects))

(defun alc-powerorg-view-next-actions (&optional _match)
  "Show reviewed tasks selected as possible next actions."
  (interactive)
  (alc-powerorg-open-view 'next-actions))

(defun alc-powerorg-view-waiting (&optional _match)
  "Show tasks waiting for something."
  (interactive)
  (alc-powerorg-open-view 'waiting))

(defun alc-powerorg-view-inbox (&optional _match)
  "Show captured tasks and notes awaiting processing."
  (interactive)
  (alc-powerorg-open-view 'inbox))

(defun alc-powerorg-view-planning-today (&optional _match)
  "Show today's events and planning timestamps in an agenda."
  (interactive)
  (alc-powerorg-open-view 'planning-today))

(defun alc-powerorg-view-events-today (&optional _match)
  "Show today's active events."
  (interactive)
  (alc-powerorg-open-view 'events-today))

(defun alc-powerorg-view-scheduled-today (&optional _match)
  "Show active tasks scheduled for today."
  (interactive)
  (alc-powerorg-open-view 'scheduled-today))

(defun alc-powerorg-view-upcoming-deadlines (&optional _match)
  "Show unfinished tasks with approaching deadlines."
  (interactive)
  (alc-powerorg-open-view 'upcoming-deadlines))

(defun alc-powerorg-view-planning-this-month (&optional _match)
  "Show this month's events and planning timestamps in an agenda."
  (interactive)
  (alc-powerorg-open-view 'planning-this-month))

;; ** Dispatcher

(defconst alc-powerorg-agenda-custom-commands
  '(("p" . "PowerOrg")
    ("pg" "GTD" alc-powerorg-view-gtd "")
    ("pr" "Weekly review" alc-powerorg-view-weekly-review "")
    ("pp" "Purchases" alc-powerorg-view-purchases "")
    ("pc" "Current projects" alc-powerorg-view-current-projects "")
    ("pn" "Next actions" alc-powerorg-view-next-actions "")
    ("pw" "Waiting" alc-powerorg-view-waiting "")
    ("pi" "Inbox" alc-powerorg-view-inbox "")
    ("pt" "Planning today" alc-powerorg-view-planning-today "")
    ("pe" "Events today" alc-powerorg-view-events-today "")
    ("ps" "Scheduled today" alc-powerorg-view-scheduled-today "")
    ("pd" "Upcoming deadlines" alc-powerorg-view-upcoming-deadlines "")
    ("pm" "Planning this month" alc-powerorg-view-planning-this-month ""))
  "PowerOrg entries added to the Org agenda dispatcher.")

(with-eval-after-load 'org-agenda
  (dolist (command alc-powerorg-agenda-custom-commands)
    (add-to-list 'org-agenda-custom-commands command t)))

;; * Wrapping up

(provide 'alc-experimental)
