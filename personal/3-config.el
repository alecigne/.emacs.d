(when (eq system-type 'gnu/linux)
  (add-to-list 'exec-path "~/.local/bin"))

(unless (and (bound-and-true-p alc-root-dir)
             (bound-and-true-p alc-current-system))
  (error "Missing system info."))

(setq alc-org-main-dir (concat alc-root-dir "org/")
      alc-tmp-dir (concat alc-root-dir "tmp/")
      alc-backup-directory (concat alc-tmp-dir "stk/emacs-backup/"))

(setq alc-website-base-dir (concat alc-root-dir "doc/per/me/Expression/anthony.lecigne.net/")
      alc-website-pub-dir (concat alc-root-dir "pub/anthony.lecigne.net/")
      alc-emacs-config-pub-dir (concat alc-root-dir "pub/emacs-config/"))

(setq alc-all-dir (list alc-org-main-dir
                        alc-tmp-dir
                        alc-backup-directory
                        alc-website-base-dir
                        alc-website-pub-dir
                        alc-emacs-config-pub-dir))

(dolist (dir alc-all-dir)
  (unless (file-exists-p dir)
    (make-directory dir t)))

(setq alc-org-todo-file (concat alc-org-main-dir "todo.org")
      alc-org-note-file (concat alc-org-main-dir "notes.org")
      alc-org-entourage-file (concat alc-org-main-dir "Entourage.org"))

(setq safe-local-variable-values
      '((eval add-hook 'after-save-hook
              (lambda () (org-babel-tangle))
              nil t)))

(put 'org-inlinetask-min-level 'safe-local-variable #'numberp)

;; One recentf file for every system I work on. This is not perfect
;; but better than having non-existent files from other systems in my
;; recentf list.
(setq recentf-save-file (locate-user-emacs-file
                         (concat "recentf/recentf-" alc-current-system)))

(setq backup-directory-alist `(("." . ,alc-backup-directory)))

(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "<f5>") 'kill-this-buffer)
(key-chord-define-global ";k" 'kill-this-buffer)
(global-set-key (kbd "M-<f5>") 'kill-buffer-and-window)
(define-key global-map (kbd "C-c ù") 'ibuffer)
(global-set-key (kbd "C-<f5>") 'delete-window)
(key-chord-define-global ";o" 'other-window)
(key-chord-define-global ";à" 'delete-window)
(key-chord-define-global ";&" 'delete-other-windows)
(key-chord-define-global ";é" 'split-window-below)
(key-chord-define-global ";\"" 'split-window-right)

(with-eval-after-load "alc-basic"
  (global-set-key (kbd "C-c d") 'alc-basic-duplicate-line)
  (global-set-key (kbd "C-c e") 'alc-basic-eval-and-replace)
  (global-set-key (kbd "<f6>") 'alc-basic-kill-buffer-in-other-window)
  (global-set-key (kbd "M-<f6>") 'alc-basic-kill-other-buffer-and-window)
  (global-set-key (kbd "C-<f6>") 'alc-basic-kill-other-window)
  (key-chord-define-global "jh" 'alc-basic-switch-to-previous-buffer))

(with-eval-after-load 'alc-helm
  (key-chord-define-global ";b" 'helm-mini))

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c s") 'helm-org-in-buffer-headings)
  (global-set-key (kbd "<f7>") 'org-agenda)
  (define-key org-mode-map (kbd "C-c C-x D") 'alc-org-insert-drawer-note)
  (define-key org-mode-map (kbd "C-c C") 'alc-org-insert-cookie-end-of-heading))

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
      '(;; Tâches
        ("t" "Nouvelle tâche"
         entry
         (file+olp alc-org-todo-file "Todo" "Inbox")
         "* TODO %?"
         :prepend t :kill-buffer t)
        ))

(setq org-agenda-files
      (delq nil
            (mapcar (lambda (x) (when (file-exists-p x) x))
                    (list alc-org-todo-file alc-org-entourage-file))))

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

(defun alc-org-place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setq org-agenda-tags-column (- 4 (window-width)))
  (org-agenda-align-tags))

(add-hook 'org-finalize-agenda-hook 'alc-org-place-agenda-tags)

(defun alc-org-add-option (view option)
  (list (car view)
        (cadr view)
        (cons option (nth 2 view))))

(setq org-agenda-custom-commands nil)

(defconst alc-org-completed-date-regexp
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
(setq alc-org-acc-events-today
      '(agenda ""
               ((org-agenda-overriding-header "Events today")
                (org-agenda-entry-types '(:timestamp :sexp))
                (org-agenda-span 'day))))

;; Events this week
(setq alc-org-acc-events-week
      '(agenda ""
               ((org-agenda-overriding-header "Events this week")
                (org-agenda-entry-types '(:timestamp :sexp))
                (org-agenda-span 'week))))

;; Events this month
(setq alc-org-acc-events-month
      '(agenda ""
               ((org-agenda-overriding-header "Events this month")
                (org-agenda-entry-types '(:timestamp :sexp))
                (org-agenda-span 'month))))

;; Deadlines
(setq alc-org-acc-deadlines
      '(agenda ""
               ((org-agenda-overriding-header "Deadlines")
                (org-agenda-span 'day)
                (org-agenda-entry-types '(:deadline))
                (org-deadline-warning-days 365)
                (org-agenda-time-grid nil)
                (org-agenda-sorting-strategy '(deadline-up)))))

;; Scheduled today
(setq alc-org-acc-scheduled-today
      '(agenda ""
               ((org-agenda-overriding-header "Scheduled today")
                (org-agenda-entry-types '(:scheduled))
                (org-agenda-span 'day)
                (org-agenda-sorting-strategy
                 '(priority-down time-down))
                (org-agenda-start-on-weekday nil)
                (org-agenda-time-grid nil))))

;; Scheduled this month
(setq alc-org-acc-scheduled-fortnight
      '(agenda ""
               ((org-agenda-overriding-header "Scheduled these next 2 weeks")
                (org-agenda-entry-types '(:scheduled))
                (org-agenda-span 14)
                (org-agenda-skip-function
                 (lambda ()
                   (let ((subtree-end (save-excursion (org-end-of-subtree t))))
                     (if (member "ménage" (org-get-tags-at))
                         subtree-end
                       nil))))
                (org-agenda-sorting-strategy
                 '(priority-down time-down))
                (org-agenda-start-on-weekday nil)
                (org-agenda-time-grid nil))))

;; Waiting
(setq alc-org-acc-waiting
      '(todo "WAITING"
             ((org-agenda-overriding-header "Waiting for something\n"))))

;; Cleaning tasks today
(setq alc-org-acc-cleaning-today
      '(agenda ""
               ((org-agenda-overriding-header "Cleaning today")
                (org-agenda-entry-types '(:scheduled))
                (org-agenda-span 'day)
                (org-agenda-skip-function
                 'alc-org-acc-cleaning-today-filter)
                (org-agenda-sorting-strategy
                 '(priority-down time-down))
                (org-agenda-start-on-weekday nil)
                (org-agenda-time-grid nil)
                (org-agenda-format-date ""))))

(defun alc-org-acc-cleaning-today-filter ()
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (member "ménage" (org-get-tags-at))
        nil		; do no skip
      subtree-end)))	; skip

;; High priority
(setq alc-org-acc-high-priority
      '(tags-todo "PRIORITY={A}"
                  ((org-agenda-overriding-header "Important\n"))))

;; Medium priority
(setq alc-org-acc-medium-priority
      '(tags-todo "PRIORITY={B}"
                  ((org-agenda-overriding-header "Somewhat important\n")
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp "\\=.*\\[#B\\]"
                                               'timestamp)))))

;; Low priority
(setq alc-org-acc-low-priority
      '(tags-todo "PRIORITY={C}"
                  ((org-agenda-overriding-header "Not important\n"))))

;; No priority
(setq alc-org-acc-no-priority
      '(todo ""
             ((org-agenda-overriding-header "No priority\n")
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'regexp "\\=.*\\[#[A-D]\\]"
                                          'todo '("TOCOMPLETE" "COMPLETING"))))))

;; Tasks in the inbox
(setq alc-org-acc-inbox
      '(tags-todo "inbox"
             ((org-agenda-overriding-header "Tasks in the inbox\n"))))

;; Block views

;; Daily digest
(setq alc-org-acc-block-today
      `((,alc-org-acc-events-today
         ,(alc-org-add-option
           alc-org-acc-scheduled-today
           '(org-agenda-skip-function 'alc-org-acc-scheduled-today-filter))
         ,alc-org-acc-inbox
         ,alc-org-acc-deadlines
         ,alc-org-acc-waiting)
        ((org-agenda-format-date ""))))

(defun alc-org-acc-scheduled-today-filter ()
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (or (member "ménage" (org-get-tags-at))
              (member (org-get-todo-state) '("WAITING" "HOLD" "DONE" "CANCELED")))
          subtree-end	; skip
        nil)))		; don't skip

;; No timestamp (by priority)
(setq alc-org-acc-block-priority
      `((,alc-org-acc-high-priority
         ,alc-org-acc-medium-priority
         ,alc-org-acc-low-priority
         ,alc-org-acc-no-priority)
        ((org-agenda-skip-function
          '(org-agenda-skip-entry-if 'timestamp)))))

;; Wrapping up
(setq org-agenda-custom-commands
      `(;; Daily digest
        ("d" "To[d]ay" ,@alc-org-acc-block-today)
        ;; No timestamp
        ("n" "[N]o timestamp" ,@alc-org-acc-block-priority)
        ;; Events
        ("v" . "E[v]ents...")
        ("vt" "Events [t]oday" ,@alc-org-acc-events-today)     
        ("vw" "Events this [w]eek" ,@alc-org-acc-events-week)
        ("vm" "Events this [m]onth" ,@alc-org-acc-events-month)
        ;; Scheduled tasks
        ("h" . "Sc[h]eduled tasks...")
        ("hd" "Scheduled to[d]ay" ,@alc-org-acc-scheduled-today)
        ("hf" "Scheduled for the next fortnight" ,@alc-org-acc-scheduled-fortnight)
        ;; Cleaning
        ("c" "[C]leaning" ,@alc-org-acc-cleaning-today)))

(setq org-ascii-links-to-notes nil)

(setq org-publish-project-alist
      `(("org-notes"
         :base-directory ,alc-website-base-dir
         :base-extension "org"
         :publishing-directory ,alc-website-pub-dir
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
         :html-preamble alc-org-mode-blog-preamble)
        ("org-static"
         :base-directory ,alc-website-base-dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory ,alc-website-pub-dir
         :recursive t
         :publishing-function org-publish-attachment)
        ("org" :components ("org-notes" "org-static"))
        ("emacs-config"
         :base-directory ,user-emacs-directory
         :base-extension "org"
         :publishing-directory ,alc-emacs-config-pub-dir
         :recursive t
         :publishing-function org-html-publish-to-html
         :exclude "elpa"
         ;; :headline-levels 4
         :auto-preamble t)))

(defun alc-org-mode-blog-preamble (options)
  "The function that creates the preamble top section for the blog.
OPTIONS contains the property list from the org-mode export."
  (let ((base-directory (plist-get options :base-directory)))
    (org-babel-with-temp-filebuffer (expand-file-name "top-bar.html" base-directory) (buffer-string))))

(with-eval-after-load "org-crypt"
  (setq org-crypt-key "F62FE7A4"))

(with-eval-after-load 'slime
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

(with-eval-after-load 'erc
  (define-key erc-mode-map (kbd "C-c C-x") nil)
  (setq erc-autojoin-channels-alist '((".*\\.freenode.net"
                                       "#stumpwm"
                                       "#lisp"
                                       "#emacs"
                                       ))))

(setq projectile-known-projects-file
      (locate-user-emacs-file (concat "projectile/projectile-bookmarks-"
                                      alc-current-system
                                      ".eld")))
(with-eval-after-load 'projectile
  (setq projectile-indexing-method 'native
        projectile-enable-caching t
        projectile-track-known-projects-automatically nil)

  (setq projectile-cache-file
        (locate-user-emacs-file (concat "projectile/projectile-"
                                        alc-current-system
                                        ".cache"))))

(with-eval-after-load "alc-gadgets"
  (define-key global-map (kbd "H-<f10>") 'writeroom-mode))
