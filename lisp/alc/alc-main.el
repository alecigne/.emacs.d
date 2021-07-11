;; * Meta
;; Helpers for the init procress itself.

(defun alc-read-system-type ()
  "Read system type from an environment variable.
Possible values are 'personal (by defaut) or 'work."
  (let ((system-type (getenv "ALC_SYSTEM_TYPE")))
    (if system-type (intern system-type) 'personal)))

(setq alc-system-type (alc-read-system-type))

(defmacro alc-with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(defmacro alc-with-system-type (type &rest body)
  "Evaluate BODY if `alc-system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq alc-system-type ',type)
     ,@body))

(defun alc-work-system-p () (eq alc-system-type 'work))
(defun alc-personal-system-p () (eq alc-system-type 'personal))

;; f.el, a library for working with files and directories
;; https://github.com/rejeep/f.el
(use-package f
  :demand t)

;; Emacs Start Up Profiler
;; https://github.com/jschaf/esup
(use-package esup)

;; * Personal directory structure

(setq alc-root-dir (getenv "ALC_DATA_DIR")
      alc-org-dir (f-join alc-root-dir "org")
      alc-tmp-dir (f-join alc-root-dir "tmp")
      alc-backup-dir (f-join alc-tmp-dir "emacs-backup"))

(let ((dir-list (list alc-org-dir alc-tmp-dir alc-backup-dir)))
  (dolist (dir dir-list)
    (unless (file-exists-p dir)
      (make-directory dir t))))

(setq alc-org-todo-file (f-join alc-org-dir "todo.org")
      alc-org-inbox-file (f-join alc-org-dir "inbox.org")
      alc-org-almanac-file (f-join alc-org-dir "almanac.org"))

(alc-with-system-type personal
  (setq alc-org-entourage-file (f-join alc-org-dir "entourage.org")))

;; * Basics

;; ** Look & feel

(use-package doom-themes
  :init
  (load-theme 'doom-one t)
  :config
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(defun alc-set-font (font height)
  "Use FONT if installed, with height HEIGHT."
  (when (member font (font-family-list))
    (set-face-attribute 'default nil :font font :height height)))

(cond ((eq system-type 'windows-nt)
       (alc-set-font "Consolas" 100))
      ((eq system-type 'gnu/linux)
       (alc-set-font "Source Code Pro" 105)))

(setq-default cursor-type 'bar)
(global-hl-line-mode 1)
(blink-cursor-mode 0)
(column-number-mode t)
(size-indication-mode t)

(delight 'auto-fill-function nil t)

;; ** Interaction

(setq inhibit-startup-screen t
      initial-scratch-message nil
      disabled-command-function nil)

(tooltip-mode 0)

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package helm
  :delight
  :defer 1
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("M-y" . helm-show-kill-ring)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :chords (";b" . helm-mini)
  :config
  (setq helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t
        helm-scroll-amount 8
        helm-ff-file-name-history-use-recentf t
        helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-follow-mode-persistent t)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  ;; Generic Helm completion (e.g., variables, etc.)
  (helm-mode 1))

(use-package helm-descbinds
  :after helm
  :config
  (helm-descbinds-mode)
  (setq-default helm-descbinds-window-style 'split-window))

(use-package helm-describe-modes
  :after helm
  :bind ([remap describe-mode] . helm-describe-modes))

(use-package which-key
  :defer 1
  :delight
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 1.0))

;; ** Frames, windows and buffers

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; current buffer and window

(global-set-key (kbd "<f5>") 'kill-this-buffer)
(global-set-key (kbd "M-<f5>") 'kill-buffer-and-window)
(global-set-key (kbd "C-<f5>") 'delete-window)
(key-chord-define-global ";à" 'delete-window)        ; ";0"
(key-chord-define-global ";&" 'delete-other-windows) ; ";1"
(key-chord-define-global ";é" 'split-window-below)   ; ";2"
(key-chord-define-global ";\"" 'split-window-right)  ; ";3"

;; other buffer and window

(key-chord-define-global ";o" 'other-window)

(defun alc-kill-buffer-in-other-window ()
  "Kill the buffer in the other window."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window 1))

(global-set-key (kbd "<f6>") 'alc-kill-buffer-in-other-window)

(defun alc-kill-other-buffer-and-window (count)
  "Switch to the other window, and kill it, with the associated buffer."
  (interactive "p")
  (other-window count)
  (kill-buffer-and-window))

(global-set-key (kbd "M-<f6>") 'alc-kill-other-buffer-and-window)

(defun alc-kill-other-window ()
  "Kill the other window but don't kill its buffer."
  (interactive)
  (other-window 1)
  (delete-window))

(global-set-key (kbd "C-<f6>") 'alc-kill-other-window)

(use-package popwin
  :defer 1
  :config (popwin-mode 1))

(use-package ibuffer
  :ensure nil
  :bind (("C-c ù" . (lambda () (interactive) (ibuffer t)))))

;; organize ibuffer by Projectile projects
(use-package ibuffer-projectile
  :after projectile
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(setq uniquify-buffer-name-style 'post-forward ; "file.el|dir1" and "file.el|dir2"
      uniquify-ignore-buffers-re "^\\*")

;; ** Editing

(global-set-key (kbd "M-à") 'mark-word)
(setq save-interprogram-paste-before-kill t)
(delete-selection-mode)
(show-paren-mode)

;; see also `crux-kill-whole-line'
(global-set-key (kbd "C-S-k") 'kill-whole-line)

(use-package aggressive-fill-paragraph)

(defun alc-unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single logical
line. This is useful, e.g., for use with `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(use-package move-text
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package undo-tree
  :defer 5
  :delight
  :config
  (global-undo-tree-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(add-hook 'text-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (setq default-justification 'left)
            (setq fill-column 70)))

;; Encoding
(setq system-time-locale "fr_FR.UTF-8"
      ;; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
      utf-translate-cjk-mode nil
      locale-coding-system 'utf-8)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))

(prefer-coding-system 'utf-8)

;; *** Completion

(use-package company
  :defer 2
  :delight
  :config
  (global-company-mode))

(use-package yasnippet
  :delight yas-minor-mode
  :defer 5
  :config
  (yas-global-mode 1))

;; ** Navigating

(setq isearch-allow-scroll 'unlimited)

;; ** Backup

(setq backup-directory-alist `(("." . ,alc-backup-dir)))

(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; ** File/project management

(use-package projectile
  :ensure t
  :defer 1
  :bind-keymap
  ("H-p" . projectile-command-map)
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-mode-line-function
        (lambda () (format " π[%s]" (projectile-project-name))))
  (use-package helm-projectile
    :init
    ;; override projectile functionalities with their helm-projectile
    ;; equivalent
    (helm-projectile-on)))

(use-package treemacs
  :config
  ;; (treemacs-follow-mode t)
  ;; (treemacs-filewatch-mode t)
  ;; (treemacs-fringe-indicator-mode t)
  :bind
  (:map global-map ("C-x t t"   . treemacs)))

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-all-the-icons
  :after treemacs
  :config (treemacs-load-theme 'all-the-icons))

(use-package recentf
  :ensure nil
  :config
  (setq recentf-max-saved-items 50))

;; ** Help

;; Jump to directly to function/variable
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)

(use-package eldoc
  :ensure nil
  :delight)

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

;; ** Privacy

(use-package password-store)

(use-package auth-source
  :ensure nil
  :config
  (setq auth-sources '(password-store))
  (auth-source-pass-enable))

(setq epa-pinentry-mode 'loopback)

;; ** Misc

(defun alc-find-user-init-file ()
  "Edit the `user-init-file'."
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "H-i") #'alc-find-user-init-file)

(defun alc-insert-week-number (&optional date)
  "Insert current week number. With a prefix argument, ask for
the date DATE."
  (interactive (list (when current-prefix-arg
                       (read-from-minibuffer "Date: "))))
  (insert
   (format-time-string "%V" (if date (date-to-time (concat date "T00:00:00"))
                              (current-time)))))

(global-set-key (kbd "H-SPC w") #'alc-insert-week-number)

(use-package crux
  :bind
  (("H-SPC f d" . crux-delete-file-and-buffer)
   ("C-c d" . crux-duplicate-current-line-or-region))
  :chords
  (("jh" . crux-switch-to-previous-buffer)))

;; * Dired

(use-package dired
  :ensure nil
  :init
  (define-prefix-command 'alc-dired-hacks-map)
  :bind
  (:map dired-mode-map ("h" . alc-dired-hacks-map))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-listing-switches "-lha"))

(use-package dired+
  :ensure nil
  :load-path "~/.emacs.d/lisp/dired+/"
  :after dired
  :config
  (diredp-toggle-find-file-reuse-dir 1))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; ** Dired hacks - https://github.com/Fuco1/dired-hacks

(use-package dired-narrow
  :after dired
  :bind (:map alc-dired-hacks-map
              ("n" . dired-narrow)
              ("f" . dired-narrow-fuzzy)
              ("r" . dired-narrow-regexp)))

(use-package dired-collapse
  :after dired
  :hook (dired-mode . dired-collapse-mode))

;; * Markup

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; * Programming

;; ** Common

(setq-default indent-tabs-mode nil)

(use-package rainbow-mode
  :delight
  :hook prog-mode)

(use-package autorevert
  :ensure nil
  :delight auto-revert-mode)

(use-package magit
  :delight magit-status-mode "magit"
  :bind (("C-c g" . magit-status)))

(use-package gitignore-mode
  :mode ".gitignore$")

(use-package gitconfig-mode
  :mode ".gitconfig$")

(use-package git-gutter
  :hook prog-mode
  :init
  ;; No changes = no gutter
  (setq git-gutter:hide-gutter t)
  :delight)

(use-package git-timemachine)
(use-package git-messenger)

;; ** Emacs Lisp

(global-set-key (kbd "C-c e") 'crux-eval-and-replace)

;; unit testing for Emacs Lisp
(use-package buttercup)

;; ** Common Lisp

(unless (alc-work-system-p)
  (use-package slime
    :config
    (when (eq system-type 'gnu/linux)
      (setq slime-contribs '(slime-fancy)
            slime-protocol-version 'ignore
            inferior-lisp-program "sbcl"))

    (defun alc-swank-listening-p ()
      (ignore-errors
        (let ((p (open-network-stream "SLIME Lisp Connection Test" nil "localhost" 4005)))
          (when p (delete-process p) t))))

    (defun alc-swank-autoconnect (&rest args)
      (if (and (not (slime-connected-p))
               (alc-swank-listening-p))
          (ignore-errors (slime-connect "localhost" 4005))))

    (alc-swank-autoconnect)))

;; ** PlantUML

(use-package plantuml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (setq org-plantuml-jar-path (expand-file-name "bin/plantuml.jar"
                                                alc-root-dir)))

;; * World

;; ** Communication

(use-package erc
  :ensure nil
  :bind (:map erc-mode-map ("C-c C-x" . nil))
  :config
  (setq erc-track-position-in-mode-line 'before-modes
        erc-track-shorten-names nil
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477")
        erc-hide-list '("JOIN" "PART" "QUIT")
        erc-autojoin-channels-alist '((".*\\.freenode.net" "#emacs"))))

(use-package elfeed
  :commands elfeed
  :bind (("C-x w" . elfeed)
         :map elfeed-search-mode-map
         ("j" . next-line)
         ("k" . previous-line)
         ("c" . alc-elfeed-clear-filter))
  :config
  (defun elfeed-search-format-date (date)
    (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time date)))

  (defun alc-elfeed-clear-filter ()
    (interactive)
    (setq elfeed-search-filter "")
    (elfeed-update))

  (setq elfeed-feeds
        '("https://news.ycombinator.com/rss"
          ("https://www.reddit.com/r/emacs/.rss" emacs)
          ("http://sachachua.com/blog/category/emacs-news/feed" emacs)
          ("http://endlessparentheses.com/atom.xml" emacs)
          ("http://www.masteringemacs.org/feed/" emacs)
          ("http://emacs-fu.blogspot.com/feeds/posts/default" emacs)
          ("http://emacsredux.com/atom.xml" emacs)
          ("http://emacshorrors.com/feed.atom" emacs)
          ("http://pragmaticemacs.com/feed/" emacs)
          ("https://xkcd.com/atom.xml")
          ("http://kitchingroup.cheme.cmu.edu/blog/category/emacs/feed/" emacs))
        elfeed-search-filter ""))

(alc-with-system-type work
  (use-package slack
    :commands (slack-start)
    :init
    (setq slack-buffer-emojify t)
    (setq slack-prefer-current-team t)))

;; ** Weather

(use-package wttrin
  :commands (wttrin))

;; * Multimedia

(eval-and-compile
  (defun alc-get-somafm-load-path ()
    (expand-file-name "lisp/somafm" user-emacs-directory)))

(use-package somafm
  :ensure nil
  :commands (somafm somafm-by-completion)
  :load-path (lambda () (list (alc-get-somafm-load-path)))
  :config
  (setq somafm-favorites-file "/media/veracrypt1/org/notes/drone_zone.org"))

;; * Other tools and gadgets

(use-package ledger-mode
  :config
  (setq ledger-default-date-format ledger-iso-date-format))

(use-package google-this
  :bind-keymap ("C-x g" . google-this-mode-submap))

(use-package writeroom-mode
  :bind ("H-<f10>" . writeroom-mode)
  :config
  (setq writeroom-restore-window-config t
        writeroom-width 120))

(use-package emojify)
(use-package fireplace)
(use-package lorem-ipsum)
(use-package all-the-icons)

;; * Wrapping up

(provide 'alc-main)
