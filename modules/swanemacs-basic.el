(use-package apropospriate-theme
  :ensure t)

(use-package monokai-theme
  :ensure t)

(load-theme 'monokai t)

(setq-default mode-line-buffer-identification
              '("%Z %b %1*%1+ %I"))

(setq-default mode-line-position
              '((-3 "%p")
                " %4l:"
                (:eval (propertize "%3c" 'face
                                   (if (>= (current-column) 80)
                                       '(:foreground "red"))))))

(setq-default mode-line-modes
              '("%["
                mode-name
                (:eval (format-mode-line minor-mode-alist))
                "%n"
                "%]"))

(setq mode-line-separator " | ")

;; Wrapping up
(setq-default mode-line-format
              '(" "
                mode-line-buffer-identification
                mode-line-separator
                mode-line-position
                mode-line-separator
                mode-line-modes
                mode-line-separator
                (global-mode-string global-mode-string)
                ))

(delight '(;; Major modes
           (text-mode "txt" :major)
           ;; Minor modes
           (eldoc-mode nil t)
           (auto-fill-function nil t)))

(use-package autorevert
  :delight auto-revert-mode)

(setq inhibit-startup-screen t
      initial-scratch-message nil)

(setq-default cursor-type 'bar)
(global-hl-line-mode 1)
(blink-cursor-mode 0)

(scroll-bar-mode 0)
(menu-bar-mode 0)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
(fringe-mode '(2 . 0))

(defun swanemacs-basic-set-font (font height)
  "Use FONT if installed, with height HEIGHT."
  (when (member font (font-family-list))
    (set-face-attribute 'default nil :font font :height height)))

(cond ((eq system-type 'windows-nt)
       (swanemacs-basic-set-font "Consolas" 100))
      ;; Source Code Pro doesn't look good (for me) on Windows
      ((eq system-type 'gnu/linux)
       (swanemacs-basic-set-font "Source Code Pro" 100)))

(defalias 'yes-or-no-p 'y-or-n-p)

(tooltip-mode 0)
(setq tooltip-delay 0.5)

(use-package which-key
  :ensure t
  :defer 1
  :delight
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 1.0
        which-key-max-display-columns nil))

(use-package popwin
  :ensure t
  :defer 1				; probably not needed right away
  :config (popwin-mode 1))

(defun swanemacs-basic-kill-other-buffers ()
  "Kill all normal buffers but the current one."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer))
                (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(defun swanemacs-basic-kill-other-buffer-and-window (count)
  "Switch to the other window, and kill it, with the associated buffer."
  (interactive "p")
  (other-window count)
  (kill-buffer-and-window))

(defun swanemacs-basic-kill-buffer-in-other-window ()
  "Kill the buffer in the other window."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window 1))

(defun swanemacs-basic-kill-other-window ()
  "Kill the other window but don't kill its buffer."
  (interactive)
  (other-window 1)
  (delete-window))

(defun swanemacs-basic-switch-to-previous-buffer ()
  "Switch to the most recently selected buffer other than current
buffer, unless the previous buffer is visible."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) nil)))

(setq uniquify-buffer-name-style 'post-forward)

(defadvice quit-window (before quit-window-always-kill)
  "When running `quit-window', always kill the buffer."
  (ad-set-arg 0 t))

(ad-activate 'quit-window)

(desktop-save-mode 0)
(setq desktop-save 'ask)

(use-package ibuffer
  :config
  (setq ibuffer-default-sorting-mode 'major-mode)
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("Dired"
                  (mode . dired-mode))
                 ("Shell"
                  (or (mode . eshell-mode)
                      (mode . shell-mode)))
                 ("Org" ;; all org-related buffers
                  (mode . org-mode)
                  )
                 ("Markdown"
                  (mode . markdown-mode))
                 ("TeX"
                  (mode . latex-mode))              
                 ("Text"
                  (mode . text-mode))
                 ("R"
                  (mode . r-mode))
                 ("PDF"
                  (name . ".*\.pdf$"))
                 ("Data files"
                  (name . ".*\.csv$"))
                 ("Common Lisp"
                  (or (mode . lisp-mode)
                      (mode . slime-repl-mode)
                      ))
                 ("Emacs Lisp"
                  (or (mode . inferior-emacs-lisp-mode)
                      (mode . lisp-interaction-mode)
                      (mode . emacs-lisp-mode)))
                 ("Python"
                  (mode . python-mode))
                 ("R"
                  (or (mode . inferior-ess-mode)
                      (mode . ess-mode)))
                 ("Calc"
                  (mode . calc-mode))
                 ("Web"
                  (mode . eww-mode))
                 ("Planning"
                  (or
                   (name . "^\\*Calendar\\*$")
                   (name . "^\\*Org Agenda\\*$")))
                 ("Jabber"
                  (or
                   (mode . jabber-roster-mode)
                   (mode . jabber-chat-mode)))
                 ("IRC"
                  (mode . erc-mode))
                 ("ELPA"
                  (mode . package-menu-mode))
                 ))))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              ;;(ibuffer-auto-mode 1)   ;auto update the buffer-list
              (ibuffer-switch-to-saved-filter-groups "default")
              ))

  ;; Don't show (filter) groups that are empty.
  (setq ibuffer-show-empty-filter-groups nil))

(setq isearch-allow-scroll t)

(defun swanemacs-basic-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun swanemacs-basic-unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
logical line. This is useful, e.g., for use with
`visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(defun swanemacs-basic-duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  (setq buffer-undo-list (cons (point) buffer-undo-list)) ; save the point for undo
  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion
      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))
      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count))))
      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))) ; end-of-let
  ;; put the point in the lowest line and return
  (next-line arg))

(setq system-time-locale "fr_FR.UTF-8"
      ;; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
      utf-translate-cjk-mode nil
      locale-coding-system 'utf-8
      ;; Save clipboard strings into kill ring before replacing them. This
      ;; saves you the burden of losing data because you killed something in
      ;; Emacs before pasting it.
      save-interprogram-paste-before-kill t)

(set-language-environment "UTF-8")
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))

(prefer-coding-system 'utf-8)

(use-package undo-tree
  :ensure t
  :demand
  :delight
  :config
  (global-undo-tree-mode))

(show-paren-mode 1)
(put 'upcase-region 'disabled nil)

(add-hook 'text-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (setq default-justification 'left)
            (setq fill-column 70)))

(setq backup-by-copying t       ; don't clobber symlinks
      delete-old-versions t     ; delete excess backup files silently
      kept-new-versions 6       ; newest versions to keep when a new
                                ; numbered backup is made
      kept-old-versions 2       ; oldest versions to keep when a new
                                ; numbered backup is made
      version-control t)        ; version numbers for backup files

(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
        treemacs-deferred-git-apply-delay   0.5
        treemacs-file-event-delay           5000
        treemacs-file-follow-delay          0.2
        treemacs-follow-after-init          t
        treemacs-follow-recenter-distance   0.1
        treemacs-goto-tag-strategy          'refetch-index
        treemacs-indentation                2
        treemacs-indentation-string         " "
        treemacs-is-never-other-window      nil
        treemacs-no-png-images              nil
        treemacs-project-follow-cleanup     nil
        treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow  nil
        treemacs-show-hidden-files          t
        treemacs-silent-filewatch           nil
        treemacs-silent-refresh             nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-space-between-root-nodes   t
        treemacs-tag-follow-cleanup         t
        treemacs-tag-follow-delay           1.5
        treemacs-width                      35)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)

  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'extended))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  :bind
  (:map global-map
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package recentf
  :config
  (setq recentf-max-saved-items 50))

(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)

(provide 'swanemacs-basic)