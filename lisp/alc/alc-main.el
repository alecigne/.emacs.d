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

(use-package f
  ;; f.el, a library for working with files and directories
  ;; https://github.com/rejeep/f.el
  :ensure t
  :demand t)

(use-package esup
  ;; Emacs Start Up Profiler
  ;; https://github.com/jschaf/esup
  :ensure t)

(use-package anaphora
  ;; Anaphoric expressions for Emacs Lisp, providing implicit temporary
  ;; variables.
  ;; https://github.com/rolandwalker/anaphora
  :ensure t
  :after emacs-lisp-mode
  :config
  ;; Fontify `it' and `self'
  (anaphora-install-font-lock-keywords))

;; * Directory structure

;; On Windows, I want the homedir to be `C:\Users\<username>' and not
;; `C:\Users\<username>\AppData\Roaming'.
(setq alc-home-dir
      (if (eq system-type 'windows-nt)
          (getenv "USERPROFILE")
        (getenv "HOME")))

(setq alc-tmp-dir (f-join alc-home-dir "tmp"))

;; * Basics

;; ** Personal info

(defun alc-load-secrets ()
  (interactive)
  (load-library (f-join alc-lisp-dir "secrets.el.gpg")))

;; ** Look & feel

(use-package doom-themes
  ;; A megapack of themes for GNU Emacs, from the Doom Emacs configuration
  ;; framework.
  ;; https://github.com/doomemacs/themes
  :ensure t
  :init
  (load-theme 'doom-dark+ t)
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
       (alc-set-font "JetBrains Mono" 120)))

(setq-default line-spacing nil)

(setq-default cursor-type 'bar)
(global-hl-line-mode 1)
(blink-cursor-mode 0)
(column-number-mode t)
(size-indication-mode t)
(unless (version< emacs-version "29")
  (pixel-scroll-precision-mode))
(delight 'auto-fill-function nil t)

(use-package hide-mode-line
  ;; An Emacs plugin that hides (or masks) the current buffer's mode-line.
  ;; https://github.com/hlissner/emacs-hide-mode-line
  :ensure t)

;; ** Interaction

(use-package help-mode
  ;; The mode used by *Help* buffers.
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Help-Mode.html
  :bind (:map help-mode-map
              ("q" . kill-this-buffer)))

(setq inhibit-startup-screen t
      initial-scratch-message nil
      disabled-command-function nil
      ring-bell-function 'ignore)

(tooltip-mode 0)
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package vertico
  ;; Vertico provides a performant and minimalistic vertical completion UI based
  ;; on the default completion system.
  ;; https://github.com/minad/vertico
  :ensure t
  :init
  (setq vertico-cycle t
        vertico-count 20)
  (vertico-mode))

(use-package marginalia
  ;; Rich annotations in the minibuffer
  ;; https://github.com/minad/vertico
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  ;; Emacs completion style that matches multiple regexps in any order.
  ;; https://github.com/oantolin/orderless
  :ensure t
  :custom (completion-styles '(orderless)))

(use-package consult
  ;; Consult provides practical commands based on the Emacs completion function
  ;; completing-read.
  ;; https://github.com/minad/consult
  :ensure t
  :bind
  (;; C-x
   ("C-x b" . consult-buffer)
   ("C-x M-:" . consult-complex-command)
   ;; Other
   ("M-y" . consult-yank-pop)
   ;; M-g bindings (goto-map)
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ;; M-s bindings (search-map)
   ("M-s d" . consult-find)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s m" . consult-multi-occur)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines))
  :chords ((";b" . consult-buffer))
  :config
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package embark
  ;; Emacs Mini-Buffer Actions Rooted in Keymaps.
  ;; https://github.com/oantolin/embark
  :ensure t
  :bind
  (("C-;" . embark-act)
   ("C-:" . embark-dwim)
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  ;; Consult integration for Embark.
  ;; https://github.com/oantolin/embark/blob/master/embark-consult.el
  :ensure t
  :after (embark consult))

(use-package which-key
  ;; Display available keybindings in popup.
  ;; https://github.com/justbur/emacs-which-key
  :ensure t
  :defer 1
  :delight
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 1.0))

(use-package dashboard
  ;; An extensible emacs startup screen showing you what's most important.
  ;; https://github.com/emacs-dashboard/emacs-dashboard
  :ensure t
  :demand t
  :custom
  (dashboard-navigator-buttons
   `(;; line1
     (("" "Update" "Update emacs"
       (lambda (&rest _) (auto-package-update-now))))))
  (dashboard-items '((recents  . 5)
                     (projects . 5)))
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-page-separator "\n\f\n")
  (dashboard-set-navigator t)
  :config
  (dashboard-setup-startup-hook))

(use-package right-click-context
  ;; Emacs Right Click Context menu.
  ;; https://github.com/zonuexe/right-click-context
  :ensure t
  :defer 10
  :delight
  :config
  (right-click-context-mode))

;; ** Frames, windows and buffers

(setq frame-title-format
      '("[" invocation-name "@" system-name "] "
        (:eval (aif (buffer-file-name) (abbreviate-file-name it) "%b"))))

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
  ;; Popup Window Manager for Emacs.
  ;; https://github.com/emacsorphanage/popwin
  :ensure t
  :defer 1
  :config (popwin-mode 1))

(use-package ibuffer
  ;; IBuffer is a major mode for viewing a list of buffers and operating on them
  ;; in a way analogous to that of Dired.
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Buffer-Menus.html
  :bind (("C-c ù" . (lambda () (interactive) (ibuffer t)))))

(use-package ibuffer-projectile
  ;; Group buffers in ibuffer list by projectile project
  ;; https://github.com/purcell/ibuffer-projectile
  :ensure t
  :after projectile
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(setq uniquify-buffer-name-style 'post-forward ; "file.el|dir1" and
                                               ; "file.el|dir2"
      uniquify-ignore-buffers-re "^\\*")

(use-package autorevert
  ;; Revert buffers when files on disk change.
  ;; https://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/autorevert.el
  :delight auto-revert-mode
  :config
  (global-auto-revert-mode t))

(use-package saveplace
  ;; Automatically save place in files.
  ;; https://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/saveplace.el
  :init (save-place-mode))

(setq native-comp-async-report-warnings-errors 'silent)

(use-package default-text-scale
  ;; Easily adjust the font size in all Emacs frames.
  ;; https://github.com/purcell/default-text-scale
  :defer 5
  :ensure t
  :config
  (default-text-scale-mode 1))

;; ** Editing

(global-set-key (kbd "M-à") 'mark-word)
(setq save-interprogram-paste-before-kill t)
(delete-selection-mode)
(show-paren-mode)

(global-set-key (kbd "C-S-k") 'crux-kill-whole-line)

(use-package aggressive-fill-paragraph
  ;; An emacs minor-mode for keeping paragraphs filled (in both comments and
  ;; prose).
  ;; https://github.com/davidshepherd7/aggressive-fill-paragraph-mode
  :ensure t)

(defun alc-unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
logical line. This is useful, e.g., for use with
`visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(use-package move-text
  ;; Move current line or region up or down.
  ;; https://github.com/emacsfodder/move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package undo-tree
  ;; Treat undo history as a tree.
  ;; https://gitlab.com/tsc25/undo-tree
  :ensure t
  :defer 5
  :delight
  :init
  (setq undo-tree-history-directory-alist `(("." . ,alc-tmp-dir)))
  :config
  (global-undo-tree-mode))

(use-package expand-region
  ;; Emacs extension to increase selected region by semantic units.
  ;; https://github.com/magnars/expand-region.el
  :ensure t
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

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package goto-addr
  ;; Highlights URLs and turns them into clickable links.
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Goto-Address-mode.html
  :custom
  (goto-address-url-face 'underline)
  :hook ((prog-mode . goto-address-prog-mode)
         ;; Do no not activate in Org mode since it already handles links
         (text-mode . (lambda ()
                        (unless (derived-mode-p 'org-mode)
                          (goto-address-mode)))))
  :bind (:map goto-address-highlight-keymap
              ("C-c C-o" . goto-address-at-point)))

(use-package page-break-lines
  ;; This Emacs library provides a global mode which displays ugly form feed
  ;; characters as tidy horizontal rules.
  ;; https://github.com/purcell/page-break-lines
  :ensure t
  :delight
  :hook (dashboard-mode . page-break-lines-mode))

(use-package multiple-cursors
  ;; Multiple cursors for Emacs.
  ;; https://github.com/magnars/multiple-cursors.el
  :ensure t
  :bind (("<M-S-mouse-1>" . 'mc/add-cursor-on-click)))

(use-package visual-fill-column
  ;; Emacs mode for wrapping visual-line-mode buffers at fill-column.
  ;; https://codeberg.org/joostkremers/visual-fill-column
  :ensure t)

;; *** Spelling

(setq ispell-program-name "aspell")

;; *** Completion

(use-package company
  ;; Modular in-buffer completion framework for Emacs
  ;; https://github.com/company-mode/company-mode
  :ensure t
  :defer 2
  :delight
  :config
  (global-company-mode))

(use-package yasnippet
  ;; A template system for Emacs.
  ;; https://github.com/joaotavora/yasnippet
  :ensure t
  :delight yas-minor-mode
  :defer 5
  :config
  (use-package yasnippet-snippets
    ;; A collection of yasnippet snippets for many languages.
    ;; https://github.com/AndreaCrotti/yasnippet-snippets
    :ensure t)
  (yas-global-mode 1))

;; ** Navigating

(setq isearch-allow-scroll 'unlimited)

;; ** Backup

(let ((backup-dir (f-join alc-tmp-dir "emacs-backup")))
  (setq backup-directory-alist `(("." . ,backup-dir))))

(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; ** File/project management

(use-package projectile
  ;; Project Interaction Library for Emacs.
  ;; https://github.com/bbatsov/projectile
  :ensure t
  :defer 1
  :bind-keymap
  ("H-p" . projectile-command-map)
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-track-known-projects-automatically nil)
  :config
  (projectile-mode +1)
  (setq projectile-mode-line-function
        (lambda () (format " π[%s]" (projectile-project-name)))))

(use-package perspective
  ;; The Perspective package provides multiple named workspaces (or
  ;; "perspectives") in Emacs, similar to multiple desktops in window managers
  ;; like Awesome and XMonad, and Spaces on the Mac.
  ;; https://github.com/nex3/perspective-el
  :ensure t
  :bind (("C-x k" . persp-kill-buffer*))
  :custom (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode))

(use-package persp-projectile
  ;; Perspective integration with Projectile
  ;; https://github.com/bbatsov/persp-projectile
  :ensure t
  :after (perspective projectile)
  :config
  (defun alc-move-buffer-to-project-perspective ()
    "Move a buffer to its project perspective."
    (interactive)
    (when (and (projectile-project-p)
               (not (string= (projectile-project-name) (persp-current-name))))
      (let ((source-buffer (buffer-name)))
        (persp-switch (projectile-project-name))
        (persp-set-buffer source-buffer)
        (persp-switch-to-buffer* source-buffer)))))

(use-package treemacs
  ;; Treemacs is a file and project explorer.
  ;; https://github.com/Alexander-Miller/treemacs
  :ensure t
  :config
  ;; (treemacs-follow-mode t)
  ;; (treemacs-filewatch-mode t)
  ;; (treemacs-fringe-indicator-mode t)
  :bind
  (:map global-map ("C-x t t"   . treemacs)))

(use-package treemacs-icons-dired
  ;; Treemacs icons for dired.
  ;; https://github.com/Alexander-Miller/treemacs/blob/master/src/extra/treemacs-icons-dired.el
  :ensure t
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-all-the-icons
  ;; `all-the-icons' integration for treemacs.
  ;; https://github.com/Alexander-Miller/treemacs/blob/master/src/extra/treemacs-all-the-icons.el
  :ensure t
  :after treemacs
  :config (treemacs-load-theme 'all-the-icons))

(use-package recentf
  ;; Setup a menu of recently opened files.
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/File-Conveniences.html
  :config
  (setq recentf-max-saved-items 50))

(use-package hl-todo
  ;; Highlight TODO keywords
  ;; https://github.com/tarsius/hl-todo
  :ensure t)

;; ** Help

;; Jump to directly to function/variable
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)

(use-package eldoc
  ;; Show function arglist or variable docstring in echo area.
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Doc.html
  :delight)

(use-package company-quickhelp
  ;; Documentation popup for Company.
  ;; https://github.com/company-mode/company-quickhelp
  :ensure t
  :hook (company-mode . company-quickhelp-mode))

;; ** Privacy

(use-package password-store
  ;; Password store (pass) support.
  ;; https://www.passwordstore.org/
  :ensure t)

(use-package auth-source
  ;; Authentication sources for Emacs.
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Authentication.html
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
  ;; A Collection of Ridiculously Useful eXtensions for Emacs. crux bundles many
  ;; useful interactive commands to enhance your overall Emacs experience.
  ;; https://github.com/bbatsov/crux
  :ensure t
  :bind
  (("H-SPC f d" . crux-delete-file-and-buffer)
   ("C-c d" . crux-duplicate-current-line-or-region))
  :chords
  (("jh" . crux-switch-to-previous-buffer)))

(defun alc-insert-iso-date (arg)
  (interactive "P")
  (let ((date (if arg (org-read-date) (format-time-string "%Y-%m-%d"))))
    (insert date)))

(defun alc-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (format "Delete file %s?" filename))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer))
          (message "Leaving file %s alone." filename))
      (message "Not a file visiting buffer!"))))

;; * Dired

(use-package dired
  ;; Directory-browsing commands.
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html
  :init
  (define-prefix-command 'alc-dired-hacks-map)
  :bind
  (:map dired-mode-map ("h" . alc-dired-hacks-map))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-listing-switches "-lha"))

(use-package dired+
  ;; Extensions to Dired.
  ;; https://www.emacswiki.org/emacs/DiredPlus
  ;; https://github.com/emacsmirror/dired-plus
  ;; This package is available on Emacs Wiki and mirrored on GitHub by the
  ;; Emacsmirror project.
  :after dired
  :vc (dired+
       :url "https://github.com/emacsmirror/dired-plus"
       :main-file "dired+.el"
       :branch "master"
       :rev :newest)
  :config
  (diredp-toggle-find-file-reuse-dir 1))

(use-package all-the-icons-dired
  ;; Add dired support to all-the-icons.
  ;; https://github.com/wyuenho/all-the-icons-dired
  :ensure t
  :delight
  :hook (dired-mode . all-the-icons-dired-mode))

;; ** Dired hacks
;; https://github.com/Fuco1/dired-hacks

(use-package dired-narrow
  ;; Live-narrowing of search results for dired.
  ;; https://github.com/Fuco1/dired-hacks/blob/master/dired-narrow.el
  :ensure t
  :after dired
  :bind (:map alc-dired-hacks-map
              ("n" . dired-narrow)
              ("f" . dired-narrow-fuzzy)
              ("r" . dired-narrow-regexp)))

(use-package dired-collapse
  ;; Collapse unique nested paths in dired listing
  ;; https://github.com/Fuco1/dired-hacks/blob/master/dired-collapse.el
  :ensure t
  :after dired
  :hook (dired-mode . dired-collapse-mode))

;; * Search

(use-package rg
  ;;  Emacs search tool based on ripgrep.
  ;; https://github.com/dajva/rg.el
  :ensure t)

;; * Markup

(use-package markdown-mode
  ;; Emacs Markdown Mode.
  ;; https://github.com/jrblevin/markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  ;; The emacs major mode for editing files in the YAML data serialization
  ;; format.
  ;; https://github.com/yoshiki/yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; * Programming

;; ** Common

(setq-default indent-tabs-mode nil)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package rainbow-mode
  ;; Colorize color names in buffers.
  ;; https://elpa.gnu.org/packages/rainbow-mode.html
  :ensure t
  :delight
  :hook prog-mode)

(use-package magit
  ;; An interface to the version control system Git, implemented as an Emacs
  ;; package.
  ;; https://github.com/magit/magit
  :ensure t
  :delight magit-status-mode "magit"
  :bind (("C-c g" . magit-status)))

(use-package git-modes
  ;; Emacs major modes for Git configuration files
  ;; https://github.com/magit/git-modes
  :ensure t)

(use-package git-gutter
  ;; Emacs port of GitGutter which is Sublime Text Plugin.
  ;; https://github.com/emacsorphanage/git-gutter
  :ensure t
  :hook prog-mode
  :init
  ;; No changes = no gutter
  (setq git-gutter:hide-gutter t)
  :delight)

(use-package git-timemachine
  ;; Step through historic versions of git controlled file.
  ;; https://gitlab.com/pidu/git-timemachine/
  :ensure t)

(use-package git-messenger
  ;; Popup last commit of current line.
  ;; https://github.com/emacsorphanage/git-messenger
  :ensure t)

(use-package restclient
  ;; HTTP REST client tool for emacs
  ;; https://github.com/pashky/restclient.el
  :ensure t
  :mode (("\\.http\\'" . restclient-mode)))

(use-package company-restclient
  ;; Company-mode completion back-end for restclient-mode
  ;; https://github.com/iquiw/company-restclient
  :ensure t
  :after (company restclient)
  :config
  (add-to-list 'company-backends 'company-restclient))

;; ** JavaScript

(setq js-indent-level 2)

;; ** Emacs Lisp

(global-set-key (kbd "C-c e") 'crux-eval-and-replace)
(add-hook 'emacs-lisp-mode-hook 'display-fill-column-indicator-mode)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq-default fill-column 80)))

(use-package buttercup
  ;; Behavior-Driven Emacs Lisp Testing
  ;; https://github.com/jorgenschaefer/emacs-buttercup
  :ensure t)

;; ** Common Lisp

(unless (alc-work-system-p)
  (use-package slime
    ;; The Superior Lisp Interaction Mode for Emacs.
    ;; https://github.com/slime/slime
    :ensure t
    :config
    (when (eq system-type 'gnu/linux)
      (setq slime-contribs '(slime-fancy)
            slime-protocol-version 'ignore
            inferior-lisp-program "sbcl"))

    (defun alc-swank-listening-p ()
      (ignore-errors
        (let ((swank-process
               (open-network-stream "SLIME Lisp Connection Test" nil
                                    "localhost" 4005)))
          (when swank-process (delete-process swank-process) t))))

    (defun alc-swank-autoconnect (&rest args)
      (if (and (not (slime-connected-p))
               (alc-swank-listening-p))
          (ignore-errors (slime-connect "localhost" 4005))))

    (alc-swank-autoconnect)))

;; ** Clojure

(use-package paredit
  ;; Minor mode for editing parentheses.
  ;; http://mumble.net/~campbell/git/paredit.git/
  :ensure t
  :hook (clojure-mode . paredit-mode))

(use-package clojure-mode
  ;; Emacs support for the Clojure(Script) programming language.
  ;; https://github.com/clojure-emacs/clojure-mode
  :ensure t)

(use-package clojure-mode-extra-font-locking
  ;; Extra font-locking for Clojure mode
  ;; https://github.com/clojure-emacs/clojure-mode/blob/master/clojure-mode-extra-font-locking.el
  :ensure t)

(use-package cider
  ;; The Clojure Interactive Development Environment that Rocks for Emacs.
  ;; https://github.com/clojure-emacs/cider
  :ensure t)

(use-package rainbow-delimiters
  ;; Emacs rainbow delimiters mode.
  ;; https://github.com/Fanael/rainbow-delimiters
  :ensure t)

(use-package tagedit
  ;; A collection of paredit-like functions for editing in html-mode.
  ;; https://github.com/magnars/tagedit
  :ensure t)

(use-package clojure-snippets
  ;; Yasnippet's for Clojure
  ;; https://github.com/swannodette/clojure-snippets
  :ensure t)

;; ** Racket

(use-package racket-mode
  ;; Emacs major and minor modes for Racket.
  ;; https://github.com/greghendershott/racket-mode
  :ensure t)

;; ** PlantUML

(use-package plantuml-mode
  ;; A major mode for editing PlantUML sources in Emacs.
  ;; https://github.com/skuro/plantuml-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (setq org-plantuml-jar-path (expand-file-name "bin/plantuml.jar"
                                                alc-home-dir)))

;; ** Groovy

(use-package groovy-mode
  ;; A groovy major mode, grails minor mode, and a groovy inferior mode.
  ;; https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes
  :ensure t)

;; * World

;; ** Communication

(use-package erc
  ;; A modular and extensible IRC client for Emacs.
  ;; https://www.gnu.org/software/emacs/manual/erc.html
  :bind (:map erc-mode-map ("C-c C-x" . nil))
  :config
  (setq erc-track-position-in-mode-line 'before-modes
        erc-track-shorten-names nil
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477")
        erc-hide-list '("JOIN" "PART" "QUIT")
        erc-autojoin-channels-alist '((".*\\.freenode.net" "#emacs"))))

(use-package elfeed
  ;; An Emacs web feeds client.
  ;; https://github.com/skeeto/elfeed
  :ensure t
  :commands elfeed
  :bind (("C-x w" . elfeed)
         :map elfeed-search-mode-map
         ("j" . next-line)
         ("k" . previous-line)
         ("c" . alc-elfeed-clear-filter))
  :config
  (setq elfeed-search-filter "")
  (defun elfeed-search-format-date (date)
    (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time date)))

  (defun alc-elfeed-clear-filter ()
    (interactive)
    (setq elfeed-search-filter "")
    (elfeed-update)))

(alc-with-system-type work
  (use-package slack
    ;; Slack client for emacs.
    ;; https://github.com/yuya373/emacs-slack
    :ensure t
    :commands (slack-start)
    :init
    (setq slack-buffer-emojify t)
    (setq slack-prefer-current-team t)))

;; ** Weather

(use-package wttrin
  ;; Emacs frontend for weather web service wttr.in
  ;; https://github.com/cjennings/emacs-wttrin
  :ensure t
  :commands (wttrin))

;; * Multimedia

(use-package somafm
  ;; A simple somafm interface in emacs.
  ;; https://github.com/alecigne/somafm.el
  :ensure t
  :if (not (eq system-type 'windows-nt))
  :commands (somafm somafm-by-completion)
  ;; I use my own fork since I once contributed to this package. Also an
  ;; illustration of the :vc keyword :)
  :vc (somafm
       :url "https://github.com/alecigne/somafm.el"
       :main-file "somafm.el"
       :branch "dev"
       :rev :newest))

;; * Other tools and gadgets

(use-package calendar
  ;; Emacs provides the functions of a desk calendar, with a diary of planned or
  ;; past events.
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Calendar_002fDiary.html
  :custom
  (calendar-week-start-day 1)
  :init
  ;; Display week number
  ;; https://stackoverflow.com/a/21367291
  (setq calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'font-lock-warning-face)
        calendar-intermonth-header
        (propertize "Wk" 'font-lock-face 'font-lock-keyword-face))
  :config
  (defun alc-open-calendar-at-date (year month day)
    "Open the Emacs calendar and jump to a specific date. YEAR, MONTH,
and DAY specify the date to jump to."
    (calendar)
    (calendar-goto-date (list month day year))))


(use-package ledger-mode
  ;; Emacs Lisp files for interacting with the C++Ledger accounting system.
  ;; https://github.com/ledger/ledger-mode
  :ensure t
  :config
  (setq ledger-default-date-format ledger-iso-date-format))

(use-package google-this
  ;; A set of emacs functions and bindings to google under point.
  ;; https://github.com/Malabarba/emacs-google-this
  :ensure t
  :bind-keymap ("C-x g" . google-this-mode-submap))

(use-package writeroom-mode
  ;; Writeroom-mode: distraction-free writing for Emacs.
  ;; https://github.com/joostkremers/writeroom-mode
  :ensure t
  :bind ("H-<f10>" . writeroom-mode)
  :config
  (setq writeroom-restore-window-config t
        writeroom-width 120))

(use-package emojify
  ;; Display emojis in Emacs.
  ;; https://github.com/iqbalansari/emacs-emojify
  :ensure t)

(use-package fireplace
  ;; A cozy fireplace for Emacs.
  ;; https://github.com/johanvts/emacs-fireplace
  :ensure t)

(use-package lorem-ipsum
  ;; Add lorem ipsum filler text to Emacs.
  ;; https://github.com/jschaf/emacs-lorem-ipsum
  :ensure t)

(use-package all-the-icons
  ;; A utility package to collect various Icon Fonts and propertize them within
  ;; Emacs.
  ;; https://github.com/domtronn/all-the-icons.el
  :ensure t)

(use-package snow
  ;; Let it snow in Emacs!
  ;; https://github.com/alphapapa/snow.el
  :ensure t)

;; * Wrapping up

(provide 'alc-main)
