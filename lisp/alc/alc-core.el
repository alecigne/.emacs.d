;;; alc-core.el --- Core Emacs configuration -*- lexical-binding: t; -*-

;; Author: Anthony Le Cigne

;;; Commentary:

;; This file contains core configuration shared across all workflows.
;;
;; It defines global UX, interaction infrastructure, and defaults:
;;
;; - UI (theme, fonts, modeline, faces)
;; - Minibuffer and completion stack (Vertico, Orderless, Corfu, etc.)
;; - Global modes and editing behavior
;; - Navigation and interaction primitives
;;
;; Domain-specific modules (languages, Org, applications, tools, etc.) are
;; excluded from the core.

;;; Code:

;; * Functions and commands

(use-package anaphora
  ;; Anaphoric expressions for Emacs Lisp, providing implicit temporary
  ;; variables.
  ;; https://github.com/rolandwalker/anaphora
  :ensure t
  :after emacs-lisp-mode
  :config
  ;; Fontify `it' and `self'
  (anaphora-install-font-lock-keywords))

(use-package crux
  ;; A Collection of Ridiculously Useful eXtensions for Emacs. crux bundles many
  ;; useful interactive commands to enhance your overall Emacs experience.
  ;; https://github.com/bbatsov/crux
  :ensure t
  :commands (;; Buffers
	     crux-delete-file-and-buffer
	     crux-switch-to-previous-buffer
	     ;; Editing
	     crux-duplicate-current-line-or-region
	     crux-kill-whole-line
	     crux-eval-and-replace))

;; * Look & Feel

(defun alc-hex-from-string-palette (s)
  "Turn a string palette from https://coolors.co/ to a list of RGB codes.
Original idea from https://redd.it/rg9ojl."
  (mapcar (lambda (x) (concat "#" x)) (split-string s "-")))

(defconst alc-heading-palette
  "1c97d9-1fe0da-66e182-efbd71-e84a52"
  "A color palette for outline and org-mode headings.")

(use-package doom-themes
  ;; A megapack of themes for GNU Emacs, from the Doom Emacs configuration
  ;; framework.
  ;; https://github.com/doomemacs/themes
  :ensure t
  :demand t
  :config
  (load-theme 'doom-dark+ t)
  (doom-themes-org-config)

  ;; Blue modeline, but not the one set by `doom-dark+-blue-modeline'
  (custom-set-faces
   '(mode-line ((t (:background "#204a87" :foreground "#ffffff"))))
   '(mode-line-inactive ((t (:background "#152f52" :foreground "#888888"))))
   '(font-lock-comment-face ((t (:foreground "#5f9253"))))))

(defun alc-set-font (font height)
  "Use FONT if installed, with height HEIGHT."
  (when (member font (font-family-list))
    (set-face-attribute 'default nil :font font :height height)))

(cond ((eq system-type 'windows-nt)
       (alc-set-font "Consolas" 100))
      ((eq system-type 'gnu/linux)
       (alc-set-font "JetBrains Mono" 120)))

(setq-default cursor-type 'bar)
(blink-cursor-mode -1)
(global-hl-line-mode 1)

(unless (version< emacs-version "29")
  (pixel-scroll-precision-mode))

;; * Startup

(setq inhibit-startup-screen t
      initial-scratch-message nil)

(use-package dashboard
  ;; An extensible emacs startup screen showing you what's most important.
  ;; https://github.com/emacs-dashboard/emacs-dashboard
  :ensure t
  :demand t
  :custom
  (dashboard-items '((recents  . 5)
                     (projects . 5)))
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-page-separator "\n\f\n")
  (dashboard-set-navigator t)
  :config
  (dashboard-setup-startup-hook))

;; * Frames, buffers, windows & files

(setq frame-title-format
      '("[" invocation-name "@" system-name "] " (:eval (buffer-name))))

(setq uniquify-buffer-name-style 'post-forward ; x.el|dir1 and x.el|dir2
      uniquify-ignore-buffers-re "^\\*")

(defun alc-kill-buffer-in-other-window ()
  "Kill the buffer in the other window."
  (interactive)
  (other-window 1)
  (kill-this-buffer)
  (other-window 1))

(defun alc-kill-other-buffer-and-window (count)
  "Switch to the other window, and kill it, with the associated buffer."
  (interactive "p")
  (other-window count)
  (kill-buffer-and-window))

(defun alc-kill-other-window ()
  "Kill the other window but don't kill its buffer."
  (interactive)
  (other-window 1)
  (delete-window))

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
      (message "Not a file-visiting buffer!"))))

(dolist (spec '(("<f5>" . kill-current-buffer)
                ("M-<f5>" . kill-buffer-and-window)
                ("C-<f5>" . delete-window)
                ("C-c <f5>" . alc-delete-file-and-buffer)
		("<f6>" . alc-kill-buffer-in-other-window)
		("M-<f6>" . alc-kill-other-buffer-and-window)
		("C-<f6>" . alc-kill-other-window)))
  (keymap-global-set (car spec) (cdr spec)))

;; This is for an AZERTY keyboard
(dolist (spec '((";à" . delete-window)        ; ";0"
                (";&" . delete-other-windows) ; ";1"
                (";é" . split-window-below)   ; ";2
                (";\"" . split-window-right)  ; ";3"
                (";o" . other-window)
		("jh" . crux-switch-to-previous-buffer)))
  (key-chord-define-global (car spec) (cdr spec)))

;; Backups
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(use-package recentf
  ;; Setup a menu of recently opened files.
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/File-Conveniences.html
  :config
  (setq recentf-max-saved-items 50))

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

(use-package popwin
  ;; Popup Window Manager for Emacs which makes you free from the hell
  ;; of annoying buffers.
  ;; https://github.com/emacsorphanage/popwin
  :ensure t
  :defer 1
  :config (popwin-mode 1))

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

(use-package ibuffer
  ;; IBuffer is a major mode for viewing a list of buffers and operating on them
  ;; in a way analogous to that of Dired.
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Buffer-Menus.html
  :bind (("C-c ù" . (lambda () (interactive) (ibuffer t)))))

;; * Modeline

(size-indication-mode t)
(column-number-mode t)

(use-package hide-mode-line
  ;; An Emacs plugin that hides (or masks) the current buffer's mode-line.
  ;; https://github.com/hlissner/emacs-hide-mode-line
  :ensure t)

;; * Minibuffer

(use-package marginalia
  ;; Rich annotations in the minibuffer
  ;; https://github.com/minad/vertico
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

(use-package savehist
  ;; Toggle saving of minibuffer history.
  :init
  (savehist-mode 1))

;; * Editing

;; ** Basics

(setq save-interprogram-paste-before-kill t)

(dolist (spec '(("C-c d" . crux-duplicate-current-line-or-region)
		("C-S-k" . crux-kill-whole-line)
		("C-c e" . crux-eval-and-replace)))
  (keymap-global-set (car spec) (cdr spec)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(delight 'auto-fill-function nil t)

;; When enabled, typed text replaces the
;; selection if the selection is active. Otherwise, typed text is just
;; inserted at point regardless of any selection.
(delete-selection-mode)

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

(use-package move-text
  ;; Move current line or region up or down.
  ;; https://github.com/emacsfodder/move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package page-break-lines
  ;; This Emacs library provides a global mode which displays ugly form feed
  ;; characters as tidy horizontal rules.
  ;; https://github.com/purcell/page-break-lines
  :ensure t
  :delight
  :hook (dashboard-mode . page-break-lines-mode))

;; ** Structure/Folding

(use-package outline
  ;; Outline mode is a major mode derived from Text mode, which is specialized
  ;; for editing outlines.
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Outline-Mode.html
  :delight outline-minor-mode
  :config
  (let* ((theme (alc-hex-from-string-palette alc-heading-palette))
         (faces '(outline-1 outline-2 outline-3 outline-4 outline-5)))
    (cl-mapc (lambda (face color)
               (set-face-attribute face nil
                                   :foreground color
                                   :height 1.0))
             faces theme)))

(use-package outli
  ;; Simple comment-based outline folding for Emacs.
  ;; https://github.com/jdtsmith/outli
  :ensure t
  :hook ((prog-mode text-mode) . outli-mode)
  :delight outli-mode
  :config
  (dolist (binding
           '(("C-c C-n" . outline-next-visible-heading)
             ("C-c C-p" . outline-previous-visible-heading)
             ("C-c C-u" . outline-up-heading)
             ("C-c C-f" . outline-forward-same-level)
             ("C-c C-b" . outline-backward-same-level)))
    (define-key outli-mode-map (kbd (car binding)) (cdr binding))))

;; ** Region/Selection

(use-package expand-region
  ;; Emacs extension to increase selected region by semantic units.
  ;; https://github.com/magnars/expand-region.el
  :ensure t
  :bind ("C-=" . er/expand-region))

;; ** Undo/Redo

(use-package undo-tree
  ;; Treat undo history as a tree.
  ;; https://gitlab.com/tsc25/undo-tree
  :ensure t
  :defer 5
  :delight
  :init
  (setq undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode))

;; * Completion

(use-package orderless
  ;; Emacs completion style that matches multiple regexps in any order.
  ;; https://github.com/oantolin/orderless
  :ensure t
  :custom (completion-styles '(orderless basic)))

(use-package vertico
  ;; Vertico provides a performant and minimalistic vertical completion UI based
  ;; on the default completion system.
  ;; https://github.com/minad/vertico
  :ensure t
  :init
  (setq vertico-cycle t
        vertico-count 20)
  (vertico-mode))

(use-package corfu
  ;; Corfu enhances in-buffer completion with a small completion popup.
  ;; https://github.com/minad/corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  (keymap-unset corfu-map "RET"))

(use-package kind-icon
  ;; Icons for Corfu.
  ;; https://github.com/jdtsmith/kind-icon
  :ensure t
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  ;; Completion At Point Extensions.
  ;; https://github.com/minad/cape
  :ensure t)

(use-package yasnippet
  ;; A template system for Emacs.
  ;; https://github.com/joaotavora/yasnippet
  :ensure t
  :delight yas-minor-mode
  :commands (yas-expand yas-minor-mode)
  :bind (("C-c y" . yas-expand))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  ;; A collection of yasnippet snippets for many languages.
  ;; https://github.com/AndreaCrotti/yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package consult-yasnippet
  ;; Implements a consulting-read interface for yasnippet.
  ;; https://github.com/mohkale/consult-yasnippet
  :ensure t
  :after yasnippet)

;; * Navigation

;; ** Search/Jump

(setq isearch-allow-scroll 'unlimited)

(use-package consult
  ;; Consult provides practical commands based on the Emacs completion
  ;; function completing-read.
  ;; https://github.com/minad/consult
  :ensure t
  :bind
  (;; C-x
   ("C-x b"     . consult-buffer)
   ;; Yank
   ("M-y"       . consult-yank-pop)
   ;; M-g (goto-map)
   ("M-g e"     . consult-compile-error)
   ("M-g o"     . consult-outline)
   ("M-g m"     . consult-mark)
   ("M-g k"     . consult-global-mark)
   ("M-g i"     . consult-imenu)
   ("M-g I"     . consult-imenu-multi)
   ;; M-g f should run consult-flycheck -- see package's config
   ;; M-s (search-map)
   ("M-s d"     . consult-find)
   ("M-s D"     . consult-locate)
   ("M-s g"     . consult-grep)
   ("M-s G"     . consult-git-grep)
   ("M-s r"     . consult-ripgrep)
   ("M-s l"     . consult-line)
   ("M-s L"     . consult-line-multi)
   ("M-s m"     . consult-multi-occur)
   ("M-s k"     . consult-keep-lines)
   ("M-s u"     . consult-focus-lines))
  :chords ((";b" . consult-buffer)
           (";l" . consult-line))
  :init
  :config
  (setq consult-narrow-key "<")
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-locate consult-find
   consult-recent-file consult-xref
   :preview-key '(:debounce 0.4 any))
  (with-eval-after-load 'projectile
    (setq consult-project-function (lambda (_) (projectile-project-root)))))

(use-package consult-flycheck
  ;; Consult integration for Flycheck.
  ;; https://github.com/minad/consult-flycheck
  :ensure t
  :after (flycheck consult)
  :bind (("M-g f" . consult-flycheck)))

;; ** Workspace

(use-package treemacs
  ;; Treemacs is a file and project explorer.
  ;; https://github.com/Alexander-Miller/treemacs
  :ensure t
  :config
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  :bind
  (:map global-map ("C-x t t"   . treemacs)))

;; * Interaction

(tooltip-mode 0)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq disabled-command-function nil
      ring-bell-function 'ignore)

(use-package embark
  ;; Emacs Mini-Buffer Actions Rooted in Keymaps.
  ;; https://github.com/oantolin/embark
  :ensure t
  :bind
  (("C-;" . embark-act)
   ("C-:" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  ;; Useful for replacing which-key. See:
  ;; https://www.matem.unam.mx/~omar/apropos-emacs.html#the-case-against-which-key-a-polemic
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  ;; Consult integration for Embark.
  ;; https://github.com/oantolin/embark/blob/master/embark-consult.el
  :ensure t
  :after (embark consult))

;; * Privacy

(setq epg-pinentry-mode 'loopback)

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

;; * Help

(use-package help-mode
  ;; The mode used by *Help* buffers.
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Help-Mode.html
  :bind (:map help-mode-map ("q" . kill-current-buffer)))

(use-package eldoc
  ;; Show function arglist or variable docstring in echo area.
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Doc.html
  :delight)

;; Jump to directly to function/variable
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)

;; * Performance/debugging

(use-package esup
  ;; Emacs Start Up Profiler
  ;; https://github.com/jschaf/esup
  :ensure t)

;; * Wrapping up

(provide 'alc-core)
