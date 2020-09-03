(defvar swanemacs-modules-dir nil
  "Emacs modules directory.")

(defvar swanemacs-lisp-dir nil
  "Elisp directory, for packages outside ELPA.")

(defvar swanemacs-preload-dir nil
  "Personal Emacs configuration directory - preload.")

(defvar swanemacs-init-org-files nil
  "A regex for detecting Org init files.")

(setq swanemacs-modules-dir (expand-file-name "modules/" user-emacs-directory)
      swanemacs-lisp-dir (expand-file-name "lisp/" user-emacs-directory)
      swanemacs-preload-dir (expand-file-name "preload/" user-emacs-directory)
      swanemacs-init-org-files "^[^#\.].*.org$")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(prefer-coding-system 'utf-8-unix)

(setq package-quickstart t)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-defer t
      use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

(use-package delight)

(use-package org
  :ensure org-plus-contrib
  :demand t	; although the code in init.el will autoload it
  :pin "org")

(unless (file-exists-p swanemacs-lisp-dir)
  (make-directory swanemacs-lisp-dir))

(use-package server
  :demand t
  :config
  (when (and (>= emacs-major-version 23) (eq system-type 'windows-nt))
    (defun server-ensure-safe-dir (dir)
      "Noop" t))
  (unless (server-running-p)
    (server-start)))

(use-package use-package-chords
  :demand t
  :config (key-chord-mode 1))

(defvar swanemacs-enabled-modules nil
  "List of enabled modules.")

(setq swanemacs-enabled-modules
      '(swanemacs-basic
        swanemacs-dired
        swanemacs-helm
        swanemacs-completion
        swanemacs-projectile
        swanemacs-git
        swanemacs-org
        swanemacs-prog
        swanemacs-latex
        swanemacs-web
        swanemacs-communication
        swanemacs-gadgets
        swanemacs-mail-news
        swanemacs-science
        swanemacs-markdown
        swanemacs-finance
        ))

(let ((dir swanemacs-preload-dir))
  (when (file-exists-p dir)
    (mapc 'org-babel-load-file (directory-files dir t swanemacs-init-org-files))))

(if (not (file-exists-p swanemacs-modules-dir))
    (error "Modules directory not found!")
  (mapc (lambda (module)
          (let ((path (expand-file-name (concat (symbol-name module) ".org")
                                        swanemacs-modules-dir)))
            (if (not (file-exists-p path))
                (error "%s doesn't exist!" path)
              (org-babel-load-file path))))
        swanemacs-enabled-modules))

(when (file-exists-p custom-file)
  (load custom-file))

(fset 'display-startup-echo-area-message 'ignore)
