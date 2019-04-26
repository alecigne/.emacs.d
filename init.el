(setq gc-cons-threshold (* 256 1024 1024))
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

(when (version< emacs-version "25.1")
  (error "This config requires GNU Emacs 25.1 or newer."))

(defvar swanemacs-modules-dir nil
  "Emacs modules directory.")

(defvar swanemacs-lisp-dir nil
  "Elisp directory, for packages outside ELPA.")

(defvar swanemacs-personal-dir nil
  "Personal Emacs configuration directory.")

(defvar swanemacs-personal-preload-dir nil
  "Personal Emacs configuration directory - preload.")

(defvar swanemacs-init-org-files nil
  "A regex for detecting Org init files.")

(setq swanemacs-modules-dir (expand-file-name "modules/" user-emacs-directory)
      swanemacs-lisp-dir (expand-file-name "lisp/" user-emacs-directory)
      swanemacs-preload-dir (expand-file-name "preload/" user-emacs-directory)
      swanemacs-init-org-files "^[^#\.].*.org$")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(prefer-coding-system 'utf-8-unix)

(require 'package)
(setq package-enable-at-startup nil)  ; don't initialize twice!

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

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
  (when (and (>= emacs-major-version 23)
             (eq system-type 'windows-nt))
    (defun server-ensure-safe-dir (dir) "Noop" t))
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
(message "Emacs is ready! Loaded in %s. Happy hacking!" (emacs-init-time))
