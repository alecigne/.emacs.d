(setq gc-cons-threshold (* 256 1024 1024))
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

(when (version< emacs-version "25.1")
  (error "This config requires GNU Emacs 25.1 or newer."))

(defvar alc-modules-dir nil
  "Emacs modules directory.")

(defvar alc-personal-dir nil
  "Personal Emacs configuration directory.")

(defvar alc-lisp-dir nil
  "Elisp directory, for packages outside ELPA.")

(setq alc-modules-dir (expand-file-name "modules/" user-emacs-directory)
      alc-personal-dir (expand-file-name "personal/" user-emacs-directory)
      alc-lisp-dir (expand-file-name "lisp/" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" alc-personal-dir))

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

(setq use-package-always-defer t)

(eval-when-compile
  (require 'use-package))

;; (require 'bind-key)

(use-package delight
  :ensure t)

(use-package org
  :ensure org-plus-contrib
  :demand t	; although the code in init.el will autoload it
  :pin "org")

(unless (file-exists-p alc-lisp-dir)
  (make-directory alc-lisp-dir))

(use-package server
  :demand t
  :config
  (when (and (>= emacs-major-version 23)
             (eq system-type 'windows-nt))
    (defun server-ensure-safe-dir (dir) "Noop" t))
  (unless (server-running-p)
    (server-start)))

(use-package use-package-chords
  :ensure t
  :demand t
  :config (key-chord-mode 1))

(defvar alc-enabled-modules nil
  "List of enabled modules.")

(when (file-exists-p alc-personal-dir)
  (mapc 'org-babel-load-file (directory-files alc-personal-dir t "^[^#\.].*org$")))

(if (not (file-exists-p alc-modules-dir))
    (error "Modules directory not found!")
  (mapc (lambda (module)
	  (let ((path (expand-file-name (concat (symbol-name module) ".org")
					alc-modules-dir)))
	    (if (not (file-exists-p path))
		(error "%s doesn't exist!" path)
	      (org-babel-load-file path))))
	alc-enabled-modules))

(when (file-exists-p custom-file)
  (load custom-file))

(fset 'display-startup-echo-area-message 'ignore)
(message "Emacs is ready! Loaded in %s. Happy hacking!" (emacs-init-time))
