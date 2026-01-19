;; * Packages-related configuration

;; ** packages.el

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Hide warning about package still requiring 'cl
(setq byte-compile-warnings '(cl-functions))

(setq package-quickstart t
      package-archives '(("gnu" .   "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities '(("melpa" . 10)
                                   ("gnu"     . 5)))
(package-initialize)

;; ** use-package

(use-package use-package
  :custom
  (use-package-always-defer t))

;; Provides a :vc use-package keyword for installing packages from VCS. Will be
;; included in Emacs 30 :)
;; https://github.com/slotThe/vc-use-package
;; TODO Remove this when Emacs 30 will be stable
(when (and (version< emacs-version "30")
           (not (package-installed-p 'vc-use-package)))
  (package-vc-install "https://github.com/slotThe/vc-use-package")
  (require 'vc-use-package))

(use-package delight
  ;; Enables you to customise the mode names displayed in the mode line. Useful
  ;; for use-package's :delight keyword
  ;; https://elpa.gnu.org/packages/delight.html
  :ensure t)

(use-package use-package-chords
  ;; The :chords keyword allows you to define key-chord bindings for use-package
  ;; declarations in the same manner as the :bind keyword.
  ;; https://github.com/jwiegley/use-package/blob/master/use-package-chords.el
  :ensure t
  :demand t
  :config (key-chord-mode 1))

;; ** Misc.

(use-package auto-package-update
  ;; Automatically update Emacs packages.
  ;; https://github.com/rranelli/auto-package-update.el
  :ensure t
  :disabled t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-prompt-before-update t
        auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; * Directories

(use-package f
  ;; f.el, a library for working with files and directories
  ;; https://github.com/rejeep/f.el
  :ensure t
  :demand t)

;; On Windows, I want the homedir to be `C:\Users\<username>' and not
;; `C:\Users\<username>\AppData\Roaming'.
(setq alc-home-dir
      (if (eq system-type 'windows-nt)
          (getenv "USERPROFILE")
        (getenv "HOME")))

(setq alc-tmp-dir (f-join alc-home-dir "tmp"))

(defun alc-load-secrets ()
  "Load GPG-encrypted lisp config."
  (interactive)
  (load-library (f-join alc-my-lisp-dir "alc-secrets.el.gpg")))

(let ((backup-dir (f-join alc-tmp-dir "emacs-backup")))
  (setq backup-directory-alist `(("." . ,backup-dir))))

(defvar alc-lisp-dir (expand-file-name "lisp/" user-emacs-directory)
  "Lisp directory.")

(defvar alc-my-lisp-dir (expand-file-name "alc/" alc-lisp-dir)
  "My personal lisp directory.")

;; * Loading my init code

(add-to-list 'load-path alc-my-lisp-dir)
(require 'alc-core)
(require 'alc-tools)
(require 'alc-org)
(require 'alc-experimental)

;; * Wrapping up

(provide 'alc-init)
