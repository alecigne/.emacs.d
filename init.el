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

(setq use-package-always-defer t)

;; Provides a :vc use-package keyword for installing packages from VCS. Will be
;; included in Emacs 30 :)
;; https://github.com/slotThe/vc-use-package
(if (and (version< emacs-version "30")
         (not (package-installed-p 'vc-use-package)))
  (package-vc-install "https://github.com/slotThe/vc-use-package")
  (require 'vc-use-package))

;; For use-package's :delight keyword
(use-package delight
  ;; Enables you to customise the mode names displayed in the mode line.
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

;; For use-package's :chords keyword
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

;; * Loading my init code

(defvar alc-lisp-dir (expand-file-name "lisp/" user-emacs-directory)
  "Lisp directory.")

(defvar alc-my-lisp-dir (expand-file-name "alc/" alc-lisp-dir)
  "My personal lisp directory.")

(add-to-list 'load-path alc-my-lisp-dir)
(require 'alc-main)
(require 'alc-org)

;; * Wrapping up

(provide 'alc-init)
