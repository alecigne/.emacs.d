;; * Loading `package.el'

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq package-quickstart t
      package-archives '(("gnu" .   "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities '(("melpa" . 10)
                                   ("gnu"     . 5)))
(package-initialize)

;; * Bootstrapping `use-package'

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-defer t)

(use-package auto-package-update
  ;; Automatically update Emacs packages.
  ;; https://github.com/rranelli/auto-package-update.el
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))

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
