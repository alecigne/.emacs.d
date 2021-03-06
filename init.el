;; * Loading `package.el'

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq package-quickstart t
      package-archives '(("gnu" .   "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" .   "https://orgmode.org/elpa/"))
      package-archive-priorities '(("melpa" . 10)
                                   ("gnu"     . 5)))
(package-initialize)

;; * Bootstrapping `use-package'

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)

(setq use-package-always-defer t
      use-package-always-ensure t)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))

(use-package delight)

(use-package use-package-chords
  :demand t
  :config (key-chord-mode 1))

;; * Loading my init code

(defvar alc-lisp-dir (expand-file-name "lisp/alc/" user-emacs-directory)
  "My personal lisp directory.")

(add-to-list 'load-path alc-lisp-dir)
(require 'alc-main)
(require 'alc-org)

;; * Wrapping up

(provide 'alc-init)
