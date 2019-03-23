(when (eq system-type 'gnu/linux)
  (add-to-list 'exec-path "~/.local/bin"))

(unless (and (bound-and-true-p swanemacs-root-dir)
             (bound-and-true-p swanemacs-current-system))
  (error "Missing system info."))

(setq swanemacs-org-main-dir (concat swanemacs-root-dir "org/")
      swanemacs-tmp-dir (concat swanemacs-root-dir "tmp/")
      swanemacs-backup-directory (concat swanemacs-tmp-dir "stk/emacs-backup/"))

(setq swanemacs-website-base-dir (concat swanemacs-root-dir "doc/per/me/Expression/anthony.lecigne.net/")
      swanemacs-website-pub-dir (concat swanemacs-root-dir "pub/anthony.lecigne.net/")
      swanemacs-emacs-config-pub-dir (concat swanemacs-root-dir "pub/emacs-config/"))

(setq swanemacs-all-dir (list swanemacs-org-main-dir
                        swanemacs-tmp-dir
                        swanemacs-backup-directory
                        swanemacs-website-base-dir
                        swanemacs-website-pub-dir
                        swanemacs-emacs-config-pub-dir))

(dolist (dir swanemacs-all-dir)
  (unless (file-exists-p dir)
    (make-directory dir t)))

(setq swanemacs-org-todo-file (concat swanemacs-org-main-dir "todo.org")
      swanemacs-org-note-file (concat swanemacs-org-main-dir "notes.org")
      swanemacs-org-entourage-file (concat swanemacs-org-main-dir "Entourage.org"))

(setq safe-local-variable-values
      '((eval add-hook 'after-save-hook
              (lambda () (org-babel-tangle))
              nil t)))

(put 'org-inlinetask-min-level 'safe-local-variable #'numberp)

(setq projectile-known-projects-file
      (locate-user-emacs-file (concat "projectile/projectile-bookmarks-"
                                      swanemacs-current-system
                                      ".eld")))
(with-eval-after-load 'projectile
  (setq projectile-indexing-method 'native
        projectile-enable-caching t
        projectile-track-known-projects-automatically nil)

  (setq projectile-cache-file
        (locate-user-emacs-file (concat "projectile/projectile-"
                                        swanemacs-current-system
                                        ".cache"))))
