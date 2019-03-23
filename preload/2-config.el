(when (eq system-type 'gnu/linux)
  (add-to-list 'exec-path "~/.local/bin"))

(unless (and (bound-and-true-p alc-root-dir)
             (bound-and-true-p alc-current-system))
  (error "Missing system info."))

(setq alc-org-main-dir (concat alc-root-dir "org/")
      alc-tmp-dir (concat alc-root-dir "tmp/")
      alc-backup-directory (concat alc-tmp-dir "stk/emacs-backup/"))

(setq alc-website-base-dir (concat alc-root-dir "doc/per/me/Expression/anthony.lecigne.net/")
      alc-website-pub-dir (concat alc-root-dir "pub/anthony.lecigne.net/")
      alc-emacs-config-pub-dir (concat alc-root-dir "pub/emacs-config/"))

(setq alc-all-dir (list alc-org-main-dir
                        alc-tmp-dir
                        alc-backup-directory
                        alc-website-base-dir
                        alc-website-pub-dir
                        alc-emacs-config-pub-dir))

(dolist (dir alc-all-dir)
  (unless (file-exists-p dir)
    (make-directory dir t)))

(setq alc-org-todo-file (concat alc-org-main-dir "todo.org")
      alc-org-note-file (concat alc-org-main-dir "notes.org")
      alc-org-entourage-file (concat alc-org-main-dir "Entourage.org"))

(setq safe-local-variable-values
      '((eval add-hook 'after-save-hook
              (lambda () (org-babel-tangle))
              nil t)))

(put 'org-inlinetask-min-level 'safe-local-variable #'numberp)

(setq projectile-known-projects-file
      (locate-user-emacs-file (concat "projectile/projectile-bookmarks-"
                                      alc-current-system
                                      ".eld")))
(with-eval-after-load 'projectile
  (setq projectile-indexing-method 'native
        projectile-enable-caching t
        projectile-track-known-projects-automatically nil)

  (setq projectile-cache-file
        (locate-user-emacs-file (concat "projectile/projectile-"
                                        alc-current-system
                                        ".cache"))))
