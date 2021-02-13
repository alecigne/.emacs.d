(setq swanemacs-root-dir "/home/alc/")

(with-eval-after-load 'swanemacs-basic
  ;; (load-theme 'doom-one t)
  (swanemacs-basic-set-font "Source Code Pro" 105))

;; (setq url-proxy-services '(("http" . "localhost:3128")
;;                            ("https" . "localhost:3128")))

(when (eq system-type 'gnu/linux)
  (add-to-list 'exec-path "~/.local/bin"))

(unless (bound-and-true-p swanemacs-root-dir)
  (error "Missing system info."))

(setq swanemacs-org-dir (concat swanemacs-root-dir "org/")
      swanemacs-tmp-dir (concat swanemacs-root-dir "tmp/")
      swanemacs-backup-dir (concat swanemacs-tmp-dir "emacs-backup/"))

(let ((dir-list (list swanemacs-org-dir
                      swanemacs-tmp-dir
                      swanemacs-backup-dir)))
  (dolist (dir dir-list)
    (unless (file-exists-p dir)
      (make-directory dir t))))

(setq swanemacs-org-todo-file (concat swanemacs-org-dir "todo.org")
      swanemacs-org-entourage-file (concat swanemacs-org-dir "entourage.org"))

(setq safe-local-variable-values
      '((eval add-hook 'after-save-hook
              (lambda () (org-babel-tangle))
              nil t)))

(put 'org-inlinetask-min-level 'safe-local-variable #'numberp)

(setq projectile-known-projects-file
      (locate-user-emacs-file "projectile/bookmarks.eld"))

(with-eval-after-load 'projectile
  (setq projectile-indexing-method 'native
	projectile-enable-caching t
	projectile-track-known-projects-automatically nil)

  (setq projectile-cache-file
	(locate-user-emacs-file "projectile/projectile.cache")))
