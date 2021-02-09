(use-package helm
  :delight
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("M-y" . helm-show-kill-ring)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :chords (";b" . helm-mini)
  :config
  (setq helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t
        helm-scroll-amount 8
        helm-ff-file-name-history-use-recentf t
        helm-M-x-fuzzy-match t)
  
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)
  
  (setq helm-follow-mode-persistent t)
  (helm-mode 1))

(use-package helm-descbinds
  :after helm
  :config
  (helm-descbinds-mode)
  (setq-default helm-descbinds-window-style 'split-window))

(use-package helm-describe-modes
  :after helm
  :bind ([remap describe-mode] . helm-describe-modes))

(use-package helm-projectile
  :after projectile
  :config
  (helm-projectile-on))

(use-package helm-swoop
  ;; :bind ([remap isearch-forward] . helm-swoop)
  :config
  (setq-default helm-swoop-move-to-line-cycle nil
                helm-swoop-speed-or-color t
                helm-swoop-split-direction 'split-window-vertically)
  (set-face-attribute 'helm-swoop-target-word-face nil
                      :background 'unspecified
                      :foreground 'unspecified
                      :inherit 'isearch))

(provide 'swanemacs-helm)
