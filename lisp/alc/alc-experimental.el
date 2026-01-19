;;; alc-experimental.el --- Experimental and in-progress configuration -*- lexical-binding: t; -*-

;; Author: Anthony Le Cigne

;;; Commentary:

;; This file is an incubator for experimental and in-progress configuration,
;; i.e. new ideas and packages.
;;
;; Anything here is subject to frequent change, removal, or promotion to the
;; core or tools modules.

;;; Code:

;; * Small tweaks

(window-divider-mode)

;; * Avy

(use-package avy
  :ensure t
  :bind (("M-j" . avy-goto-char-timer)
         ;; ("C-:"   . avy-goto-char)
         ;; ("C-,"   . avy-goto-char-2)
         ;; ("M-g w" . avy-goto-word-1)
         ("M-g M-g" . avy-goto-line)
         )
  :custom
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (avy-background t)
  (avy-style 'de-bruijn)
  :config
  (setq avy-all-windows 'all-frames)
  (setq avy-all-windows-alt t)
  (setq avy-dispatch-alist
        '((?m . avy-action-mark)
          (?c . avy-action-copy)
          (?y . avy-action-yank)
          (?k . avy-action-kill-move)
          (?K . avy-action-kill-stay)
          (?t . avy-action-teleport)
          (?w . avy-action-ispell)
          (?z . avy-action-zap-to-char))))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-background t))

;; * Pulsar

(use-package pulsar
  ;; Emacs package to pulse the current line after running select functions.
  ;; https://github.com/protesilaos/pulsar
  :ensure t
  :disabled t
  :init
  (pulsar-global-mode 1)
  :custom
  (pulsar-pulse-functions
   '(;; Consult
     consult-line
     ;; Outline
     outline-backward-same-level
     outline-forward-same-level
     outline-next-visible-heading
     outline-previous-visible-heading
     outline-up-heading))
  :bind ("<f8>" . pulsar-pulse-line)
  :config
  (with-eval-after-load 'consult
    (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
    (add-hook 'consult-after-jump-hook #'pulsar-pulse-line)))

(provide 'alc-experimental)
