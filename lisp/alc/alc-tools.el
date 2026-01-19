;;; alc-tools.el --- Domain-specific tools and integrations -*- lexical-binding: t; -*-

;; Author: Anthony Le Cigne

;;; Commentary:

;; This file contains configuration for domain-specific tools and integrations
;; built on top of the core editor substrate, which is intentionally kept
;; separate.
;;
;; Unlike the core module, everything here is:
;;
;; - Domain-specific, where the "domain" is not "Emacs itself": languages,
;;   workflows, external services, etc.
;; - Optional, in the sense that it can be removed without breaking the editor
;;   "flow" to which I am accustomed.
;;
;; Examples of what belongs here:
;;
;; - Project and VCS tooling (Projectile, Magit)
;; - Language and format modes (Lisp, Markdown, YAML, etc.)
;; - Information and communication tools (Elfeed, calendar, weather)
;; - External integrations (AI tools, network services)
;; - Gadgets, games, etc.

;;; Code:

;; * Project management

;; ** Projectile

(use-package projectile
  ;; Project Interaction Library for Emacs.
  ;; https://github.com/bbatsov/projectile
  :ensure t
  :defer 1
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-track-known-projects-automatically nil)
  :config
  (projectile-mode +1)
  (setq projectile-mode-line-function
        (lambda () (format " π[%s]" (projectile-project-name))))
  (setq projectile-switch-project-action #'projectile-commander))

(use-package ibuffer-projectile
  ;; Group buffers in ibuffer list by projectile project
  ;; https://github.com/purcell/ibuffer-projectile
  :ensure t
  :after projectile
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package persp-projectile
  ;; Perspective integration with Projectile.
  ;; https://github.com/bbatsov/persp-projectile
  :ensure t
  :after (perspective projectile)
  :config
  (defun alc-move-buffer-to-project-perspective ()
    "Move a buffer to its project perspective."
    (interactive)
    (when (and (projectile-project-p)
               (not (string= (projectile-project-name) (persp-current-name))))
      (let ((source-buffer (buffer-name)))
        (persp-switch (projectile-project-name))
        (persp-set-buffer source-buffer)
        (persp-switch-to-buffer* source-buffer)))))

;; ** TODOs

(use-package hl-todo
  ;; Highlight todo keywords
  ;; https://github.com/tarsius/hl-todo
  :ensure t
  :hook (prog-mode yaml-mode)
  :config
  ;; Do not highlight TODO keywords that start with a double quote
  ;; https://github.com/tarsius/hl-todo/issues/64#issuecomment-907615685
  (modify-syntax-entry ?\" "w" hl-todo--syntax-table))

(use-package consult-todo
  ;; Search and jump hl-todo keywords in buffers with consult.
  ;; https://github.com/liuyinz/consult-todo
  :ensure t
  :after (hl-todo consult)
  :bind (("M-s t" . consult-todo)))

;; * Version control

(use-package magit
  ;; An interface to the version control system Git, implemented as an Emacs
  ;; package.
  ;; https://github.com/magit/magit
  :ensure t
  :delight magit-status-mode "magit"
  :bind (("C-c g" . magit-status)))

(use-package git-modes
  ;; Emacs major modes for Git configuration files
  ;; https://github.com/magit/git-modes
  :ensure t)

(use-package git-gutter
  ;; Emacs port of GitGutter which is Sublime Text Plugin.
  ;; https://github.com/emacsorphanage/git-gutter
  :ensure t
  :hook prog-mode
  :init
  ;; No changes = no gutter
  (setq git-gutter:hide-gutter t)
  :delight)

(use-package git-timemachine
  ;; Step through historic versions of git controlled file.
  ;; https://gitlab.com/pidu/git-timemachine/
  :ensure t)

(use-package git-messenger
  ;; Popup last commit of current line.
  ;; https://github.com/emacsorphanage/git-messenger
  :ensure t)

;; * Time

(use-package calendar
  ;; Emacs provides the functions of a desk calendar, with a diary of planned or
  ;; past events.
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Calendar_002fDiary.html
  :custom
  (calendar-week-start-day 1)
  :init
  ;; Display week number
  ;; https://stackoverflow.com/a/21367291
  (setq calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'font-lock-warning-face)
        calendar-intermonth-header
        (propertize "Wk" 'font-lock-face 'font-lock-keyword-face)))

(defun alc-insert-week-number (&optional date)
  "Insert current week number. With a prefix argument, ask for
the date DATE."
  (interactive (list (when current-prefix-arg
                       (read-from-minibuffer "Date: "))))
  (insert
   (format-time-string "%V" (if date (date-to-time (concat date "T00:00:00"))
                              (current-time)))))

(global-set-key (kbd "H-SPC w") #'alc-insert-week-number)

(defun alc-insert-iso-date (arg)
  (interactive "P")
  (let ((date (if arg (org-read-date) (format-time-string "%Y-%m-%d"))))
    (insert date)))

;; * File management

(use-package dired
  ;; Directory-browsing commands.
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html
  :init
  (define-prefix-command 'alc-dired-hacks-map)
  :bind
  (:map dired-mode-map ("h" . alc-dired-hacks-map))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-listing-switches "-lha"))

(use-package dired+
  ;; Extensions to Dired.
  ;; https://www.emacswiki.org/emacs/DiredPlus
  ;; https://github.com/emacsmirror/dired-plus
  ;; This package is available on Emacs Wiki and mirrored on GitHub by the
  ;; Emacsmirror project.
  :after dired
  :vc (dired+
       :url "https://github.com/emacsmirror/dired-plus"
       :main-file "dired+.el"
       :branch "master"
       :rev :newest)
  :config
  (diredp-toggle-find-file-reuse-dir 1))

(use-package all-the-icons-dired
  ;; Add dired support to all-the-icons.
  ;; https://github.com/wyuenho/all-the-icons-dired
  :ensure t
  :delight
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-narrow
  ;; Live-narrowing of search results for dired.
  ;; https://github.com/Fuco1/dired-hacks/blob/master/dired-narrow.el
  :ensure t
  :after dired
  :bind (:map alc-dired-hacks-map
              ("n" . dired-narrow)
              ("f" . dired-narrow-fuzzy)
              ("r" . dired-narrow-regexp)))

(use-package dired-collapse
  ;; Collapse unique nested paths in dired listing
  ;; https://github.com/Fuco1/dired-hacks/blob/master/dired-collapse.el
  :ensure t
  :after dired
  :hook (dired-mode . dired-collapse-mode))

;; * Search tools

(use-package rg
  ;; Emacs search tool based on ripgrep.
  ;; https://github.com/dajva/rg.el
  :ensure t)

;; * Languages & Formats

;; ** Common

(setq-default indent-tabs-mode nil)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package rainbow-mode
  ;; Colorize color names in buffers.
  ;; https://elpa.gnu.org/packages/rainbow-mode.html
  :ensure t
  :delight
  :hook prog-mode)

(use-package just-mode
  ;;  Emacs mode for justfiles.
  ;; https://github.com/leon-barrett/just-mode.el
  :ensure t)

(use-package flycheck
  ;; On the fly syntax checking for GNU Emacs.
  ;; https://github.com/flycheck/flycheck
  :ensure t)

;; ** C

(use-package clang-format
  ;; clang-format support for Emacs.
  ;; https://github.com/llvm/llvm-project/blob/main/clang/tools/clang-format/clang-format.el
  :if (executable-find "clang-format")
  :ensure t
  :after cc-mode
  :bind (:map c-mode-base-map ("C-c f" . clang-format-buffer)))

;; ** Lisp

(use-package paredit
  ;; Minor mode for editing parentheses.
  ;; http://mumble.net/~campbell/git/paredit.git/
  :ensure t)

(use-package outli
  ;; Outli specialization for Lisp -- see core config.
  :config
  (add-to-list 'outli-heading-config '(lisp-mode ";; " ?*))
  (add-to-list 'outli-heading-config '(emacs-lisp-mode ";; " ?*)))

(use-package rainbow-delimiters
  ;; Emacs rainbow delimiters mode.
  ;; https://github.com/Fanael/rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :ensure t)

;; *** Emacs Lisp

(add-hook 'emacs-lisp-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq-default fill-column 80)))

(use-package buttercup
  ;; Behavior-Driven Emacs Lisp Testing
  ;; https://github.com/jorgenschaefer/emacs-buttercup
  :ensure t)

;; *** Common Lisp

(use-package sly
  ;; Sylvester the Cat's Common Lisp IDE.
  ;; https://github.com/joaotavora/sly
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl"
        org-babel-lisp-eval-fn #'sly-eval))

;; *** Clojure

(use-package clojure-mode
  ;; Emacs support for the Clojure(Script) programming language.
  ;; https://github.com/clojure-emacs/clojure-mode
  :ensure t)

(use-package clojure-mode-extra-font-locking
  ;; Extra font-locking for Clojure mode
  ;; https://github.com/clojure-emacs/clojure-mode/blob/master/clojure-mode-extra-font-locking.el
  :ensure t)

(use-package cider
  ;; The Clojure Interactive Development Environment that Rocks for Emacs.
  ;; https://github.com/clojure-emacs/cider
  :ensure t)

(use-package clojure-snippets
  ;; Yasnippet's for Clojure
  ;; https://github.com/swannodette/clojure-snippets
  :ensure t)

;; *** Racket

(use-package racket-mode
  ;; Emacs major and minor modes for Racket.
  ;; https://github.com/greghendershott/racket-mode
  :ensure t)

;; *** Fennel

(use-package fennel-mode
  ;; Emacs support for the Fennel programming language.
  ;; https://git.sr.ht/~technomancy/fennel-mode
  :ensure t)

;; ** Groovy

(use-package groovy-mode
  ;; A groovy major mode, grails minor mode, and a groovy inferior mode.
  ;; https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes
  :ensure t)

;; ** Lua

(use-package lua-mode
  ;; Emacs major mode for editing Lua.
  ;; https://github.com/immerrr/lua-mode
  :ensure t)

;; ** JavaScript

(setq js-indent-level 2)

;; ** HTML

(use-package tagedit
  ;; A collection of paredit-like functions for editing in html-mode.
  ;; https://github.com/magnars/tagedit
  :ensure t)

;; ** Markdown

(use-package markdown-mode
  ;; Emacs Markdown Mode.
  ;; https://github.com/jrblevin/markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; ** YAML

(use-package yaml-mode
  ;; The emacs major mode for editing files in the YAML data serialization
  ;; format.
  ;; https://github.com/yoshiki/yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; ** PlantUML

(use-package plantuml-mode
  ;; A major mode for editing PlantUML sources in Emacs.
  ;; https://github.com/skuro/plantuml-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (setq org-plantuml-jar-path (expand-file-name "bin/plantuml.jar"
                                                alc-home-dir)))

;; * Network

(use-package restclient
  ;; HTTP REST client tool for Emacs.
  ;; https://github.com/pashky/restclient.el
  :ensure t
  :mode (("\\.http\\'" . restclient-mode))
  :init
  (when (and (package-installed-p 'cape)
             (package-installed-p 'company-restclient))
    (defun alc-init-restclient ()
      "Init the restclient package by adding the adequate completion function."
      (let ((capf (cape-company-to-capf #'company-restclient)))
        (add-hook 'completion-at-point-functions capf)))
    (add-hook 'restclient-mode-hook #'alc-init-restclient)))

(use-package company-restclient
  ;; Company-mode completion back-end for restclient-mode.
  ;; https://github.com/iquiw/company-restclient
  ;; The package is plugged into the completion functions through
  ;; cape-company-to-capf to be used by corfu.
  :ensure t
  :after restclient)

;; * Communication

(use-package erc
  ;; A modular and extensible IRC client for Emacs.
  ;; https://www.gnu.org/software/emacs/manual/erc.html
  :config
  (setq erc-hide-list '("JOIN" "PART" "QUIT")))

;; * Information

(use-package google-this
  ;; A set of emacs functions and bindings to google under point.
  ;; https://github.com/Malabarba/emacs-google-this
  :ensure t
  :bind-keymap ("C-x g" . google-this-mode-submap))

(use-package wttrin
  ;; Emacs frontend for weather web service wttr.in
  ;; https://github.com/cjennings/emacs-wttrin
  :ensure t
  :commands (wttrin))

(use-package elfeed
  ;; An Emacs web feeds client.
  ;; https://github.com/skeeto/elfeed
  :ensure t
  :commands elfeed
  :bind (("C-x w" . elfeed)
         :map elfeed-search-mode-map
         ("j" . next-line)
         ("k" . previous-line)
         ("c" . alc-elfeed-clear-filter))
  :config
  (setq elfeed-search-filter "")
  (defun elfeed-search-format-date (date)
    (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time date)))

  (defun alc-elfeed-clear-filter ()
    (interactive)
    (setq elfeed-search-filter "")
    (elfeed-update)))

;; * Media

(use-package somafm
  ;; A simple somafm interface in emacs.
  ;; https://github.com/alecigne/somafm.el
  :ensure t
  :if (not (eq system-type 'windows-nt))
  :commands (somafm somafm-by-completion)
  ;; I use my own fork since I once contributed to this package. Also an
  ;; illustration of the :vc keyword :)
  :vc (somafm
       :url "https://github.com/alecigne/somafm.el"
       :main-file "somafm.el"
       :branch "dev"
       :rev :newest))

;; * Writing

(setq ispell-program-name "aspell")

(use-package aggressive-fill-paragraph
  ;; An emacs minor-mode for keeping paragraphs filled (in both comments and
  ;; prose).
  ;; https://github.com/davidshepherd7/aggressive-fill-paragraph-mode
  :ensure t)

(defun alc-unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
logical line. This is useful, e.g., for use with
`visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(add-hook 'text-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (setq default-justification 'left)
            (setq fill-column 70)))

(use-package visual-fill-column
  ;; Emacs mode for wrapping visual-line-mode buffers at fill-column.
  ;; https://codeberg.org/joostkremers/visual-fill-column
  :ensure t)

(use-package writeroom-mode
  ;; Writeroom-mode: distraction-free writing for Emacs.
  ;; https://github.com/joostkremers/writeroom-mode
  :ensure t
  :bind ("H-<f10>" . writeroom-mode)
  :config
  (setq writeroom-restore-window-config t
        writeroom-width 120))

;; * AI

(use-package gptel
  ;; A simple LLM client for Emacs.
  ;; https://github.com/karthink/gptel
  :ensure t)

;; * Extras

;; ** Tools

(use-package multiple-cursors
  ;; Multiple cursors for Emacs.
  ;; https://github.com/magnars/multiple-cursors.el
  :ensure t
  :bind (("<M-S-mouse-1>" . 'mc/add-cursor-on-click)))

(use-package default-text-scale
  ;; Easily adjust the font size in all Emacs frames.
  ;; https://github.com/purcell/default-text-scale
  :defer 5
  :ensure t
  :config
  (default-text-scale-mode 1))

(defun alc-random-string (&optional n)
  "Generate a random alphanumeric string of length N."
  (let* ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
         (nchars (length chars)))
    (cl-loop
     repeat (or n 5)
     concat (char-to-string (aref chars (random nchars))))))

(use-package right-click-context
  ;; Emacs Right Click Context menu.
  ;; https://github.com/zonuexe/right-click-context
  :ensure t
  :defer 10
  :delight
  :config
  (right-click-context-mode))

(use-package lorem-ipsum
  ;; Add lorem ipsum filler text to Emacs.
  ;; https://github.com/jschaf/emacs-lorem-ipsum
  :ensure t)

(use-package ledger-mode
  ;; Emacs Lisp files for interacting with the C++Ledger accounting system.
  ;; https://github.com/ledger/ledger-mode
  :ensure t
  :config
  (setq ledger-default-date-format ledger-iso-date-format))

;; ** Eye-candy

(use-package all-the-icons
  ;; A utility package to collect various Icon Fonts and propertize them within
  ;; Emacs.
  ;; https://github.com/domtronn/all-the-icons.el
  :ensure t)

(use-package emojify
  ;; Display emojis in Emacs.
  ;; https://github.com/iqbalansari/emacs-emojify
  :ensure t)

;; ** Fun and games

(use-package fireplace
  ;; A cozy fireplace for Emacs.
  ;; https://github.com/johanvts/emacs-fireplace
  :ensure t)

(use-package snow
  ;; Let it snow in Emacs!
  ;; https://github.com/alphapapa/snow.el
  :ensure t)

(use-package xkcd-geohashing
  ;; An implementation of XKCD's geohashing algorithm for Emacs.
  ;; https://github.com/alecigne/xkcd-geohashing.el
  :ensure t
  :vc (xkcd-geohashing
       :url "https://github.com/alecigne/xkcd-geohashing.el"
       :main-file "xkcd-geohashing.el"
       :branch "master"
       :rev "v0.1.0"))

;; * Wrapping up

(provide 'alc-tools)
