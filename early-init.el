;;; early-init.el --- Emacs 27+ pre-init -*- lexical-binding: t -*-

;; Optimize some params during startup and reboot them afterwards
(let ((default-gc-cons-threshold gc-cons-threshold)
      (default-file-name-handler-alist file-name-handler-alist)
      (start-time (current-time)))
  (setq gc-cons-threshold most-positive-fixnum
        file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist default-file-name-handler-alist)
              ;; The first GC might take a while. Do it while idle.
              (run-with-idle-timer
               5 nil
               (lambda ()
                 (setq gc-cons-threshold default-gc-cons-threshold)
                 (garbage-collect)))
              (message "Emacs started in %.03fs."
                       (float-time (time-since start-time))))))

;; Tweak UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode '(2 . 2))
(setq frame-inhibit-implied-resize t)

;; Do not initialize packages automatically
(setq package-enable-at-startup nil)
