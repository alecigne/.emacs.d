;;; alc-functions-test.el --- Small tests for SwanEmacs -*- lexical-binding: t; -*-

(require 'buttercup)

(defconst alc-test-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory of the SwanEmacs configuration under test.")

;; Deliberately exercise the configuration as a whole.  This also makes the
;; tests fail early when the real startup path can no longer be loaded.
(unless (featurep 'alc-init)
  (setq user-emacs-directory alc-test-root)
  (load (expand-file-name "early-init.el" alc-test-root) nil nil)
  (load (expand-file-name "init.el" alc-test-root) nil nil))

;; Activate deferred configuration before asserting Org-specific behavior.
(require 'org)

(describe "alc-hex-from-string-palette"
  (it "turns a Coolors palette into hexadecimal colors"
    (expect (alc-hex-from-string-palette "1c97d9-66e182-e84a52")
            :to-equal '("#1c97d9" "#66e182" "#e84a52")))

  (it "accepts a palette containing one color"
    (expect (alc-hex-from-string-palette "abcdef")
            :to-equal '("#abcdef"))))

(describe "alc-random-string"
  (it "uses a default length of five characters"
    (expect (alc-random-string) :to-match "\\`[A-Za-z0-9]\\{5\\}\\'"))

  (it "honors an explicit length"
    (expect (alc-random-string 12) :to-match "\\`[A-Za-z0-9]\\{12\\}\\'"))

  (it "can generate an empty string"
    (expect (alc-random-string 0) :to-equal "")))

(describe "alc-insert-week-number"
  (it "uses ISO week numbering at a year boundary"
    (with-temp-buffer
      (alc-insert-week-number "2021-01-01")
      (expect (buffer-string) :to-equal "53")))

  (it "pads single-digit week numbers"
    (with-temp-buffer
      (alc-insert-week-number "2021-01-04")
      (expect (buffer-string) :to-equal "01"))))

(describe "alc-unfill-region"
  (it "joins the visual lines of a paragraph"
    (with-temp-buffer
      (insert "This paragraph is\nwrapped across lines.")
      (alc-unfill-region (point-min) (point-max))
      (expect (buffer-string)
              :to-equal "This paragraph is wrapped across lines."))))

(describe "alc-org-project-pages--title"
  (let (file)
    (before-each
      (setq file (make-temp-file "swanemacs-" nil ".org")))

    (after-each
      (when (and file (file-exists-p file))
        (delete-file file)))

    (it "reads an Org title case-insensitively and trims it"
      (with-temp-file file
        (insert "#+TITLE:   A lovely page   \n"))
      (expect (alc-org-project-pages--title file)
              :to-equal "A lovely page"))

    (it "falls back to the file name when no title exists"
      (with-temp-file file
        (insert "A page without a title.\n"))
      (expect (alc-org-project-pages--title file)
              :to-equal (file-name-base file)))))

(describe "alc-powerorg-view-definition"
  (it "returns an explicitly defined view"
    (let ((alc-powerorg-view-definitions
           '((overview :title "Overview" :blocks (inbox calendar))))
          (alc-powerorg-block-definitions nil))
      (expect (alc-powerorg-view-definition 'overview)
              :to-equal '(:title "Overview" :blocks (inbox calendar)))))

  (it "synthesizes a view from a view-enabled block"
    (let ((alc-powerorg-view-definitions nil)
          (alc-powerorg-block-definitions
           '((inbox :title "Inbox" :view t))))
      (expect (alc-powerorg-view-definition 'inbox)
              :to-equal '(:title "Inbox" :blocks (inbox))))))

(describe "SwanEmacs startup"
  (it "loads every public configuration module"
    (expect (mapcar #'featurep
                    '(alc-core alc-tools alc-org alc-experimental alc-init))
            :to-equal '(t t t t t)))

  (it "installs the main Org commands on their global keys"
    (expect (key-binding (kbd "C-c a")) :to-be #'org-agenda)
    (expect (key-binding (kbd "C-c c")) :to-be #'org-capture)
    (expect (key-binding (kbd "C-c l")) :to-be #'org-store-link)))

(describe "the SwanEmacs Org workflow"
  (it "defines its task-state progression"
    (expect org-todo-keywords
            :to-equal
            '((sequence "TODO(t!)"
                        "MAYB(m!)"
                        "PROG(p!)"
                        "OPEN(o!)"
                        "WAIT(w@/!)"
                        "HOLD(h@/!)"
                        "|"
                        "DONE(d!)"
                        "GIVN(g@)"
                        "CNCL(c@)"))))

  (it "provides task and note capture templates"
    (expect (mapcar #'car org-capture-templates)
            :to-equal '("t" "n")))

  (it "keeps workflow tags local to their task"
    (expect org-tags-exclude-from-inheritance
            :to-contain alc-org-next-tag)
    (expect org-tags-exclude-from-inheritance
            :to-contain alc-org-project-tag)))

(describe "alc-org-delete-link"
  (it "replaces an Org link with its description"
    (with-temp-buffer
      (org-mode)
      (insert "[[https://example.com][Example]]")
      (goto-char (point-min))
      (alc-org-delete-link)
      (expect (buffer-string) :to-equal "Example")))

  (it "uses the address when the link has no description"
    (with-temp-buffer
      (org-mode)
      (insert "[[https://example.com]]")
      (goto-char (point-min))
      (alc-org-delete-link)
      (expect (buffer-string) :to-equal "https://example.com"))))

(describe "alc-org-agenda-block-has-entry-p"
  (it "rejects a block containing only presentation text"
    (with-temp-buffer
      (insert "Nothing scheduled")
      (expect (alc-org-agenda-block-has-entry-p (point-min) (point-max))
              :to-be nil)))

  (it "recognizes an entry carrying an Org marker"
    (with-temp-buffer
      (insert "Scheduled task")
      (add-text-properties (point-min) (1+ (point-min)) '(org-marker t))
      (expect (alc-org-agenda-block-has-entry-p (point-min) (point-max))
              :not :to-be nil))))

;;; alc-functions-test.el ends here
