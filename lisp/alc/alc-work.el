;;; alc-work.el --- Work-related configuration

;; * Utils

(defmacro region-or-prompt (prompt)
  `(interactive
    (if (region-active-p)
        (list (buffer-substring-no-properties (region-beginning) (region-end)))
      (list (read-string ,prompt)))))

;; * Org Roam

(defun alc-work-insert-last-daily ()
  "Insert the last Org roam daily."
  (interactive)
  (insert-file (car (last (org-roam-dailies--list-files)))))

(defun alc-work-format-roam-node ()
  (interactive)
  (let* ((node (org-roam-node-read))
         (link-desc (org-roam-node-formatted node))
         (link (org-link-make-string
                     (concat "id:" (org-roam-node-id node))
                     link-desc))
         (description (org-roam-node-file-title node))
         (pattern "[%s] %s"))
    (insert (format pattern link description))))

;; * Jira

(require 'request)

(defvar alc-work-jira-base-url nil
  "Base URL for Jira.")

(defvar alc-work-jira-token nil
  "Personal Jira token.")

(defun alc-work-jira-call-jira (path &rest args)
  (let* ((url (concat alc-work-jira-base-url path))
         (headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(format "Bearer %s" alc-work-jira-token))))
         (response
          (apply #'request url
                 :headers headers
                 :parser 'json-read
                 :sync t
                 args)))
    (request-response-data response)))

(defun alc-work-jira-get-issue (issue-id)
  (alc-work-jira-call-jira (format "/rest/api/2/issue/%s" issue-id)))

(defun alc-work-jira-add-comment (issue-id body)
  (alc-work-jira-call-jira (format "/rest/api/2/issue/%s/comment" issue-id)
                           :type "POST"
                           :data (json-encode `((body . ,body)))))

(defun alc-work-jira-get-issue-title (issue-id)
  (interactive)
  (let ((issue (alc-work-jira-get-issue issue-id)))
    (cdr (assoc 'summary (cdr (assoc 'fields issue))))))

(defun alc-work-jira-insert-issue (id)
  (interactive "sJira ID: ")
  (insert (alc-work-jira-get-issue-title id)))

(defun alc-work-jira-browse-issue (id)
  (region-or-prompt "Jira ID: ")
  (browse-url (format (concat alc-work-jira-base-url "/browse/%s") id)))

;; * Org & Org Roam / Jira

(use-package ox-jira
  ;; Org-mode export backend for JIRA markup
  ;; https://github.com/stig/ox-jira.el
  :ensure t
  :after org)

(defun alc-work-jira-insert-jira-link (issue-id)
  (region-or-prompt "Jira ID: ")
  (let ((org-link (org-link-make-string
               (concat alc-work-jira-base-url "/browse/" issue-id)
               issue-id)))
    (insert org-link)))

;; TODO It would be nice if the slug was built from the issue ID.
(defun alc-work-jira-create-roam-node (issue-id)
  "Create and populate an Org roam node from a Jira issue ID."
  (region-or-prompt "Jira ID: ")
  (let ((title (alc-work-jira-get-issue-title issue-id))
        (ref (concat alc-work-jira-base-url "/browse/" issue-id)))
    (org-roam-capture-
     :node (org-roam-node-create
            :title title
            :tags ":issue:"
            :aliases issue-id
            :refs ref)
     :templates '(("d" "default" plain "%?"
                   :target
                   (file+head
                    "%<%Y%m%d%H%M%S>-${slug}.org"
                    ":PROPERTIES:
:ID:       ${id}
:ROAM_ALIASES: ${aliases}
:ROAM_REFS: ${refs}
:END:
#+title: ${title}
#+filetags: ${tags}")
                   :unnarrowed t)))))

;; * Wrapping up

(provide 'alc-work)
