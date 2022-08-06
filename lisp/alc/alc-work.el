;;; alc-work.el --- Work-related configuration

;; Copyright (C) 2022

;; Author: Anthony Le Cigne <dev@lecigne.net>

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

(defun alc-work-get-issue-title (issue-id)
  (interactive)
  (let ((issue (alc-work-jira-get-issue issue-id)))
    (cdr (assoc 'summary (cdr (assoc 'fields issue))))))

;; * Wrapping up

(provide 'alc-work)
