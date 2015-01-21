

(setq jiralib-url "https://broadbean.atlassian.net")
; you need make sure the jiralib-url is correct. Login your jira
; server in browser, the home page URL should be like:
; https://issues.apache.org/jira/secure/Dashboard.jspa
; remove the "/secure/Dashboard.jspa" part and you get the jiralib-url: 
; "https://issues.apache.org/jira"

(require 'org-jira)
; jiralib is not explicitly required, since org-jira will load it
