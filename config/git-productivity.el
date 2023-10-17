


;;JPL: check with magit upgrade
;; git-timeline on current buffer
(defun jpl/git-timeline-on-buffer ()
  (interactive)
  (shell-command (format "git-timeline --file='%s'" (buffer-file-name)))
  )


(global-set-key (kbd "C-o d g t") 'jpl/git-timeline-on-buffer)


(global-set-key (kbd "C-x v G") 'git-messenger:popup-message)


;; git-link
;; https://github.com/sshaw/git-link
(global-set-key (kbd "C-o e c g g") 'jpl/git-link-for-branch)

(global-set-key (kbd "C-o e c g f") 'jpl/git-org-link-project-file)
(global-set-key (kbd "C-o e c g l") 'jpl/git-org-link-current-line-text)




;;; copy git-link for master branch

(defun jpl/git-link-master-branch ()
  (interactive)
  (let ((git-link-default-branch "master")
        (current-prefix-arg nil))
    (call-interactively 'git-link)))


;; Main entry point for smart git-link
(defun jpl/git-link-for-branch (arg)
  "Call git-link to copy a git link for the current branch. If
called with a prefix argument, use the 'master' branch instead."
  (interactive "P")
  (if arg
      (jpl/git-link-master-branch)
    (call-interactively 'git-link)))


(defun jpl/git-org-link--formatter (url title)
  (format "[[%s][%s]]" url title))

(defun jpl/git-markdown-link--formatter (url title)
  (format "[%s](%s)" title url))


(defun jpl/git-link--kill-formatterd-link (title-fn formatter-fn formatter-name)
  "Return org-link with the LINK and the title from calling TITLE-FN"
  (call-interactively 'jpl/git-link-for-branch)
  (let* ((url (current-kill 0))
         (title (funcall title-fn))
         (formatted-link (funcall formatter-fn url title)))
    (kill-new formatted-link t)
    (message "Copied %s link: %s --> %s" formatter-name title url)))

(defun jpl/git-org-link-project-file (arg)
  "Copy a git-link, but as an org-mode link. Use the
buffer (project relative) filename as the link title."
  (interactive "P")
  (jpl/git-link--kill-formatterd-link
   (lambda () (file-relative-name buffer-file-name (projectile-project-root)))
   'jpl/git-org-link--formatter
   "Org"))

(defun jpl/git-org-link--current-line-text ()
  (buffer-substring
     (save-excursion (beginning-of-line-text) (point))
     (line-end-position)))
(defun jpl/git-org-link-current-line-text (arg)
  "Copy a git-link, but as an org-mode link. Use the current line
(without indentation) as the link title."
  (interactive "P")
  (jpl/git-link--kill-formatterd-link
   (lambda () (jpl/git-org-link--current-line-text))
   'jpl/git-org-link--formatter
   "Org"))

(defun jpl/git-org-link-perl-method-name (arg)
  "Copy a git-link, but as an org-mode link. Use the current Perl
method as the title."
  (interactive "P")
  (jpl/git-link--kill-formatterd-link
   (lambda () (ps/current-method-name))
   'jpl/git-org-link--formatter
   "Org"))

(defun jpl/git-org-link-perl-package-name (arg)
  "Copy a git-link, but as an org-mode link. Use the current Perl
package name as the title."
  (interactive "P")
  (jpl/git-link--kill-formatterd-link
   (lambda () (ps/current-package-name))
   'jpl/git-org-link--formatter
   "Org"))

(defun jpl/git-org-link-perl-sub-name (arg)
  "Copy a git-link, but as an org-mode link. Use the current Perl
sub name as the title."
  (interactive "P")
  (jpl/git-link--kill-formatterd-link
   (lambda () (ps/current-sub-name))
   'jpl/git-org-link--formatter
   "Org"))
