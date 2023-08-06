


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
(global-set-key (kbd "C-o e c g") 'jpl/git-link-dwim-branch)



;;; copy git-link for master branch

(defun jpl/git-link-master-branch ()
  (interactive)
  (let ((git-link-default-branch "master")
        (current-prefix-arg nil))
    (call-interactively 'git-link)))


(defun jpl/git-link-dwim-branch (arg)
  "Call git-link to copy a git link for the current branch. If
called with a prefix argument, use the 'master' branch instead."
  (interactive "P")
  (if arg
      (jpl/git-link-master-branch)
    (call-interactively 'git-link)))
