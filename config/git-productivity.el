


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
(global-set-key (kbd "C-o e c g") 'git-link)
