
;; git-timeline on current buffer
(defun jpl/git-timeline-on-buffer ()
  (interactive)
  (shell-command (format "git-timeline --file='%s'" (buffer-file-name)))
  )


(global-set-key (kbd "C-o d g t") 'jpl/git-timeline-on-buffer)
