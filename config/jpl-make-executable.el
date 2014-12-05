
(defun jpl/make-buffer-file-executable ()
  "Make the file of the buffer executable"
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (concat "chmod u+x " (buffer-file-name)))
      (message "Buffer doesn't have a file"))))
