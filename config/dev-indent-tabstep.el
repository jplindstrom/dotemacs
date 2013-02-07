
(defun jpl/indent-rigidly-tabstep (start end)
  "Indent rigidly four spaces"
  (interactive "r")
  (indent-rigidly start end 4)
  )

(defun jpl/unindent-rigidly-tabstep (start end)
  "Indent rigidly four spaces"
  (interactive "r")
  (indent-rigidly start end -4)
  )

(global-set-key (kbd "C-x <C-tab>") 'jpl/indent-rigidly-tabstep)
(global-set-key (kbd "C-x <C-S-iso-lefttab>") 'jpl/unindent-rigidly-tabstep)




