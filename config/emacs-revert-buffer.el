
;; Ctrl-x r == Revert-buffer
(defun jpl-revert-buffer ()
  "Revert buffer without query."
  (interactive)
  (revert-buffer t t))

;; JPL: hidden by undo-tree
(global-set-key (kbd "\C-x r") 'jpl-revert-buffer)



