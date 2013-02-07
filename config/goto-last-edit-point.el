
(defun goto-last-edit-point ()
    "Go to the last point where editing occured."
      (interactive)
        (let ((undos buffer-undo-list))
	      (if (listp undos)
		  (while (and undos
			      (let ((pos (or (cdr-safe (car undos))
					     (car undos))))
				(not (and (integerp pos)
					  (goto-char (abs pos))))))
		    (setq undos (cdr undos))))))

(global-set-key "\C-c " 'goto-last-edit-point)



