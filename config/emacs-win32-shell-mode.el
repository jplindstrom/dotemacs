
(defun shell-strip-ctrls (output)
  "Strip ^M, and treat ^H as backspace in comint output."
  (interactive (list ""))
  (save-excursion
    (let ((x comint-last-output-start)
	  (mark (process-mark (get-buffer-process (current-buffer))))
	  ch)
      (while (< x mark)
	(setq ch (char-after x))
	(cond
	 ((= ch 13) (delete-region x (1+ x)))
	 ((= ch 8)
	  (delete-region (1- x) (1+ x))
	  (setq x (1- x)))
	 (t (setq x (1+ x)))
	 )))))

(add-hook 'shell-mode-hook (lambda ()
			     (add-hook 'comint-output-filter-functions
				       'shell-strip-ctrls nil t)))

