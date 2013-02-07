
;; PerlTidy -- move this to PerlySense
(defun perltidy-region (beg end)
  "Run perltidy on the current region."
  (interactive "r")
  (let ((original-point (point)))
      (shell-command-on-region beg end "perltidy" nil t)
      (goto-char original-point)
      )
  )

(defun perltidy-sub ()
  "Run perltidy on the current sub."
  (interactive)
  (let ((original-point (point)))
    (if (perltidy-mark-sub-and-docs)
        (progn
          (perltidy-region (point) (mark))
          (goto-char original-point)
          )
      )
    )
  )

(defun perltidy-mark-sub-and-docs ()
  "Mark the current sub, and possibly its preceeding POD block"
  (interactive)
  (mark-defun)
  (if (looking-back "\n=cut *?\n+")
      (progn
        (cperl-backward-to-noncomment (point-min))
        (if (search-forward-regexp "\n=" (mark) t)
            (progn
              (beginning-of-line)
              (if (looking-back "\n") (forward-line -1))
              t
              )
          nil
          ))
    t
    )
  )

(defun perltidy-buffer ()
  "Run perltidy on the current buffer."
  (interactive)
  (let ((original-point (point)))
    (perltidy-region (point-min) (point-max))
    (goto-char original-point)
    )
  (message "Ran Perl::Tidy on buffer")
  )

(defun perltidy-dwim ()
  "Run perltidy on the region, or the current sub"
  (interactive)
  (if (and mark-active transient-mark-mode)
      (progn
        (perltidy-region (region-beginning) (region-end))
        (message "Ran Perl::Tidy on region")  )
    (progn
      (perltidy-sub)
      (message "Ran Perl::Tidy on sub")
      )
    )
  (cperl-mode)
  )

(global-set-key (kbd "\C-o m t t") 'perltidy-dwim)
(global-set-key (kbd "\C-o m t b") 'perltidy-buffer)
(global-set-key (kbd "\C-o m t r") 'perltidy-region)
(global-set-key (kbd "\C-o m t s") 'perltidy-sub)



