
;; CVS

;; Find checkin revision from annotate/blame
(fset 'cvs-find-checkin-from-annotate-line
   [home ?\C-  ?\C-s ?  S-left C-insert home ?\C-x ?v ?l C-home ?\C-s ?r ?e ?v ?i ?s ?i ?o ?n ?  ?\M-y ?\C-q ?\C-j up])
(global-set-key (kbd "\C-x v \C-g c") 'cvs-find-checkin-from-annotate-line)





;; git-gutter

(load "git-gutter-fringe") ;; require doesn't work for some reason
(setq git-gutter-fr:side 'right-fringe)
(global-git-gutter-mode +1)

(set-face-foreground 'git-gutter-fr:added "royal blue")
(set-face-foreground 'git-gutter-fr:deleted "OrangeRed2")
(set-face-foreground 'git-gutter-fr:modified "royal blue")


;; Jump to next/previous hunklight
(global-set-key (kbd "C-x v [") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x v ]") 'git-gutter:next-hunk)

;; Display Diff
(global-set-key (kbd "C-x v d") 'git-gutter:popup-diff)
;; Display Hunk
(global-set-key (kbd "C-x v h") 'git-gutter:popup-hunk)


;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
;; Revert (kill) current hunk
(global-set-key (kbd "C-x v k") 'git-gutter:revert-hunk)


(global-set-key (kbd "C-x v c") 'magit-commit)
