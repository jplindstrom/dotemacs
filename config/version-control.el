
;; CVS

;; Find checkin revision from annotate/blame
(fset 'cvs-find-checkin-from-annotate-line
   [home ?\C-  ?\C-s ?  S-left C-insert home ?\C-x ?v ?l C-home ?\C-s ?r ?e ?v ?i ?s ?i ?o ?n ?  ?\M-y ?\C-q ?\C-j up])
(global-set-key (kbd "\C-x v \C-g c") 'cvs-find-checkin-from-annotate-line)



