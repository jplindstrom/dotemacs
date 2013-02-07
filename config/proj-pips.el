
;; Pips
;; (defun pips3/crid2pid ()
;;   "Convert crid2pid"
;;   (interactive)
;;   (shell-command-on-region (point) (mark) "(cd $PIPS3_ROOT && ./script/crid2pid.pl)" t t)
;;   )

;; (defun pips3/pid_crid ()
;;   "Convert between crids and crids"
;;   (interactive)
;;   (shell-command-on-region (point) (mark) "(cd $PIPS3_ROOT && ./script/pid/convert.pl -)" t t)
;;   )

(defun pips3/pid_crid_info ()
  "Display the pid/crid converted to the other"
  (interactive)
  (let ((string (buffer-substring-no-properties (point) (mark))))
    (with-temp-buffer
      (insert string)
      (shell-command-on-region (point-min) (point-max)
                               "(cd $PIPS3_ROOT && ./script/pid/convert.pl -)"
                               (current-buffer) t)
      (let ((pid-crid
             (buffer-substring-no-properties (point-min) (- (point-max) 1))))
           (kill-new pid-crid)
           (message "%s" pid-crid))
      )))

(fset 'pips3/diff-xml-file
   [?\C-s ?g ?o ?t ?  ?x ?m ?l ?  ?f ?i ?l ?e ?  ?w ?r ?i ?t ?t ?e ?n ?  ?t ?o ?  left right S-end ?\M-w home ?\C-x ?1 ?\C-x ?3 ?\C-\; ?\C-x ?\C-f S-insert backspace backspace backspace backspace backspace backspace backspace backspace return ?\C-x ?o ?\C-\; ?\C-x ?\C-f S-insert return C-tab ?\M-x ?e ?d ?i ?f ?f ?  ?b ?u ?f ?f ?  return return return])


;; Escape the : and / (like, in a bad bid or oid)
(fset 'pips3/escape-bidoid
   [?\C-s ?: left S-right ?\M-x ?r ?e ?p ?l ?a ?c ?e ?  ?s ?t ?r ?i ?n ?g return ?: return ?% ?3 ?A return ?\C-s ?/ left S-right ?\M-x up return ?/ return ?% ?2 ?F return])



