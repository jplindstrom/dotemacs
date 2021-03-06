
(add-hook 'sql-mode-hook '(lambda () (setq tab-width 4)))



;; SQL stuff

(setq sql-mysql-options (list "-P 3306" "-A"))
(setq sql-server "localhost")

(setq sql-user "batapp")
(setq sql-password "batapp")
(setq sql-database "bat")


;; Sybase stuff
(setq sql-sybase-program "sqsh")



(defun jpl/sql-align-ddl (begin end)
  (interactive "r")
  (align-all-strings begin end " int\\| varchar\\| text\\| timestamp")
  (align-all-strings begin end " not null")
  (align-all-strings begin end " null")
  (align-all-strings begin end " default ")
  (align-all-strings begin end " references ")
  )



(fset 'sql-exec-region
   [C-insert ?\C-x ?b ?* ?S ?Q ?L ?* return C-end S-insert return ?\C-x ?b return])
(global-set-key (kbd "C-; s r") 'sql-exec-region)


(fset 'sql-exec-statement
   [?\M-x ?s backspace ?b ?o ?o ?k ?m ?a ?r ?k ?- ?s ?e ?t return ?s ?q ?l ?- ?e ?x ?e ?c return ?\C-r ?\C-q ?\C-j ?\C-q ?\C-j down down ?\C-  ?\C-s ?\; S-right C-insert ?\M-x ?b ?o ?o ?k ?m ?a ?r ?k ?- ?j ?u ?m ?p return return ?\C-x ?4 ?b ?* ?S ?Q ?L ?* return C-end S-insert return ?\C-x ?o])
(global-set-key (kbd "C-; s s") 'sql-exec-statement)


;; (fset 'sql-exec-desc-table
;;    [?\C-\M-r ?[ ?^ ?a ?- ?z ?] right ?\C-  ?\C-\M-s ?\C-\M-s S-left C-insert ?\C-x ?4 ?b ?* ?S ?Q ?L ?* return C-end S-insert home ?d ?e ?s ?c ?  end ?\; return ?\C-x ?o])
(fset 'sql-exec-desc-table
   [?\M-x ?s backspace ?b ?o ?o ?k ?m ?a ?r ?k ?- ?s ?e ?t return ?s ?q ?l ?- ?e ?x ?e ?c return ?\C-\M-r ?[ ?^ ?a ?- ?z ?0 ?- ?9 ?_ ?] right ?\C-  ?\C-\M-s ?\C-\M-s S-left C-insert ?\C-x ?4 ?b ?* ?S ?Q ?L ?* return C-end S-insert home ?d ?e ?s ?c ?  end ?\; return ?\C-x ?o ?\M-x ?b ?o ?o ?k ?m ?a ?r ?k ?- ?j ?u ?m ?p return])
(global-set-key (kbd "C-; s d") 'sql-exec-desc-table)

(fset 'sql-exec-count-table
   [?\M-x ?s backspace ?b ?o ?o ?k ?m ?a ?r ?k ?- ?s ?e ?t return ?s ?q ?l ?- ?e ?x ?e ?c return ?\C-\M-r ?[ ?^ ?a ?- ?z ?0 ?- ?9 ?_ ?] right ?\C-  ?\C-\M-s ?\C-\M-s S-left C-insert ?\C-x ?4 ?b ?* ?S ?Q ?L ?* return C-end S-insert home ?s ?e ?l ?e ?c ?t ?  ?c ?o ?u ?n ?t ?( ?* ?) ?  ?f ?r ?o ?m ?  end ?\; return ?\C-x ?o ?\M-x ?b ?o ?o ?k ?m ?a ?r ?k ?- ?j ?u ?m ?p return])
(global-set-key (kbd "C-; s c") 'sql-exec-count-table)

;; In the current SQL block, replace the next placeholder " ? " with
;; the next value in the list of values after the SQL block. The value
;; list must be comma separated and start with a :

(fset 'sql-replace-placeholder-with-value
   [?\C-e ?\( ?/ ?  ?? ?\C-m ?l ?m ?w ?\) ?/ ?: ?  ?\C-m ?l ?l ?d ?t ?, ?x ?x ?` ?w ?v ?P ?\C-p ?\C-p ?h ?\( ?\C-u ?\C-  ?B ?l])
(global-set-key (kbd "C-; s r") 'sql-replace-placeholder-with-value)



(fset 'jpl-sql-replace-placeholder-with-value
   [?\C-a ?\C-e ?F ?\' ?v ?a ?\' ?o ?l ?d ?\C-  ?\C-  ?? ?/ ?\C-? ?? ?\C-m ?v ?P ?\C-u ?\C-  ?\C-u ?\C-  ?\C-u ?\C-  ?X ?X ?\C-a])



(defun jpl/replace-all-rexes (search-for-rex replace-with begin end)
  (save-excursion
    (goto-char begin)
    (while (search-forward-regexp search-for end t)
      (replace-match replace-with t t))
    )
  )

(defun jpl/sql-format-query-in-region (begin end)
  (interactive "r")
  (save-excursion
    (jpl/replace-all-rexes " FROM " "\n    FROM " begin end)
    (jpl/replace-all-rexes " JOIN " "\n    JOIN " begin end)
    (jpl/replace-all-rexes " WHERE " "\n    WHERE " begin end)
    (jpl/replace-all-rexes " GROUP BY " "\n    GROUP BY " begin end)
    (jpl/replace-all-rexes " HAVING " "\n    HAVING " begin end)
    (jpl/replace-all-rexes " ORDER BY " "\n    ORDER BY " begin end)
    (jpl/replace-all-rexes " LIMIT " "\n    LIMIT " begin end)
    )
  )
