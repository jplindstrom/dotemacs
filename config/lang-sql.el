
(add-hook 'sql-mode-hook #'(lambda () (setq tab-width 4)))



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
