
; obsoleted by PerlySense command
;; http://blog.jrock.us/articles/Increment%20test%20counter.pod

(defun jr/increment-number-at-point (&optional amount)
  "Increment the number under point by `amount'"
  (interactive "p")
  (let ((num (number-at-point)))
    (when (numberp num)
      (let ((newnum (+ num amount))
            (p (point)))
        (save-excursion
          (skip-chars-backward "-.0123456789")
          (delete-region (point) (+ (point) (length (number-to-string num))))
          (insert (number-to-string newnum)))
        (message (format "%d" newnum))
        (goto-char p)))))

(defun jr/increment-test-counter (&optional amount)
  "Increment the Test::More test counter by `amount'"
  (interactive "p")
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "tests\s+=>\s+" nil nil nil)
    (jr/increment-number-at-point amount)))

(global-set-key (kbd "C-; t") 'jr/increment-test-counter)



;; Make the $self->method(params) call into a sub declaration with the params
(fset 'perl-transform-self-method-call-to-method
   [home ?\C-s ?s ?e ?l ?f ?- ?> left right S-home delete ?s ?u ?b ?  ?\C-s ?( left ?  ?{ delete return ?m ?y ?  ?$ ?s ?e ?l ?f ?  ?= ?  ?s ?h ?i ?f ?t ?\; return ?m ?y ?  end backspace ?  ?= ?  ?@ ?_ ?\; return])
(global-set-key (kbd "C-; C-t s") 'perl-transform-self-method-call-to-method)


;Transform too long method call into one param per line
(fset 'perl-reformat-parameter-list
   [home ?\C-s ?- ?> left ?\C-s ?( left right left ?\C-  right return up end left ?\C-  ?\C-( ?\M-x ?n ?a ?r ?r ?o ?w ?  ?t ?o ?  ?r ?e ?g ?  return C-home ?\M-% ?, ?  return ?, ?\C-q ?\C-j return ?! C-end ?\C-r ?) left right return up end ?, C-home ?\M-x ?w ?i ?d ?e ?n return ?\C-  ?\C-( ?\M-x ?i ?n ?d ?e ?n ?t ?  ?r ?e ?g ?  return left ?\C-( home])
(global-set-key (kbd "C-; C-r") 'perl-reformat-parameter-list)


(global-set-key (kbd "C-; a") 'align-current)

(defun insert-env (env-variable-name)
  "Insert the value of env-variable-name into the buffer"
  (insert (getenv env-variable-name)))

(defun pips3/insert-test-db ()
  "Insert the value of $PIPS_TEST_DB into the buffer"
  (interactive)
  (insert-env "PIPS3_TEST_DB"))

(fset 'pips3/use-test-db
      [?u ?s ?e ?  ?\M-x ?i ?n ?s ?e ?r backspace backspace backspace backspace backspace ?p ?i ?p ?s ?3 ?/ ?i ?n ?s ?e ?r tab return ?\; return])
(global-set-key (kbd "C-; S-e") 'pips3/use-test-db)


(fset 'pips3-svn-commit-comment-with-branch-name
   [?\C-x ?1 C-home C-end ?\C-r ?# ?# ?  home down C-S-end delete ?\C-o ?g ?v C-home end ?\C-r ?/ C-tab ?\C-r ?\C-r right S-end S-left ?\M-w ?\C-x ?b return S-insert ?: ? ])
(global-set-key (kbd "C-; C-c") 'pips3-svn-commit-comment-with-branch-name)



(fset 'pid-test-pid
   [?\C-  right C-left ?\M-x ?m ?a ?r ?k ?  ?s ?e ?x ?  return ?\M-x ?p ?i ?p ?s ?3 ?4 ?/ backspace backspace ?/ ?p ?i ?d ?_ tab return end ?  ?# S-insert C-S-left C-S-left C-S-left C-S-left C-S-left C-S-left ?\M-x up return S-end S-insert ?\C-u ?\C-  ?\C-u ?\C-  ?\C-u ?\C-  ?\C-u ?\C-  ?\C-u ?\C-  ?\C-u ?\C- ])

(fset 'pid-copy-pids
   [?\C-  ?\M-x ?t ?e ?m ?p ?  ?t ?e ?s ?t ?  return ?\C-  end C-S-left ?\M-w left left S-end delete ?\C-u ?\C-  ?\C-u ?\C-  ?\C-u ?\C-  ?\C-  right C-left ?\M-x ?t ?e ?m backspace backspace backspace ?m ?a ?r ?k ?  ?d backspace ?s ?e ?x ?  return ?\M-w ?\C-u ?\C-  ?\C-u ?\C-  ?\C-x ?\C-s])

(fset 'pid-find-pid
   [?\M-x ?t ?e ?m ?  ?c ?o ?p ?y ?  return ?\C-\; ?\C-f up C-right C-right C-right C-right C-S-left ?\C-y end return C-tab])

(fset 'pid-replace-pid
   [C-home ?\M-% ?\C-y return ?\C-y ?\M-y return ?! ?\C-x ?\C-s])

(fset 'pid-replace-first-match
   [C-end C-home down down down down return ?\M-% ?\C-y return ?\C-y ?\M-y return ?! ?\C-x ?\C-s C-tab down])

(fset 'pid-replace-grep-match
   [return C-home ?\M-% return ?! ?\C-x ?\C-s C-tab down])





(fset 't2-find-t-file
   [?\C-s ?< left right ?\C-  ?\C-s ?> S-left C-insert ?\C-x ?o ?\C-x ?\C-f S-insert return C-home ?\C-x ?o home down down ?\C-  ?\C-s ?\C-q ?\C-j ?\C-q ?\C-j S-left C-insert ?\C-x ?o C-home ?\C-s ?= ?h ?e ?a ?d ?1 ?\C-x])
(global-set-key (kbd "\C-t f") 't2-find-t-file)


(fset 't2-insert-head1-name
   [?\C-x ?o ?\C-r ?= ?i ?t ?e ?m right right right right right right right right ?\C-  ?\C-s ?> S-left C-insert home down down ?\C-x ?o return home ?= ?h ?e ?a ?d ?1 ?  ?N ?A ?M ?E return return home S-insert ?  ?- return return home ?= ?c ?u ?t return return up up up up end ?  ?\C-x ?o home ?\C-  ?\C-s ?\C-q ?\C-j ?\C-q ?\C-j S-up C-insert ?\C-x ?o S-insert delete ?\C-r ?= ?h ?e ?a ?d ?1 home down down ?\M-q up up])
(global-set-key (kbd "\C-t n") 't2-insert-head1-name)
(global-set-key (kbd "\C-t \C-t") 't2-insert-head1-name)


(fset 't2-name-to-description
   [?\C-  ?\C-s ?= ?c ?u ?t S-up S-delete return return home ?= ?h ?e ?a ?d ?1 ?  ?D ?E ?S ?C ?R ?I ?P ?T ?I ?O ?N return return home S-insert up ?\C-r ?= ?h ?e ?a ?d ?1 down down home ?\M-q ?\C-r ?= ?h ?e ?a ?d ?1 home])
(global-set-key (kbd "\C-t d") 't2-name-to-description)



(fset 't2-insert-perl-shebang
   [?\C-x ?b return C-home S-down C-insert ?\C-x ?b return C-home S-insert down])
(global-set-key (kbd "\C-t p") 't2-insert-perl-shebang)

(fset 't2-again
   [?\C-x ?o ?\C-t ?f])
(global-set-key (kbd "\C-t \C-r") 't2-again)






(fset 'jpl-makeover-move-name-top-top
   [C-home ?\C-s ?= ?h ?e ?a ?d ?1 ?  ?n ?a ?m ?e home ?\C-  ?\C-s ?= ?c ?u ?t S-down S-delete C-home return S-insert return return return])
(fset 'jpl-makeover-move-use-strict-to-top
   [C-home ?\C-s ?u ?s ?e ?  ?s ?t ?r ?i ?c ?t home S-down S-down S-delete delete C-home ?\C-s ?p ?a ?c ?k ?a ?g ?e ?  home S-insert return])


(defun jpl-makeover-remove-pod-sub-whitespace ()
  "Clean up Perl source whitespace formatting"
  (interactive)

  (save-excursion
    ;; Remove moronic comment
    (replace-regexp "{\n[ \n]*# grab arguments *\n" "{\n" nil (point-min) (point-max))

    ;; Remove =cut with more POD as the next thing
    (replace-regexp "=cut\n\n+=" "\n\n=" nil (point-min) (point-max))

    ;; Remove whitespace between POD and sub
    (replace-regexp "=cut\n+sub " "=cut\nsub " nil (point-min) (point-max))

    ;; 3 newlines before subs if no POD
    (replace-regexp "\n\n+sub " "\n\n\n\nsub " nil (point-min) (point-max))

    ;; 3 newlines before =head...
    (replace-regexp "\n\n+=head" "\n\n\n\n=head" nil (point-min) (point-max))

    ;; ...except between the =head1 METHODS and the first POD after
    (replace-regexp "=head1 METHODS\n+=" "=head1 METHODS\n\n=" nil (point-min) (point-max))

    ;; ...except at the top POD block where it's more compact
    (replace-regexp "\n\n+=head1 NAME" "\n=head1 NAME" nil (point-min) (point-max))
    (replace-regexp "\n\n+=head1 DESCRIPTION" "\n\n=head1 DESCRIPTION" nil (point-min) (point-max))
    (replace-regexp "\n\n+=head1 SYNOPSIS" "\n\n=head1 SYNOPSIS" nil (point-min) (point-max))
    )

  (font-lock-fontify-buffer)   ;; In case it got confused
  )


(fset 'jpl-makeover-remove-author-add-end
   [C-home ?\C-s ?= ?h ?e ?a ?d ?1 ?  ?a ?u ?t ?h ?o ?r home ?\C-  ?\C-s ?= ?c ?u ?t delete backspace delete C-end ?\C-r ?1 ?\; end return return return ?+ ?+ S-backspace S-backspace ?_ ?_ ?E ?N ?D ?_ ?_ home return end C-S-end return up up up up up])

;; (global-set-key [f3] 'jpl-makeover-move-name-top-top)
;; (global-set-key [f4] 'jpl-makeover-move-use-strict-to-top)
;; (global-set-key [f5] 'jpl-makeover-remove-pod-sub-whitespace)
;; (global-set-key [f6] 'jpl-makeover-remove-author-add-end)
;; (global-set-key [f7] 'jpl-makeover-replace-item-with-head)






;; TO-DELETE
;; ;; from http://www.emacswiki.org/cgi-bin/wiki/PerlTidyElisp
;; ;; modified for PIPS
;; (defun my-perltidy (arg)
;;   (interactive "P")
;;   (let ((buffer (generate-new-buffer "*perltidy*"))
;;         (point (point))
;;         start end)
;;     (if (and mark-active transient-mark-mode)
;;         (setq start (region-beginning)
;;               end (region-end))
;;       (setq start (point-min)
;;             end (point-max)))
;;     (shell-command-on-region start end "perltidy -pro=$PIPS3_ROOT/.perltidyrc" buffer)
;;     (if arg
;;         (with-current-buffer buffer
;;           (perl-mode)
;;           (display-buffer buffer))
;;       (delete-region start end)
;;       (insert-buffer buffer)
;;       (kill-buffer buffer)
;;       (goto-char point))))



