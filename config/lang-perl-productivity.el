
;; Perl productivity macros
(fset 'insert-perl-self
   "$self->")

(fset 'insert-perl-arrow
   "->")

(fset 'insert-perl-fat-comma
   "=> ")

(fset 'insert-perl-topic-call
   "$_->")

;; (fset 'insert-perl-block
;;    [?  ?{ return return up tab])

(fset 'insert-tt-brackets
   [?[ ?% ?  ?  ?% ?] left left left])

(fset 'insert-tt-c-uri-for
   [?[ ?% ?  ?c ?. ?u ?r ?i ?_ ?f ?o ?r ?( ?' ?' ?) ?  ?% ?] left left left left left])

(fset 'perl-moose-has
   [home ?h ?a ?s ?  end ?  ?\C-\; ?\C-s ?( ?i ?s ?  ?\C-\; ?\C-s ?\" ?r ?w ?\" ?) ?\; home C-right right])


;; Re-indent Perl code in POD blocks
;; With region in POD, run this to reformat at one brace level
(fset 'jpl/perl-reindent-region
   [?y ?\C-x ?b ?* ?t ?e ?m ?p ?- ?r ?e ?f ?o ?r ?m ?a ?t ?- ?c ?p ?e ?r ?l ?* return ?\M-x ?c ?p ?e ?r ?l ?  ?m ?o ?d ?  return ?i ?\{ return escape ?V ?P ?g ?g ?V ?G ?= ?j ?V ?G ?k ?y ?\C-x ?k return ?g ?v ?P])



(defun jpl-makeover-replace-item-with-head ()
  "Clean up Perl source =item vs =head2 formatting"
  (interactive)

  (save-excursion
    ;; Remove over, back
    (replace-regexp "\n=over 4\n" "" nil (point-min) (point-max))
    (replace-regexp "\n=back\n" "" nil (point-min) (point-max))

    ;; Replace =item with =head2
    (replace-regexp "=item" "=head2" nil (point-min) (point-max))
    )

  (cperl-mode)   ;; In case it got confused
  )



(defun cperl-insert-block ()
   "Insert a { } block/hashref and put the point at a proper
position ready to start typing.

If point is preceeded by a fat arrow (=>) it's considered part of
a data structure, and a comma will be added after the { }.
"
   (interactive)

   (let ((post-amble (if (looking-back "=> *") "," "")))
     (if (not (looking-back " ")) (insert " "))
     (insert "{\n\n}" post-amble)
     (indent-according-to-mode)

     (previous-line 1)
     (indent-according-to-mode)
     )
   )


(defun cperl-insert-block-dwim ()
   "Insert a { } block (see `cperl-insert-block').

If there is a region, wrap the block around the region.
"
   (interactive)
   (if (not (and transient-mark-mode mark-active))
       (cperl-insert-block)
     (kill-region (region-beginning) (region-end))
     (cperl-insert-block)

     (delete-indentation)
     (yank)
     (indent-region (region-beginning) (region-end))

     (exchange-point-and-mark)
     (forward-char)
     (indent-according-to-mode)
     )
   )


(defun cperl-add-semicolon ()
  (interactive)
  (save-excursion
    (when (search-forward-regexp "[})]" nil t)
      (while (looking-at "[})]")
        (forward-char))
      (insert ";")
      )))



(defun perl/split-note-string ()
  "For use on a line with a 'note' STRING where the string is too
long. Wrap the double quoted STRING by wrapping it to the next
line with a new 'note' call."
  (interactive)
  (when (<= (point-at-eol) (+ (point-at-bol) fill-column ))
    (error "String doesn't need to be wrapped"))
  (beginning-of-line)
  (unless (looking-at " \*note ") (error "No test 'note' call on this line"))
  (forward-char fill-column)
  (search-backward " ")
  (forward-char)
  (insert "\";\nnote \"")
  (indent-according-to-mode)
  (back-to-indentation))



(defun lp/point-of-previous-defun ()
  "Return point of the previous defun, or nil if there isn't one"
  (save-excursion
    (if (beginning-of-defun)
        (point)
      nil)))

(defun lp/point-of-pod-line-with (end pod-text)
  "Return point of POD line which contains POD-TEXT between point
and END (which should be earlier than point) or nil if there is
none."
  (save-excursion
    (let ((case-fold-search nil))
      (if (search-backward-regexp
           ;; Matches POD directive, and also POD-TEXT as a word, e.g.
           ;; =head2 foo
           ;; =item $self->foo($bar)
           (format "^=\\w+\\ .*?\\b%s\\b" (regexp-quote pod-text))
           end
           t
           )
          (progn
            (beginning-of-line)
            (point))
        nil
        ))))

(defun lp/sub-name ()
  "Return the name of the sub at point, or nil if there isn't one"
  (save-excursion
    (if (looking-at "\\ *sub\\ +\\(\\w+\\)") ;; Also include _
        (match-string 1)
      nil)
    ))

(defun lp/point-of-sub-pod ()
  "Return point of the POD heading/whatever of sub at point.

It is assumed point is on a line with

  sub foo

and there's an earlier line

  =SOME_POD_DIRECTIVE optional something, something foo

in between the sub foo and the sub preceeding it (or the
beginning of the buffer if there isn't one).
"
  (let ((sub-name (lp/sub-name))
        (end (or (lp/point-of-previous-defun) (point-min)))
        )
    (if (and sub-name end)
        (lp/point-of-pod-line-with end sub-name)
      nil
      ))
  )

(defun lp/beginning-of-sub-pod ()
  "Move point to the POD line of the sub at point, or leave it be
  if there isn't one."
  (let ((pod-point (lp/point-of-sub-pod)))
    (when pod-point (goto-char pod-point)))
  )

(defun lp/recenter-top-of-defun ()
  "Move to the beginning of the current defun and put that line
at the top of the window."
  (interactive)

  (when (not (lp/sub-name))                     ;; If not yet at the sub {}
    (when (save-excursion (beginning-of-defun)) ;; If we can move to sub {}
      (beginning-of-defun)))                    ;; Move to the sub {}
  (when (lp/sub-name)
    (let ((finish-pos (point)))
      (lp/beginning-of-sub-pod)

      ;; Nicked from window.el -- recenter-top-bottom
      (let ((this-scroll-margin
             (min (max 0 scroll-margin)
                  (truncate (/ (window-body-height) 4.0))))
            )
        (recenter this-scroll-margin))

      (goto-char finish-pos)
      )
    )
  )


(defun lr-extract-method (beg end &optional arg-variable-name)
  "Do refactoring 'extract Perl method (or sub)' of active region.

Ask the user for a mehhod name to extract the active region
into.

Replace all occurences in the current defun with the method call
and insert a new method above the current sub with the code in
the region text.

Push the mark and then leave point at the new method
declaration (you shoud ensure this is a reasonable location
before jumping back)."
  (interactive "r")
  (unless (and transient-mark-mode mark-active)
    (error "Select a self-contained piece of code to extract"))
  (when (< (point) (mark)) (exchange-point-and-mark t))
  ;; (let ( (the-point (min (point) (mark) ))
  ;;        (the-mark (max (mark) (point))) )
  (let ( (the-point (point))
         (the-mark (mark )) )
    (set-mark-command nil)
    (save-restriction
      (shell-command-on-region
       the-point the-mark
       "perl_extract" nil t)
      (deactivate-mark)

      (let ( (point-at-end (point)) )
        ;; Kill the entire sub
        (backward-sexp)
        (beginning-of-line)
        (prin1 point-at-end)
        (kill-region (point) point-at-end)

        ;; ;; Move up to the metod call and indent the line
        (forward-line -1)
        (indent-according-to-mode)

        ;; Move up to before the current sub
        (backward-up-list)
        (forward-line -1)

        ;; Insert killed sub, with whitespace
        (insert "\n\n")
        (forward-line -1)
        (yank)

        ;; Position point at sub name
        (backward-sexp)
        (beginning-of-line)
        (forward-word) (forward-char)
        )
      )
    )
  )


(fset 'jpl/perl-test-convert-comment-to-note-evil
   [?\C-a ?\C-s ?# ?\C-m ?b ?c ?w ?n ?o ?t ?e escape ?l ?v ?\C-e ?h ?s ?\" ?A ?\; escape ?\C-a])



(fset 'jpl/perl-insert-package-from-filename
   [?\C-o ?e ?c ?f ?O ?p ?a ?c ?k ?a ?g ?e ?  escape ?P ?A backspace backspace backspace ?\; escape ?? ?/ ?l ?i ?b ?/ ?\C-m ?e ?l ?l ?d ?B ?V ?\M-% ?/ return ?: ?: return ?! ?\C-a])


;; (jpl/add-to-load-path "lib/lang-refactor-perl")
;; (require 'lang-refactor-perl)


;; Insert
;;     my $time_this = timer("Package::Name->sub_name");
(fset 'jpl/insert-timer-guard
   [?o ?m ?y ?  ?$ ?t ?i ?m ?e ?_ ?t ?h ?i ?s ?  ?= ?  ?t ?i ?m ?e ?r ?\( ?\" escape ?\C-o ?e ?c ?p ?P ?a ?\C-\; ?\C-a escape ?\C-o ?e ?c ?s ?P ?A ?\; escape ?\C-a])



(defun jpl/perl-goto-jpl-grep-buffer ()
  (interactive)
  (switch-to-buffer "*JPL-grep*")
  )




(defun jpl/perl-disable-all-subtests ()
  "Disable all lines which starts with 'subtest '"
  (interactive)
  (save-excursion
    (replace-regexp "^subtest " "0 and subtest " nil (point-min) (point-max))))

(defun jpl/perl-enable-all-subtests ()
  "Enable all disabled subtests"
  (interactive)
  (save-excursion
    (replace-regexp "^0 and subtest " "subtest " nil (point-min) (point-max))))
