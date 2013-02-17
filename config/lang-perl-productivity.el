
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



(jpl/add-to-load-path "lib/lang-refactor-perl")
(require 'lang-refactor-perl)


