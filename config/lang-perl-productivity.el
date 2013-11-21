
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
    (if (looking-at "\\ *sub\\ +\\(\\w+\\)")
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
    (lp/beginning-of-sub-pod)

    ;; Nicked from window.el -- recenter-top-bottom
    (let ((this-scroll-margin
           (min (max 0 scroll-margin)
                (truncate (/ (window-body-height) 4.0))))
          )

      (recenter this-scroll-margin))
    )
  )


;; (jpl/add-to-load-path "lib/lang-refactor-perl")
;; (require 'lang-refactor-perl)
