


;; (global-set-key (kbd "\C-p \C-p") 'insert-perl-block)  ; with electric parens, insert {} BLOCK

(global-set-key (kbd "C-; C-l") 'insert-perl-self)   ; $self->
(global-set-key (kbd "C-; C-k") 'insert-perl-topic-call)   ; $_->
(global-set-key (kbd "C-; C-a") 'insert-perl-arrow)  ; ->
(global-set-key (kbd "C-; C-s") 'insert-perl-fat-comma)  ; =>

(global-set-key (kbd "C-; C-t u") 'insert-tt-c-uri-for)   ; [% c.uri_for('') %]
;; (global-set-key (kbd "\C-\; t") 'insert-tt-brackets)   ; [%  %]


(global-set-key (kbd "C-; w") 'delete-trailing-whitespace)

(fset 'insert-perl-block-including-next-lines
   [?  ?{ delete ?\C-s ?\C-q ?\C-j ?\C-q ?\C-j up return up tab ?} left ?\C-  ?\C-( ?\M-x ?i ?n ?d ?e ?n ?t ?  ?r ?e ?g ?  return down tab home tab])
(global-set-key (kbd "C-; C-;") 'insert-perl-block-including-next-lines)  ; with electric parens, insert {} BLOCK


(global-set-key (kbd "C-; I") 'indent-region)

(global-set-key (kbd "C-; c") 'comment-region)
(global-set-key (kbd "C-; C") 'uncomment-region)



(fset 'extract-perl-declaration-assignment
   [?\C-s ?m ?y ?  left left left ?\C-  ?\C-s ?  ?\C-s S-left C-insert ?\C-r ?m ?y ?  right left delete delete delete ?\C-r ?\C-q ?\C-j ?\C-q ?\C-j down home S-insert ?\; home])
(global-set-key (kbd "C-; e d") 'extract-perl-declaration-assignment)


(fset 'extract-perl-inline-comment
   [home ?\C-s ?# left S-end S-delete home return up S-insert home tab])
(global-set-key (kbd "C-; e c") 'extract-perl-inline-comment)


; obsoleted by argsm yasnippet
(fset 'insert-perl-method-self
   [?m ?y ?  ?$ ?s ?e ?l ?f ?  ?= ?  ?s ?h ?i ?f ?t ?\; return ?m ?y ?  ?( ?) ?  ?= ?  ?@ ?- ?\; backspace backspace ?_ ?\; return return up up right right right right ?$ backspace])
(global-set-key (kbd "C-; \C-m") 'insert-perl-method-self)


(defun just-one-space-and-newline ()
  (interactive)
  (just-one-space -1))
(global-set-key (kbd "C-; C-w") 'just-one-space-and-newline)



;; ee-imenu
;; Brign up the ee-imenu and expand the first heading
(fset 'ee-imenu-expand
   [?\M-x ?e ?e ?  ?i ?m ?e ?n ?u return right down C-right C-left])
(global-set-key (kbd "C-; i") 'ee-imenu-expand)


;; TAGS stuff
(global-set-key (kbd "C-; \C-t a") 'tags-apropos)
(global-set-key (kbd "C-; \C-t s") 'tags-search)



(global-set-key (kbd "C-; C-;") 'cperl-insert-block-dwim)


(global-set-key (kbd "\C-o e e v") 'lr-extract-variable)

