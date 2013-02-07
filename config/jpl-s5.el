
;; Convert org-mode region to html and put in kill-ring
(require 'pc-select)  ;; next-line-nomark
(defun jpl-org-s5-ul-to-html-copy ()
  (interactive)
  (if (not (and (point) (mark)))
      (message "Select a region to convert")
    (save-excursion
      (let
          ((html (org-export-region-as-html (point) (mark) nil 'string)))
        (with-temp-buffer
          (insert html)

          ;; Clean up id tags and breaks
          (goto-char (point-min))
          (replace-regexp " id=\"sec-[0-9.]+\"" "")
          (goto-char (point-min))
          (replace-regexp "<br/> *
 *</li>" "</li>")
          (goto-char (point-min))
          (replace-regexp "<br/> *
 *<ul>" "
<ul>")

          ;; Find the starting UL
          (goto-char (point-min))
          (search-forward-regexp "table-of-contents")
          (search-forward-regexp "</div>")
          (forward-line 2)
          (beginning-of-line)

          ;; Find the end of the UL
          (push-mark)
          (search-forward-regexp "<div id=\"postamble\">")
          (forward-line -1)
          (beginning-of-line)

          (copy-region-as-kill (point) (mark)))))))

;; Assuming a split window
;;  Take the org-lines in the region
;;  Convert them to an <ul> html list
;;  Switch to the other buffer
;;  Replace the content in the <div class="handout"></div>
(fset 'jpl-org-s5-insert-handout-html-from-region
   [?\M-x ?j ?p ?l ?- ?o ?r ?g ?- ?s ?5 ?- ?u ?l ?- ?t ?o ?- ?h ?t ?m ?l ?- ?c ?o ?p ?y tab return C-tab ?\C-r ?c ?l ?a ?s ?s ?= ?\" ?s ?l ?i ?d ?e ?\" end ?\C-s ?c ?l ?a ?s ?s ?= ?\" ?h ?a ?n ?d ?o ?u ?t ?\" end down home ?\C-  ?\C-s ?< ?/ ?d ?i ?v home delete S-insert ?\M-x ?i ?n ?d ?e ?n ?t ?  ?r ?e ?g ?  return ?\C-r ?c ?l ?a ?s ?s ?= ?\" ?h ?a ?n ?d ?o ?u ?t ?\" end ?\C-s ?< ?l ?i ?> left right])
(global-set-key (kbd "C-c h") 'jpl-org-s5-insert-handout-html-from-region)

;; Bold region
;; In the org-buffer, *surround* the region with stars, i.e. it'll
;; become bold once in the presentation
(fset 'jpl-org-s5-star_region
   "\C-x\C-x*\C-x\C-x*")
(global-set-key (kbd "C-c b") 'jpl-org-s5-star_region)

;; Insert takahashi_two heading text into presentation
;; Assuming a split window
;; In the org-buffer
;; Copy the region
;; Go to the presentation and find the first <div> in the current slide
;; Make the class takahashi_two, and insert the heading as the text, with / converted to <br />
(fset 'jpl-org-s5-insert-takahashi_two
   [?\M-w C-tab ?\C-r ?c ?l ?a ?s ?s ?= ?\" ?s ?l ?i ?d ?e ?\" right end ?\C-s ?< ?d ?i ?v ?  ?c ?l ?a ?s ?s ?= ?\" left right ?\C-  ?\C-s ?\" left delete ?t ?a ?k ?a ?h ?a ?s ?h ?i ?_ ?t ?w ?o right right ?\C-  ?\C-s ?< left delete S-insert ?\C-r ?/ left right delete ?< ?b ?r backspace backspace backspace backspace ?< ?b ?r ?/ right delete left right C-tab])
(global-set-key (kbd "C-c t") 'jpl-org-s5-insert-takahashi_two)




