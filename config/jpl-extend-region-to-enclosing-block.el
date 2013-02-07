
;; Johan Lindstrom 2007. Free software. GPL.
(defun extend-region-to-enclosing-block ()
   "Extend the region to the enclosing syntactic block (naive, language unaware paren matching)"
   (interactive)
   (if (search-backward-regexp "[(){}]" nil t)
       (if (looking-at "[({]")
           ;; Enclosing parent block, make it the region
           (progn
             (forward-list)
             (push-mark nil t t)
             (backward-list)
             )
         ;; else, it's a sibling block, skip over it and try again
         (progn
           (forward-char)
           (backward-list)
           (extend-region-to-enclosing-block)
           )
         ))
   )



