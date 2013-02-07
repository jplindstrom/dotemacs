
;; Occur from isearch
;; From: http://www.perlmonks.org/index.pl?node_id=539546
(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)


;; Kill from isearch
(defun isearch-kill-string ()
  "Make the current isearch-string the latest kill in the kill ring."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (kill-new (if isearch-regexp isearch-string (regexp-quote isearch-string)))
    (message "Killed region (still in I-search mode)")
    ))

(define-key isearch-mode-map (kbd "C-k") 'isearch-kill-string)




