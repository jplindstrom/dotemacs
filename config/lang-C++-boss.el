
;; Boss coding style

(defun boss-lineup-statement-cont (langelem)
  ;; handle MTD_IN/MTD_OUT with no semicolon
  (save-excursion
	(back-to-indentation)
	(cond
	 ((looking-at "}") '-)
	 ((re-search-backward "MTD_\\(IN\\|OUT\\)(\"[^\"]*\")[ \t\n]*\\=" nil t) 0)
	 (t '++))
	))

(c-add-style "boss"
	     '("cc-mode"
	       (indent-tabs-mode . t)
	       (tab-width . 4)
	       (c-basic-offset . 4)
	       (c-offsets-alist
			(defun-open . 0)
			(defun-close . 0)
			(defun-block-intro . +)
			(class-open . 0)
			(class-close . 0)
			(inline-open . 0)
			(inline-close . 0)
			(statement . 0)
			(statement-cont . boss-lineup-statement-cont)
			(block-open . 0)
			(block-close . 0)
			(statement-block-intro . +)
			(statement-case-intro . +)
			(statement-case-open . 0)
			(substatement . +)
			(substatement-open . 0)
			(case-label . +)
			(access-label . -)
			(else-clause . 0)
			)))

(defun boss-mode-setup ()
  (c-set-style "boss")
  (remove-hook 'c-special-indent-hook 'c-gnu-impose-minimum))

(add-hook 'c++-mode-hook 'boss-mode-setup)




