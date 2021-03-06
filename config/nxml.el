
(jpl/add-to-load-path "lib/nxml-mode")


(load "rng-auto.el")
(setq auto-mode-alist
      (cons '("\\.\\(wsdl\\|xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
	    auto-mode-alist))
;(add-hook 'nxml-mode-hook
;	  '(lambda ()
;              (setq show-trailing-whitespace t)
;              (local-set-key [(return)] 'newline-and-indent)
;              )
; 	  )


(setq
 nxml-sexp-element-flag t
)

(defun nxml-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))


