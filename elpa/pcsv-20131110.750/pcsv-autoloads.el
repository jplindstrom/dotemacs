;;; pcsv-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (pcsv-file-parser pcsv-parser pcsv-parse-file pcsv-parse-buffer
;;;;;;  pcsv-parse-region) "pcsv" "pcsv.el" (21682 58895))
;;; Generated autoloads from pcsv.el

(autoload 'pcsv-parse-region "pcsv" "\
Parse region as a csv.

\(fn START END)" nil nil)

(autoload 'pcsv-parse-buffer "pcsv" "\
Parse a BUFFER as a csv. BUFFER defaults to `current-buffer'.

\(fn &optional BUFFER)" nil nil)

(autoload 'pcsv-parse-file "pcsv" "\
Parse FILE as a csv file with CODING-SYSTEM.
To handle huge file, please try `pcsv-file-parser' function.

\(fn FILE &optional CODING-SYSTEM)" nil nil)

(autoload 'pcsv-parser "pcsv" "\
Get a CSV parser to parse BUFFER.
This function supported only Emacs 24 or later.


Example:
\(setq parser (pcsv-parser))
\(let (tmp)
  (while (setq tmp (funcall parser))
    (print tmp)))

\(fn &optional BUFFER)" nil nil)

(autoload 'pcsv-file-parser "pcsv" "\
Create a csv parser to read huge FILE.
This csv parser accept a optional arg which non-nil means terminate the parser.

Optional arg BLOCK-SIZE indicate bytes to read FILE each time.

Example:
\(let ((parser (pcsv-file-parser \"/path/to/csv\")))
  (unwind-protect
      (let (tmp)
        (while (setq tmp (funcall parser))
          (print tmp)))
    ;; Must close the parser
    (funcall parser t)))

\(fn FILE &optional CODING-SYSTEM BLOCK-SIZE)" nil nil)

;;;***

;;;### (autoloads nil nil ("pcsv-pkg.el") (21682 58895 445990))

;;;***

(provide 'pcsv-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pcsv-autoloads.el ends here
