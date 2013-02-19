;;; lang-refactor-perl-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (lr-extract-variable) "lang-refactor-perl" "lang-refactor-perl.el"
;;;;;;  (20771 63595))
;;; Generated autoloads from lang-refactor-perl.el

(autoload 'lr-extract-variable "lang-refactor-perl" "\
Do refactoring 'extract Perl variable' of active region.

Ask the user for a variable name to extract the active region
into.

Replace all occurences in the current defun with the variable and
insert a variable declarion (initialized with the region text).

Push the mark and then leave point at the new variable
declaration (you'll need to ensure this is a reasonable location
before jumping back).

By default, only the current defun is changed. Invoke with the
prefix arg to change the entire buffer.

Both replacements and the declaration are highlighted.

\(fn BEG END)" t nil)

;;;***

;;;### (autoloads nil nil ("lang-refactor-perl-pkg.el") (20771 63595
;;;;;;  894364))

;;;***

(provide 'lang-refactor-perl-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lang-refactor-perl-autoloads.el ends here
