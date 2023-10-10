;;; jest-test-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "jest-test-mode" "jest-test-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from jest-test-mode.el

(autoload 'jest-test-mode "jest-test-mode" "\
Toggle jest minor mode.
With no argument, this command toggles the mode. Non-null prefix
argument turns on the mode. Null prefix argument turns off the
mode

If called interactively, enable Jest-Test mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'jest-test-run "jest-test-mode" "\
Run the current buffer's file as a test." t nil)

(autoload 'jest-test-command "jest-test-mode" "\
Format test arguments for FILENAME.

\(fn FILENAME)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jest-test-mode" '("jest-test-")))

;;;***

;;;### (autoloads nil nil ("jest-test-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; jest-test-mode-autoloads.el ends here
