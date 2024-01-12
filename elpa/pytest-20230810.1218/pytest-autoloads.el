;;; pytest-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pytest" "pytest.el" (0 0 0 0))
;;; Generated autoloads from pytest.el

(autoload 'pytest-all "pytest" "\
Run all tests.
Optional argument FLAGS pytest command line flags.

\(fn &optional FLAGS)" t nil)

(autoload 'pytest-failed "pytest" "\
Quit test suite on first failed test." t nil)

(autoload 'pytest-pdb-all "pytest" "\
Start pdb on error." t nil)

(autoload 'pytest-last-failed "pytest" "\
Run tests that failed last time.
Optional argument FLAGS pytest command line flags.

\(fn &optional FLAGS)" t nil)

(autoload 'pytest-pdb-last-failed "pytest" "\
Run tests that failed last time, enger debugger on error." t nil)

(autoload 'pytest-directory "pytest" "\
Run pytest on all the files in the current buffer.
Optional argument FLAGS pytest command line flags.

\(fn &optional FLAGS)" t nil)

(autoload 'pytest-pdb-directory "pytest" "\
Run pytest on all the files in the current buffer.
Optional argument FLAGS pytest command line flags.

\(fn &optional FLAGS)" t nil)

(autoload 'pytest-module "pytest" "\
Run pytest (via eggs/bin/test) on current buffer.
Optional argument FLAGS pytest command line flags.

\(fn &optional FLAGS)" t nil)

(autoload 'pytest-pdb-module "pytest" "\
Run pytest on a module, enter debugger on error." t nil)

(autoload 'pytest-one "pytest" "\
Run pytest (via eggs/bin/test) on testable thing at point in current buffer.
Optional argument FLAGS pytest command line flags.

\(fn &optional FLAGS)" t nil)

(autoload 'pytest-pdb-one "pytest" "\
Run pytest on testable thing at point, enter debugger on error." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pytest" '("pytest-")))

;;;***

;;;### (autoloads nil nil ("pytest-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pytest-autoloads.el ends here
