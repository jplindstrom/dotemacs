;;; impostman-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "impostman" "impostman.el" (0 0 0 0))
;;; Generated autoloads from impostman.el

(autoload 'impostman-parse-file "impostman" "\
Parse a file with a Postman collection.

COLLECTION is a Postman collection filename.
ENVIRONMENT is a Postman environment filename (optional).
OUTPUT-ALIST is an alist with the output callbacks.

\(fn COLLECTION ENVIRONMENT OUTPUT-ALIST)" nil nil)

(autoload 'impostman-parse-string "impostman" "\
Parse a string with a Postman collection.

COLLECTION is a string with a Postman collection.
ENVIRONMENT is a string with a Postman environment (optional).
OUTPUT-ALIST is an alist with the output callbacks.

\(fn COLLECTION ENVIRONMENT OUTPUT-ALIST)" nil nil)

(autoload 'impostman-import-file "impostman" "\
Import a file with a Postman collection.

COLLECTION is a Postman collection filename.
ENVIRONMENT is a Postman environment filename (optional).
OUTPUT-NAME is a string with the desired output (eg: \"verb\").

\(fn &optional COLLECTION ENVIRONMENT OUTPUT-NAME)" t nil)

(autoload 'impostman-import-string "impostman" "\
Import a string with a Postman collection.

COLLECTION is a string with a Postman collection.
ENVIRONMENT is a string with a Postman environment (optional).
OUTPUT-NAME is a string with the desired output (eg: \"verb\").

\(fn COLLECTION ENVIRONMENT &optional OUTPUT-NAME)" t nil)

(autoload 'impostman-version "impostman" "\
Return the Impostman version.

PRINT-DEST is the output stream, by default the echo area.

With \\[universal-argument] prefix, output is in the current buffer.

\(fn &optional PRINT-DEST)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "impostman" '("impostman-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; impostman-autoloads.el ends here
