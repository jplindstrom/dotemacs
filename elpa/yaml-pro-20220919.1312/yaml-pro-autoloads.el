;;; yaml-pro-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "yaml-pro" "yaml-pro.el" (0 0 0 0))
;;; Generated autoloads from yaml-pro.el

(autoload 'yaml-pro-mode "yaml-pro" "\
Binds additional functions to aid in editing YAML files.

If called interactively, enable Yaml-Pro mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\\{yaml-pro-mode-map}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yaml-pro" '("yaml-pro-")))

;;;***

;;;### (autoloads nil "yaml-pro-edit" "yaml-pro-edit.el" (0 0 0 0))
;;; Generated autoloads from yaml-pro-edit.el

(autoload 'yaml-pro-edit-scalar "yaml-pro-edit" "\
Edit the scalar value at the point in a separate buffer.
If prefix argument P is provided, prompt user for initialization command.

\(fn P)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "yaml-pro-edit" '("yaml-pro-edit-")))

;;;***

;;;### (autoloads nil nil ("yaml-pro-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; yaml-pro-autoloads.el ends here
