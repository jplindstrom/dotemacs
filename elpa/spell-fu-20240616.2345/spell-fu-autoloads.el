;;; spell-fu-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "spell-fu" "spell-fu.el" (0 0 0 0))
;;; Generated autoloads from spell-fu.el

(put 'spell-fu-buffer-session-localwords 'safe-local-variable #'spell-fu-list-of-strings-p)

(autoload 'spell-fu-list-of-strings-p "spell-fu" "\
Return t when OBJ is a list of strings.

\(fn OBJ)" nil nil)

(autoload 'spell-fu-buffer-session-localwords-update "spell-fu" "\
Refresh after changing `spell-fu-buffer-session-localwords'." nil nil)

(autoload 'spell-fu-mode "spell-fu" "\
Toggle variable `spell-fu-mode' in the current buffer.

If called interactively, enable Spell-Fu mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'spell-fu-global-mode 'globalized-minor-mode t)

(defvar spell-fu-global-mode nil "\
Non-nil if Spell-Fu-Global mode is enabled.
See the `spell-fu-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `spell-fu-global-mode'.")

(custom-autoload 'spell-fu-global-mode "spell-fu" nil)

(autoload 'spell-fu-global-mode "spell-fu" "\
Toggle Spell-Fu mode in all buffers.
With prefix ARG, enable Spell-Fu-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Spell-Fu mode is enabled in all buffers where
`spell-fu--mode-turn-on' would do it.
See `spell-fu-mode' for more information on Spell-Fu mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "spell-fu" '("spell-fu-")))

;;;***

;;;### (autoloads nil nil ("spell-fu-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; spell-fu-autoloads.el ends here
