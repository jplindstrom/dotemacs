;;; org-web-tools-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-web-tools" "org-web-tools.el" (0 0 0 0))
;;; Generated autoloads from org-web-tools.el

(autoload 'org-web-tools-insert-link-for-url "org-web-tools" "\
Insert Org link to URL using title of HTML page at URL.
If URL is not given, look for first URL in `kill-ring'.

\(fn URL)" t nil)

(autoload 'org-web-tools-insert-web-page-as-entry "org-web-tools" "\
Insert web page contents of URL as Org sibling entry.
Page is processed with `eww-readable'.

\(fn URL &key (CAPTURE-FN #\\='org-web-tools--url-as-readable-org))" t nil)

(autoload 'org-web-tools-read-url-as-org "org-web-tools" "\
Read URL's readable content in an Org buffer.
Buffer is displayed using SHOW-BUFFER-FN.

\(fn URL &key (SHOW-BUFFER-FN #\\='switch-to-buffer))" t nil)

(autoload 'org-web-tools-convert-links-to-page-entries "org-web-tools" "\
Convert links in current entry into entries containing linked pages' content.
Both plain links and Org bracket links are processed.  Page
content is processed with `eww-readable'.  All links in the
current entry (i.e. this does not look deeper in the subtree, nor
outside of it) will be converted." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-web-tools" '("org-web-tools-")))

;;;***

;;;### (autoloads nil "org-web-tools-archive" "org-web-tools-archive.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-web-tools-archive.el

(autoload 'org-web-tools-archive-attach "org-web-tools-archive" "\
Download archive of page at URL and attach with `org-attach'.
If CHOOSE-FN is non-nil (interactively, with universal prefix),
prompt for the archive function to use.  If VIEW is
non-nil (interactively, with two universal prefixes), view the
archive immediately after attaching.

\(fn URL &optional CHOOSE-FN VIEW)" t nil)

(autoload 'org-web-tools-archive-view "org-web-tools-archive" "\
Open Zip file archive of web page.
Extracts to a temp directory and opens with
`browse-url-default-browser'.  Note: the extracted files are left
on-disk in the temp directory." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-web-tools-archive" '("org-web-tools-a")))

;;;***

;;;### (autoloads nil nil ("org-web-tools-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-web-tools-autoloads.el ends here
