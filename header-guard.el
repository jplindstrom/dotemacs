(load "toggle-header")

(defun header-guard-insert-line-at-beginning (str)
  (save-excursion
    (goto-char (point-min))
    (insert str)
    (newline)
    );;save
  );;defun

;;(header-guard-insert-line-at-beginning "test")

(defun header-guard-insert-line-at-end (str)
  (save-excursion
    (goto-char (point-max))
    (newline)
    (insert str)
    (newline)
    );;save
  );;defun

;;(header-guard-insert-line-at-end "test")


(defun header-guard-join-and-add (str-list char)
  (cond
   ((not (null str-list))
    (cond
     ((not (null (cdr str-list)))
      (concat (car str-list) (string char) (header-guard-join-and-add (cdr str-list) char))
      )
     ((null (cdr str-list))
      (concat (car str-list))
      )
     )
    )
   );;cond
  );;defun

;;(header-guard-join-and-add '("str") ?_)
;;"str"
;;(header-guard-join-and-add '("str" "txt") ?_)
;;"str_txt"

(defun header-guard-replace-dots (str)
  "Replaces '.' chars in a string with '_'."
  (header-guard-join-and-add (split-string str "[.]") ?_)
  );;defun

;;(header-guard-replace-dots "test.h")
;;"test_h"

(setq header-guard-prefix "HEADER_GUARD_")
(setq header-guard-suffix "")

(defun header-guard ()
  "Adds ifdef guards to a header file."
  (interactive)
  (let*
      ((file (buffer-file-name))
       (header-file (file-name-nondirectory file))
       )
    (cond
     ((toggle-header-is-of-suffixes file '(".h" ".hpp" ".H"))
      (let*
	  ((hdr-info (concat header-guard-prefix (header-guard-replace-dots (upcase header-file)) header-guard-suffix))
	   )
	(header-guard-insert-line-at-beginning (concat "#define " hdr-info))
	(header-guard-insert-line-at-beginning (concat "#ifndef " hdr-info))
	(header-guard-insert-line-at-end (concat "#endif //" hdr-info))
	);;let
      )
     );;cond
   );;let
  );;defun

;;(file-name-nondirectory (buffer-file-name))

;;(header-guard)



