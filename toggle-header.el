(defun toggle-header-map-first (f alist) 
  "Calls f on each list member, and returns when the first function returns true."
  (catch 'found 
    (cond
     ((listp alist)
      (let
	  ((head (car alist))
	   (tail (cdr alist))
	   )
	(cond 
	 ((not (null 'head))
	  (let* 
	      ((ret (funcall f head)))
	    (cond 
	     ((not (boundp ret))
	      (cond
	       ((not (eq tail nil))
		(toggle-header-map-first f tail)
		)
	       )
	      )
	     ((eq ret t)
	      (throw 'found head)
	      )
	     ((eq ret nil)
	      (cond
	       ((not (eq tail nil))
		(toggle-header-map-first f tail)
		)
	       )
	      )
	     )
	    )
	  )
	 )
	) ;;let
      ) ;;listp
     ) ;;cond
    );;catch
  ) ;;defun

;;(toggle-header-map-first 'file-exists-p '("test" "/bin/sh"))
;;"/bin/sh"
;;(toggle-header-map-first 'file-exists-p '("test" "/foo/bar"))
;;nil

(defun toggle-header-find-last-dot-index (file)
  "Returns the index of the last dot in a path/file. Returns 0 if not found."
  (catch 'result
    (let*
	((num (- (length file) 1)))
      (while (> num 0)
	(cond 
	 ((string-equal (char-to-string (elt file num)) ".")
	  (throw 'result num)
	  )
	 )
	(setq num (- num 1))
	);;while
      );;let
    (throw 'result -1)
    );;catch
  );;defun

;;(toggle-header-find-last-dot-index "/boot/home/test.txt")
;;15
;;(toggle-header-find-last-dot-index "/boot/home/testtxt")
;;-1

(defun toggle-header-is-of-suffix (file suffix)
  "Takes a file and a suffix and checks whether the file has that suffix or not."
  (catch 'result
    (cond 
     ((string-equal (substring file (toggle-header-find-last-dot-index file)) suffix)
      (throw 'result t)
      )
     );;cond
    );;catch
  );;defun

;;(toggle-header-is-of-suffix "/boot/home/test.txt" ".txt")
;;t
;;(toggle-header-is-of-suffix "/boot/home/test.txt" ".zip")
;;nil
;;(toggle-header-is-of-suffix "/boot/home/testtxt" ".txt")
;;nil

(defun toggle-header-is-of-suffixes (file suffix-list)
  "Returns true if file has any of the suffixes in the suffix, else nil."
  (catch 'result
    (cond
     ((listp suffix-list)
      (cond 
       ((not (null (toggle-header-map-first '(lambda (suffix) (toggle-header-is-of-suffix file suffix)) suffix-list)))
	(throw 'result t)
	)
       );;cond
      );;listp
     ) ;;cond
    );;catch 
  );;defun

;;(toggle-header-is-of-suffixes "test.txt" '(".zip" ".h" ".txt"))
;;t
;;(toggle-header-is-of-suffixes "foo.bar" '(".zip" ".h" ".txt"))
;;nil

(defun toggle-header-switch-file-suffix (file suffix)
  "Takes a file and changes the file suffix."
  (let*
      ((index (toggle-header-find-last-dot-index file)))
    (cond 
     ((or (= index 0) (> index 0))
      (concat (substring file 0 index) suffix)
      )
     ((< index 0)
      (concat file suffix)
      )      
     );;cond
    );;let
  );;defun

; (toggle-header-switch-file-suffix "test.txt" ".h")
; "test.h"
; (toggle-header-switch-file-suffix "test.zip" ".h")
; "test.h"
; (toggle-header-switch-file-suffix "test" ".h")


(defun toggle-header-create-file-list (file suffixes)
  "Takes a file and a suffix list and create a list of files where the suffix of file has been changed to the suffixes in the list."
  (cond
   ((not (null suffixes))
    (cons (toggle-header-switch-file-suffix file (car suffixes)) (toggle-header-create-file-list file (cdr suffixes)))
    )
   );;cond
  );;defun

;;(toggle-header-create-file-list "test.txt" '(".h" ".cpp" ".zip"))
;;("test.h" "test.cpp" "test.zip")

(defun toggle-hdr-src ()
  "Switches to headerfile or sourcefile, depending on which is currently open."
  (interactive)
  (let*
      ((file (buffer-file-name))
       (src-suffixes '(".c" ".cpp" ".C" ".cp" ".cc"))
       (hdr-suffixes '(".h" ".hpp" ".H"))
       )
    (cond
     ((toggle-header-is-of-suffixes file src-suffixes)
      (let*
	  ((newfile (toggle-header-map-first 'file-exists-p (toggle-header-create-file-list file hdr-suffixes)))
	   )
	(cond
	 ((not (null newfile))
	  (find-file newfile)
	  )
	 )
	)
      )
     ((toggle-header-is-of-suffixes file hdr-suffixes)
      (let*
	  ((newfile (toggle-header-map-first 'file-exists-p (toggle-header-create-file-list file src-suffixes)))
	   )
	(cond
	 ((not (null newfile))
	  (find-file newfile)
	  )
	 )
	)
      )
     ) ;;cond
    ) ;;let
  ) ;;defun
