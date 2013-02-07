;;next: parse /etc/groups

;;; ec.el --- evening commander, yet another filemanager for emacs.

;; Copyright (C) 2001 Patrick Gundlach

;; Author: Patrick Gundlach <patrick@gundla.ch>
;; Created: 05 April 2001
;; Version: 0.96b4
;; Keywords: unix
;; Homepage: http://levana.de/emacs
;; Last Change: Tue Oct 16 16:13:53 2001
;; File Version: $Revision: 1.18 $

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; version 2

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; http://www.fsf.org/copyleft/gpl.html 


;;; Commentary:

;; Ok, you wanted some comments:
;; ec.el is my first emacs lisp program written. So don't lough too
;; loud about it when using it. Thanks.
;; this is a very beta version of ec.el. Please do not use this
;; filenmanager if you have data that you want to keep!!!! This
;; filemanager has probably a lot of bugs (and I know of some!) . So
;; the chance of data loss is rather high! 
;; If the previous sentences do not keep you from trying this code,
;; here are some notes:

;; Installation:
;; ============
;; Put ec.el somewhere in the loadpath of your one and only editor
;; (aka emacs) and put the following line in your .emacs:

;; (autoload 'ec "ec" "ec - evening commander." t nil)

;; (without the two ';;' certainly). Then start the evening commander
;; with 'M-x ec'.

;; Features:
;; ========

;; (write some features here :-) XXX

;; * full mouse support (incl. wheelmouse: button 4 and 5)  
;; * easy navigation with cursor keys, f2 and direct i-search
;; * context menu on right mouse button lets the user invoke 
;;   external programs such as the acrobat reader for pdf files


;; Todo: 
;; =====

;; since this is an early beta version, there is a *whole* lot
;; to do! I would be happy if you send me your ideas or even better,
;; just implement them! Search for XX if you want something to do!

;; see file ec.bugs!


;;; Code:

;;;; define some important and unimportand variables:

(defvar ec-actions 
  '(("\.dvi$" 
     [view "view" "xdvi %f"])
    ("\.pdf$" 
     [view "view" "acroread %f"])
    ("\.tex$" 
     [edit "edit"    ec-edit-file]
     [latex "LaTeX"   ec-nil]    ;; just to show that you can state
     [tex "TeX"     ec-nil]))    ;; many actions
  "Actions that can be done to a file. %f will match the current filename.")

    
(defvar ec-current-isearch-string nil 
  "String used for incremental search")

(defvar ec-use-scrollbars nil
  "If nil, no scrollbars are used")

(defvar ec-buffer-name-left "*ec-left*"
  "Name of left ec-buffer")
(defvar ec-buffer-name-right "*ec-right*"
  "Name of right ec-buffer")

(defvar ec-buffer-left nil
  "pointer to left ec-buffer")
(defvar ec-buffer-right nil
  "pointer to right ec-buffer")
(defvar ec-other-buffer nil
  "pointer to other buffer")

(defvar ec-mode-line-uid ""
  "UID (user id) (as a string) of the current file to be displayed in
the modeline") 
(defvar ec-mode-line-gid ""
  "GID (group id) (as a string) of the current file to be displayed in
the modeline")  
(defvar ec-mode-line-size ""
  "Size (as a string) of the current file to be displayed in the modeline")
(defvar ec-mode-line-mtime ""
  "Last modification time as a String  to be displayed in the
modeline")
(defvar ec-mode-line-perm ""
  "Permissions (like drwxr-xr-x) as a String to be displayed in the
modeline")
(defvar ec-mode-line-format 
  (list '(12 . ec-mode-line-mtime)
	" "
	'(10 . ec-mode-line-perm)
	" "
	'ec-mode-line-size)
  "Format of the mode line")
(defvar ec-buffer-side nil)
(defvar ec-cursor-overlay nil ;stolen from nc.el
  "Cursor'bar'")
(defvar ec-directoryv nil
  "vector holding current directory: 
1: name
2: overlay (can be removed?? XXX)
3: file-attributes
4: point
5: dir
6: file-or-symlink
")
(defvar ec-directory-without-dots-regexp
	;; explanation of regex (each part)
	;; 1: not a single dots                    (one char)
	;; 2: first not a dot and second not a dot (two chars)
	;; 3: first not a dot but second           (two chars)
	;; 4: first a dot but second not           (two chars)
	;; 5: or anything larger then two chars
	"\\(^[^\n.]$\\|^[^\n.][^\n.]$\\|^[^\n.]\\.$\\|^\\.[^\n.]$\\|^[^\n][^\n][^\n]+$\\)"
	"When used with directory-files, this eliminates the . and ..")
(defvar ec-directory-unshown-regexp "^\\.$")
(defvar ec-dotfile-show-regexp "^\\.\\b")
(defvar ec-file-unshown-regexp "" )
(defvar ec-first-file-offset 1
  "Line of first file. 0 means first line, 1 means second line...")
(defvar ec-history nil
  "History for ec-buffers")
(defvar ec-last-cursor-fileno nil
  "Filenumber last ec-show-cursor (and alike) was at")
(defvar ec-mode-hook nil
  "*List of functions to call when entering ec mode.")
(defvar ec-mode-map nil
  "Keymap for ec major mode.")
(defvar ec-original-scroll-bar nil 
  "User uses scrollbar in current frame. Used to reset the value")
(defvar ec-old-buffer nil
  "Original buffer user came from")
(defvar ec-selected-dirs nil
  "alist: directories (and overlays) selected in current dir")
(defvar ec-selected-files nil
  "alist: files (and overlays) selected in current dir")
(defvar ec-show-dotfiles-flag nil
  "*Tells ec to show files with a beginning period (aka hidden files)")
(defvar ec-window-configuration nil
  "pointer to window-configuration-object. This will be restored upon return") 

(defvar ec-number-of-files nil 
  "number of files in current directory")
(defvar ec-number-of-dirs nil 
  "number of subdirectories in current directory")
(defvar ec-number-of-files-and-dirs nil 
  "number of files and subdirectories in current directory")



;;; define some categories for displaying files dirs, symlinks etc.
(put 'ec-cat-cursor 'face 'secondary-selection); cursor `bar'
(put 'ec-cat-cursor 'priority 2) 
(put 'ec-cat-cursor 'evaporate t)

(put 'ec-cat-directory 'before-string "/") 
;;(put 'ec-cat-directory 'face 'bold) 
(put 'ec-cat-directory 'evaporate t) 

(put 'ec-cat-file 'before-string " ")
(put 'ec-cat-file 'evaporate t)
(put 'ec-cat-symdir 'before-string "~"); dir as symlink
(put 'ec-cat-symfile 'before-string "@"); file as symlink
(put 'ec-cat-special 'before-string "#"); special file
(put 'ec-cat-execute 'before-string "*"); executable file

(put 'ec-cat-selected 'face 'bold); selected files and dirs
(put 'ec-cat-selected 'priority 1) 
(put 'ec-cat-selected 'evaporate 1)

;; to have the file information on both sides different
(make-variable-buffer-local 'ec-mode-line-size)
(make-variable-buffer-local 'ec-mode-line-mtime)
(make-variable-buffer-local 'ec-mode-line-perm)

(make-variable-buffer-local 'ec-last-cursor-fileno)
(make-variable-buffer-local 'ec-cursor-overlay)
(make-variable-buffer-local 'ec-buffer-side)
(make-variable-buffer-local 'ec-last-line)
(make-variable-buffer-local 'ec-directoryv)
(make-variable-buffer-local 'ec-other-buffer)
(make-variable-buffer-local 'ec-selected-files)
(make-variable-buffer-local 'ec-selected-dirs)
(make-variable-buffer-local 'ec-number-of-files)
(make-variable-buffer-local 'ec-number-of-dirs)
(make-variable-buffer-local 'ec-number-of-files-and-dirs)
(make-variable-buffer-local 'ec-current-isearch-string)
;(make-variable-buffer-local 'ec-buffer-current-directory)




;;;;;;;;;; Keymap
;(if ec-mode-map
;    nil
(setq ec-mode-map (make-sparse-keymap))
;;(suppress-keymap ec-mode-map t)
;; bind keys "a-z" and "punctuation characters" to ec-isearch
;; see hexl.el for a perhaps better method
(let ((i 32) ;; don't match control chars
      (thischar))
  (while (< i 127);; emacs 20.6.1 seems to have a problem w/ M-". I
		  ;; don't know if this is a fix
    ;; leave out UPPER CASE LETTERS
    (if (or (< i 65)
	    (> i 90))
	(progn 
	  (setq thischar 
		;; word and symbol constituent
		(if (string-match "\\(\\sw\\|\\s_\\)" (char-to-string i))
		    (char-to-string i)
		  nil))
	  (if thischar
	      (define-key ec-mode-map thischar 'ec-isearch)
	    )))
    (setq i (1+ i))))
(define-key ec-mode-map [backspace] 'ec-delete-isearch-char)
(define-key ec-mode-map [f1] 'ec-help)
(define-key ec-mode-map [f2] 'ec-goto-directory-interactive) 
(define-key ec-mode-map [f3] 'ec-view-file)
(define-key ec-mode-map [f4] 'ec-edit-file)
(define-key ec-mode-map [f5] 'ec-copy-selected-files)
(define-key ec-mode-map [f6] 'ec-move-selected-files)
(define-key ec-mode-map [f7] 'ec-make-directory)
(define-key ec-mode-map [f8] 'ec-delete-selected-files)
(define-key ec-mode-map [f10] 'ec-quit)
(define-key ec-mode-map "/" 'ec-goto-directory-interactive) 
(define-key ec-mode-map ":" 'ec-toggle-display-dotfiles)
(define-key ec-mode-map [tab] 'ec-other-window)
(define-key ec-mode-map [insert] 'ec-select-unselect)
(define-key ec-mode-map [left] 'ec-up-directory)
(define-key ec-mode-map [right] 'ec-down-directory)
(define-key ec-mode-map [up] '(lambda () (interactive) (ec-next-line 1)))
(define-key ec-mode-map [down] 'ec-next-line)
(define-key ec-mode-map [return] 'ec-execute-file)
(define-key ec-mode-map [home] 'ec-goto-first-file)
(define-key ec-mode-map [end] 'ec-goto-last-file)
(define-key ec-mode-map "\e<" 'ec-goto-first-file)
(define-key ec-mode-map "\e>" 'ec-goto-last-file)
(define-key ec-mode-map [next] '(lambda () 
				  (interactive) 
				  (ec-scroll-down-some-files 7 1)))
(define-key ec-mode-map [prior] '(lambda () 
				   (interactive) 
				   (ec-scroll-down-some-files 7 -1)))
(define-key ec-mode-map [kp-add] 'ec-select-files-query)
(define-key ec-mode-map [kp-subtract] 'ec-deselect-files-query)
(define-key ec-mode-map [kp-multiply] 'ec-toggle-selected-files)
(define-key ec-mode-map "\C-r" 'ec-reread-directory)
(define-key ec-mode-map "\C-l" 'ec-restore-buffers)
(define-key ec-mode-map [mouse-5] '(lambda ()
				     (interactive)
				     (ec-mouse-scroll-down-some-files 1)))
(define-key ec-mode-map [mouse-4] '(lambda ()
				     (interactive)
				     (ec-mouse-scroll-down-some-files -1)))

(define-key ec-mode-map [mouse-1] 'ec-show-mouse)
(define-key ec-mode-map [down-mouse-1] '(lambda () (interactive)))
(define-key ec-mode-map [double-mouse-1] 'ec-show-mouse)
(define-key ec-mode-map [mouse-3] 'ec-context-menu)
(define-key ec-mode-map [down-mouse-3] 'ec-show-mouse)
;)

(defvar ec-rmb-menu (make-sparse-keymap "Action...")
  "Menu map for context menu")



;;;;;;;;;; All functions are here

;;;; first startup

;; make two buffers (ec-left and ec-right) , display only one, then
;; make two windows (split vertically) and display the other buffer in
;; the other window.



;;; main functions, like prepare buffer, quit, restore windows...

(defun ec ()
  "Start evening commander, a midnight commander clone. (Which is a norton commander clone)"
  (interactive)
 
  (easy-menu-define ec-mode-menu ec-mode-map
  "Menu for ec."
  '("Ec"
    ["Help" ec-help t]
    ["Change directory" ec-goto-directory-interactive t]
    ["View" ec-view-file t]
    ["Edit" ec-edit-file t]
    ["Copy" ec-copy-selected-files t]
    ["Move" ec-move-selected-files t]
    ["Make directory" ec-make-directory t]
    ["Delete" ec-delete-selected-files t]
    ["(Un)Show hidden files" ec-toggle-display-dotfiles t]
    ["Exit" ec-quit t]))

  ;; ok: first of all remember scroll bars, then 
  ;; create two empty buffers.
  (setq ec-original-scroll-bar (frame-parameter nil 'vertical-scroll-bars)) 
  (setq ec-old-buffer (buffer-name))
  (setq ec-buffer-left (ec-prepare-buffer ec-buffer-name-left 'left))
  (setq ec-buffer-right (ec-prepare-buffer ec-buffer-name-right 'right))
  (show-buffer (frame-selected-window) ec-buffer-left)
  (delete-other-windows)
  (split-window-horizontally)
  (display-buffer ec-buffer-right)
  (if ec-use-scrollbars 
      (toggle-scroll-bar 1)
    (toggle-scroll-bar -1)
    ) 
  (setq ec-other-buffer ec-buffer-right)
  (set-buffer ec-buffer-right)
  (setq ec-other-buffer ec-buffer-left)
  (ec-read-directory)
  (switch-to-buffer ec-buffer-left)
  (setq ec-window-configuration (current-window-configuration))
  (ec-read-directory)
  (ec-show-cursor (point))
  (add-hook 'window-configuration-change-hook 'ec-window-configuration-hook)
  )

(defun ec-prepare-buffer (bufname side)
  "Makes a new buffer called BUFNAME and updates local variable 
ec-buffer-side to SIDE. Returns pointer to created buffer."
  (get-buffer-create bufname)  
  (set-buffer bufname)
  (kill-all-local-variables)
  (setq major-mode 'ec-mode)
  (setq mode-name "Ec")
  (use-local-map ec-mode-map)
  (setq mode-line-format ec-mode-line-format)
  (setq ec-buffer-side side)
  (make-local-variable 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'ec-kill-buffer-hook)
  ;; (setq ec-last-line 1)
  
  (setq ec-cursor-overlay (make-overlay 0 0))
  (current-buffer)
  )

(defun ec-window-configuration-hook ()
  "Added viewing any buffer"
  (if (or (equal (buffer-name) "*ec-left*")
	  (equal (buffer-name) "*ec-right*")
	  )
      (progn 
	(remove-hook 
	 'window-configuration-change-hook 
	 'ec-window-configuration-hook)
	(ec-restore-buffers)) )
  )



(defun ec-quit ()
  "Quits ec, restores scrollbar"
  (interactive)
  (remove-hook 
   'window-configuration-change-hook 
   'ec-window-configuration-hook)
  (modify-frame-parameters (selected-frame) 
			   (list (cons 'vertical-scroll-bars ec-original-scroll-bar))) 
  (if (buffer-live-p ec-buffer-left)
      (kill-buffer ec-buffer-left))
  (if (buffer-live-p ec-buffer-right)
      (kill-buffer ec-buffer-right))
  (set-buffer ec-old-buffer)
  (delete-other-windows)
  )



;;; searching 
(defun ec-isearch ()
  (interactive)
  (let ((foundpoint nil)
	(thischar (this-command-keys)))
    (save-excursion
      (if ec-current-isearch-string
	  (beginning-of-line)
	(goto-char (point-min)))
      (if (re-search-forward 
	   (concat "^" (regexp-quote 
			(concat ec-current-isearch-string thischar)))
	   nil t)
	  (progn 
	    (setq ec-current-isearch-string 
		  (concat ec-current-isearch-string thischar))
	    (message ec-current-isearch-string)
	    (setq foundpoint (point))))
      )
    (if foundpoint
	(ec-show-cursor foundpoint))
    )
  )

(defun ec-delete-isearch-char ()
  "Removes the last character of the current isearch string"
  (interactive)
  (if (> (length ec-current-isearch-string) 1)
      (setq ec-current-isearch-string 
	    (substring ec-current-isearch-string 0 -1))
    (setq ec-current-isearch-string nil))
  (message ec-current-isearch-string)
  )

(defun ec-delete-isearch-string ()
  "Deletes entire current isearch string"
  (setq ec-current-isearch-string nil)
  )


;;; copy, move, delete (the look alike)
(defun ec-copy-file (source dest)
  "Copy files from list SOURCE to destination DEST. If only one 
source selected, dest may be dir or file."
  ;;(copy-file source dest)
  (message "Copy %s %s" (car source) dest)
  (copy-file (car source) dest t)
  )

(defun ec-copy-directories-recursively (source dest)
  "Copy all files and directories from SOURCE to DEST. SOURCE can be a
relative or absolute directory name. DEST must be a directory except
when source is only a single regular file."
  ;;(message "entering ec-copy-directories-recursively, dest= %s" dest)
  
  (if (file-exists-p dest)
      (if (not (file-directory-p dest))
	  (error "Destination not a directory"))
    (make-directory  (file-name-as-directory dest))
    (message "making dir %s" (file-name-as-directory dest)))
  (while (car source)
    (if (file-directory-p (car source))
	;; source is directory
	(ec-copy-directories-recursively 
	 (mapcar '(lambda (x) 
		    (concat (file-name-as-directory (car source)) x))
		 (directory-files (car source) nil 
				  ec-directory-without-dots-regexp))
	 (file-name-as-directory (concat 
				  (file-name-as-directory dest) 
				  (file-name-nondirectory (car source)))))
      ;; source not a directory
      (ec-copy-file (list (car source)) 
		    (concat (file-name-as-directory dest) 
			    (file-name-nondirectory (car source)))))
    (setq source (cdr source)))
  )

(defun ec-copy-selected-files ()
  "Copy (selected) files or dirs to dest."
  (interactive)
  ;; XXX the if clause should be done as a cond with a 'this cannot happen' fallback
  ;; 5 cases:
  ;; 1:
  ;; nothing selected, cursor on file or one file selected "copy file to"
  ;; 2:
  ;; nothing selected, cursor on dir or one dir selected "copy dir to"
  ;; 3: 
  ;; only dirs selected: "copy (num) selected dirs to"
  ;; 4:
  ;; only files selected: "copy (num) selected files to:"
  ;; 5:
  ;; some dirs and some files selected "copy (num) sel. files/directories to:"

  
  ;; case 2 and 1:
  ;;  (save-window-excursion		; for read-string
  (setq ec-window-configuration (current-window-configuration))
  (if (and (= (length ec-selected-files) 0)
	   (= (length ec-selected-dirs) 0))
      ;; one file/directory
      (let ((thisfile (ec-file-at-point))
	    (dest))
	(if (ec-fileorsymlink-directory-p thisfile)
	    ;; source is directory
	    (progn 
	      (setq dest (read-string 
			  (format "Copy directory `%s' to: " thisfile) 
			  (ec-dir-other-buffer) 'ec-history))

	      ;; ok dest does not exist (?) XXX and is not a dir
	      ;; -> create a dir and copy the contents of the
	      ;; source-dir to the dest-dir
	      (if (not (file-directory-p dest))
		  (make-directory dest))
		
	      (ec-copy-directories-recursively 	 
	       (mapcar '(lambda (x) 
			  (concat (file-name-as-directory thisfile) x))
		       (directory-files 
			thisfile nil ec-directory-without-dots-regexp))
	       dest))
	  ;; ok, source is a file
	  (setq dest (read-string 
		      (format "Copy file `%s' to: " thisfile)
		      (ec-dir-other-buffer) 'ec-history))
	  (if (file-directory-p dest)
		
	      ;; copy one regular file to dest directory
	      (ec-copy-file  (list thisfile)
			     (concat (file-name-as-directory  dest)
				     (file-name-nondirectory 
				      thisfile)))
	    ;; copy one file to dest file
	    (ec-copy-file (list thisfile)
			  dest)))))
  
  ;; case 3:
  (if (and (= (length ec-selected-files) 0)
	   (> (length ec-selected-dirs) 0))
      (let* ((num (length ec-selected-dirs))
	     (dest (read-string (format "Copy %d selected dirs? " num)
				(ec-dir-other-buffer) 'ec-history)))
	(ec-copy-directories-recursively 
	 (ec-num-to-files (ec-first ec-selected-dirs)) 
	 dest)))
  ;;(message "Copy %d selected dirs?" num)
  
    
  ;; case 4: (only files)
  (if (and (> (length ec-selected-files) 0)
	   (= (length ec-selected-dirs) 0))
      (let* ((num (length ec-selected-files))
	     (dest (read-string (format "Copy %d selected files? " num)
				(ec-dir-other-buffer) 'ec-history)))
	(ec-copy-directories-recursively 
	 (ec-num-to-files (ec-first ec-selected-files)) dest)))
  ;; case 5:
  (if (and (> (length ec-selected-files) 0)
	   (> (length ec-selected-dirs) 0))
      (let* ((num (+ (length ec-selected-files)
		     (length ec-selected-dirs)))
	     (dest (read-string (format "Copy %d selected dirs/files? " num)
				(ec-dir-other-buffer) 'ec-history)))
	(ec-copy-directories-recursively 
	 (ec-num-to-files (ec-first 
			   (append ec-selected-files ec-selected-dirs)))
	 dest)))
  (ec-reread-directory)    
  )

(defun ec-move-selected-files ()
  "Prompt for destdir and move (rename) existing files to destdir. 
If only one dir/file selected,  
DESTDIR does not have to be a directory; it can be just the target 
name. If multiple files are selected, DESTDIR must be an existing dir."
  (interactive)
  (ec-delete-isearch-string)
  (setq ec-window-configuration (current-window-configuration))
  ;; ok 5 cases:
  ;; 1:
  ;; nothing selected, cursor on file or one file selected "move file to"
  ;; 2:
  ;; nothing selected, cursor on dir or one dir selected "move dir to"
  ;; 3: 
  ;; only dirs selected: "move (num) selected dirs to"
  ;; 4:
  ;; only files selected: "move (num) selected files to:"
  ;; 5:
  ;; some dirs and some files selected "move (num) sel. files/directories to:"

  ;; case 2 and 1:
  (if (and (= (length ec-selected-files) 0)
	   (= (length ec-selected-dirs) 0))
      ;; one file/directory
      (let ((thisfile (ec-file-at-point)))
	(ec-rename-files 
	 (list thisfile)
	 (if (ec-fileorsymlink-directory-p thisfile)
	     (read-string "Move directory to: " 
			  (ec-dir-other-buffer) 'ec-history)
	   (read-string "Move file to: " 
			(ec-dir-other-buffer) 'ec-history )
	   ))))
  ;; case 3: (only dirs)
  (if (and (= (length ec-selected-files) 0)
	   (> (length ec-selected-dirs) 0))  
      (let ((num (length ec-selected-dirs))
	    (dest nil))
	(setq dest (read-string 
		    (format "Move %d selected directories to: " num) 
		    (ec-dir-other-buffer) 'ec-history))
	(if (file-directory-p dest)
	    (ec-rename-files 
	     (ec-num-to-files (ec-first ec-selected-dirs)) dest)
 	  (message "Destination %s does not exist" dest))))
  ;; case 4: (only files)
  (if (and (= (length ec-selected-dirs) 0)
	   (> (length ec-selected-files) 0))  
      (let ((num (length ec-selected-files))
	    (dest nil))
	(setq dest (read-string 
		    (format "Move %d selected files to: " num) 
		    (ec-dir-other-buffer) 'ec-history))
	(if (file-directory-p dest)
	    (ec-rename-files 
	     (ec-num-to-files (ec-first ec-selected-files)) dest)
	  (message "Destination %s does not exist" dest))))
  ;; case 5 (some files/dirs)
  (if (and (> (length ec-selected-files) 0)
	   (> (length ec-selected-dirs) 0))
      (let ((num (+ (length ec-selected-files)
		    (length ec-selected-dirs)))
	    (dest nil))
	(setq dest (read-string 
		    (format 
		     "Move %d selected files files/directories to: " num) 
		    (ec-dir-other-buffer) 'ec-history))
	(if (file-directory-p dest)
	    (progn (ec-rename-files 
		    (ec-num-to-files (ec-first ec-selected-files)) dest)
		   (ec-rename-files 
		    (ec-num-to-files (ec-first ec-selected-dirs)) dest))
	  (message "Destination %s does not exist" dest))))
  (ec-reread-directory)
  )




(defun ec-delete-directories-recursively (dirs &optional dontask)
  "Delete directories recursively. Delete all dirs listed in DIRS and
if DONTASK not nil it will remove them silently"
  (while (car dirs)
    (message "recursive deleting %s" (car dirs))
    (if (ec-fileorsymlink-directory-p (car dirs))
	;; directory
	(if (or (not (directory-files (car dirs) t
				      ec-directory-without-dots-regexp))
		dontask
		(y-or-n-p 
		 (format "Directory %s not empty. Delete it recursively? "
			 (car dirs))))
	    (progn 
	      (ec-delete-directories-recursively 
	       (directory-files (car dirs) t ec-directory-without-dots-regexp) t)
	      (delete-directory (car dirs))
	      ;;(message "delete directory %s" (car dirs))
	      ))
      ;; file
      (ec-delete-files (list (car dirs))))
    (setq dirs (cdr dirs)))
  )


(defun ec-delete-files (files)
  "Delete FILES in current directory. FILES is a list of files to be removed."
  (mapcar '(lambda (x)
	     (progn 
	       ;;(message "deleting file %s" x)
	       (delete-file x)))
	  files)
  )

(defun ec-delete-selected-files ()
  "Asks user if (selected) files or dirs should be deleted and if so it deletes 'em"
  (interactive)
  ;; XXX the if clause should be done as a cond with a 'this cannot happen' fallback
  ;; ok, there should be five cases:
  ;; 1: no files and no dirs selected, cursor on file
  ;;    ask: "Delete this file"  
  ;; 2: no files and no dirs selected, cursor on dir
  ;;    ask: "Delete this dir?"  
  ;; 3: no files only dirs selected: delete all dirs in ec-sel-dirs
  ;;    ask "delete (num) selected dirs?" 
  ;; 4: only files selected, no dirs, 
  ;;    ask "delete (num) selected files?"
  ;; 5: some files and some dirs selected
  ;;    ask "delete (num) selected dirs/files"
  (setq ec-window-configuration (current-window-configuration))
  ;; case 2 and 1:
  (if (and (= (length ec-selected-files) 0)
	   (= (length ec-selected-dirs) 0))
      ;; one file/directory
      (if (ec-fileorsymlink-directory-p (ec-file-at-point))
	  (if (y-or-n-p "Delete this directory? ")
	      (ec-delete-directories-recursively (list (ec-file-at-point))))
	(if (y-or-n-p "Delete this file? ")
	    (ec-delete-files (list (ec-file-at-point))))))
  
  ;; case 3:
  (if (and (= (length ec-selected-files) 0)
	   (> (length ec-selected-dirs) 0))
      (let ((num (length ec-selected-dirs)))
	(if (y-or-n-p (format "Delete %d selected dirs? " num))
	    (ec-delete-directories-recursively 
	     (ec-num-to-files (ec-first ec-selected-dirs))))))
  
  ;; case 4: (only files)
  (if (and (> (length ec-selected-files) 0)
	   (= (length ec-selected-dirs) 0))
      (let ((num (length ec-selected-files)))
	(if (y-or-n-p (format "Delete %d selected files? " num))
	    (ec-delete-files (ec-num-to-files (ec-first ec-selected-files))))))
  ;; case 5:
  (if (and (> (length ec-selected-files) 0)
	   (> (length ec-selected-dirs) 0))
      (let ((num (+ (length ec-selected-files)
		    (length ec-selected-dirs))))
	(if (y-or-n-p 
	     (format "Delete %d selected dirs/files?" num))
	    (progn
	      (ec-delete-files 
	       (ec-num-to-files (ec-first ec-selected-files)))
	      (ec-delete-directories-recursively 
	       (ec-num-to-files (ec-first ec-selected-dirs)))))))
  (ec-reread-directory)
  )


(defun ec-make-directory (dir)
  "Make directory DIR in current directory"
  (interactive "MDirectory to create: ")
  (ec-delete-isearch-string)
  (make-directory dir)
  (ec-reread-directory)
  (ec-goto-fileno (ec-whereis-filename  dir))
  )



;;; selection

(defun ec-deselect-files-query (pattern)
  "Ask user which files to deselect"
  (interactive "MDeselect files (regexp): ")
  ;; (message "selecting %s" pattern)
  ;; ok select only files (for now -> make it configurable)
  (let ((i 0)
	(name)
	(max ec-number-of-files-and-dirs))
    (while (< i max)
      (setq name (car (aref ec-directoryv i)))
      (if (and (not (ec-file-directory-p i))
	       (string-match pattern name))
	  (ec-unselect-file i))
      (setq i (1+ i))))
  )

(defun ec-directory-selected-p (filenumber)
  "Return t if directory FILENUMBER is selected"
  (if (assq filenumber ec-selected-dirs)
      t
    nil)
  )

(defun ec-unselect-file (filenumber) 
  "Unselect file or directory FILENUMBER"
  (if (or (ec-file-selected-p filenumber)
	  (ec-directory-selected-p filenumber))
      (let (overlay 
	    opoint)
	;; this file/dir is already selected
	;; delete overlay and entry from ec-selected-*
	(if (ec-fileorsymlink-directory-p filenumber)
	    (progn
	      ;; removing dir
	      (delete-overlay 
	       (car (cdr (assq filenumber ec-selected-dirs))))
	      (setq ec-selected-dirs 
		    (delete (assq filenumber ec-selected-dirs) 
			    ec-selected-dirs)))
	  (progn 
	    ;; removing file
	    (delete-overlay (car (cdr (assq filenumber ec-selected-files))))
	    (setq ec-selected-files 
		  (delete 
		   (assq filenumber ec-selected-files) ec-selected-files))))))
  
  )


(defun ec-toggle-selected-files ()
  "Toggle selection of files. Unselected files get selected, 
selected ones will be unselected. Works only for files right now"
  (interactive)
  (ec-delete-isearch-string)
  (save-excursion
    (let ((i 0)
	  (max ec-number-of-files-and-dirs ))
      (while (< i max)
	(if (not (ec-file-directory-p  i))
	    (ec-select-unselect i))
	(setq i (1+ i))
	)))
  (ec-show-cursor)
  )
(defun ec-select-unselect (&optional filenum)
  "Selects or unselects a file or directory"
  (interactive)
  (ec-delete-isearch-string)
  (let ((num (if filenum filenum
	       (ec-filenumber-at-point)))
	(opoint))
    (cond ( (> num 0)
	    (if (or (assq num ec-selected-files) 
		    (assq num ec-selected-dirs))
		(ec-unselect-file num)
	      
	      ;; not selected yet
	      ;; symlinks should be handled as files not dirs!
	      (ec-select-file num)))))
  (if (interactive-p)
      (ec-next-line))
    )

(defun ec-select-files-query (pattern)
  "Ask user which files to select"
  (interactive "MSelect files (regexp): ")
  (ec-delete-isearch-string)
  ;;(message "selecting %s" pattern)
  ;; ok select only files (for now -> make it configurable)
  (save-excursion
    (let ((i 0)
	  (name)
	  (max ec-number-of-files-and-dirs ))
      (while (< i max)
	(setq name (car (aref ec-directoryv i)))
	(if (and (not (ec-file-directory-p i))
		 (string-match pattern name))
	    (ec-select-file i))
	(setq i (1+ i)))))
  (ec-show-cursor)
  )


(defun ec-select-file (filenumber)
  "Select file number FILENUMBER"
  (if (not (or (ec-file-selected-p filenumber)
	       (ec-directory-selected-p filenumber)))
      (let ((opoint) 
	    (overlay))
	;; why do go to the file? There should be a nicer way?!?
	(ec-goto-fileno filenumber)
	(setq opoint (point))
	(end-of-line)
	(setq overlay (make-overlay opoint (point)))
	(overlay-put overlay 'category 'ec-cat-selected)
	(if (ec-fileorsymlink-directory-p filenumber)
	    (setq ec-selected-dirs
		  (append `((,filenumber ,overlay))
			  ec-selected-dirs))
	  (setq ec-selected-files 
		(append `((,filenumber ,overlay))
			ec-selected-files))))
	;(ec-goto-fileno filenumber)
    )
  )



;;; reading dirs and stuff like that
(defun ec-up-directory ()
  "go one higher XXX"
  (interactive)
  (ec-delete-isearch-string)
  (let ((dir-now (expand-file-name default-directory)))
    (setq default-directory (expand-file-name (concat default-directory "../")))
    ;;(message dir-now)
    (ec-read-directory)
    (re-search-forward 
     (concat "^" (regexp-quote (substring dir-now (string-match "[^/]*/$" dir-now) -1))))
    (beginning-of-line)
    (ec-show-cursor (point))
    ))







;;; helper functions

(defun ec-nil ()
  "This is for demonstration purpose only. I also could have said
'(lambda () (interactive)), but ec-nil is a more intuitive name. Not
really ment to be used."
  (interactive)
  )


(defun ec-first (arg)
  "ARG is a list of lists; return list of the first elts."
  (mapcar '(lambda (x) 
	     (car x))
	  arg)
  )


(defun ec-dir-other-buffer ()
  "Return default-directory of the other ec-buffer"
  (save-excursion
    (set-buffer ec-other-buffer)
    default-directory)
  )

(defun ec-down-directory ()
  "Read directory that is at the point."
  (interactive)
  (ec-delete-isearch-string)
  ;;check whether line is a directory line
  (beginning-of-line)
  (let ((destdir (expand-file-name 
		  (concat default-directory 
			  (ec-file-at-point)))))
    (if (file-directory-p destdir)
	(progn 
	  ;;  (setq ec-last-line  (locate-current-line-number))
	  (setq default-directory (file-name-as-directory destdir))
	  (ec-read-directory)
	  (ec-show-cursor (point))
	  (setq ec-last-cursor-fileno nil)
	  )
      ;;(message "nix")
      ))
  ) 
(defun ec-edit-file ()
  "Edit file at current point"
  (interactive)
  (ec-delete-isearch-string)
  ;;(message "ec-edit-file enter")
  (let* ((filename (ec-file-at-point))
	 (full-filename (expand-file-name 
			 (concat default-directory filename))))
    (if (not (file-directory-p full-filename))
	(progn
	  (setq ec-window-configuration (current-window-configuration))
	  (remove-hook 
	   'window-configuration-change-hook 
	   'ec-window-configuration-hook)
	  (delete-other-windows)
	  (find-file full-filename)
	  (add-hook 'window-configuration-change-hook 
		    'ec-window-configuration-hook))
      ))
  ;; (message "ec-edit-file exit")
  )



(defun ec-file-directory-p (filenumber)
  "Return t if directory or nil if file"
  (nth 4 (aref ec-directoryv filenumber))
  )

(defun ec-fileorsymlink-directory-p (file)
  "Return t if FILE directory or nil if (file or symlink). 
FILE is a string or a filenumber."
  (if (stringp file)
      (if (and (file-directory-p file)
	       (not (file-symlink-p file)))
	  t
	nil)
    (nth 5 (aref ec-directoryv file)))
  )

(defun ec-file-at-point ()
  "Return file or directory (or whatever) name where cursor is at"
  (car (aref ec-directoryv (ec-filenumber-at-point)))
  )

(defun ec-filename-at-number (num)
  "Return file or directory (or whatever) name where cursor is at"
  (car (aref ec-directoryv num))
  )

(defun ec-filenumber-at-point ()
  "Return the array position of current file"
  (save-excursion
    (beginning-of-line)
    (let ((opoint (point)))
      (goto-char (point-min))
      (- (count-lines (point) opoint) ec-first-file-offset)))
  )


(defun ec-file-lists (directory &optional pattern)
  "Create file lists for DIRECTORY.
The car is the list of directories, the cdr is list of files.
If PATTERN is set, return only directories and files matching PATTERN"
  ;; see speedbar-file-lists. add function to set ignore headers XXX
  
  (setq directory (expand-file-name directory))
  ;; ec -> nice feature XXX
  ;; find the directory, either in the cache, or build it.
  ;;(or (cdr-safe (assoc directory speedbar-directory-contents-alist))
  (let ((default-directory directory)
	(dir (directory-files directory nil pattern))
	(dirs nil)
	(files nil)
	(show-dotfiles-regexp (if ec-show-dotfiles-flag "\\`\\'"
				ec-dotfile-show-regexp)))
    (while dir
      (if (not
	   (or				
	    (string-match show-dotfiles-regexp (car dir))
	    (string-match ec-directory-unshown-regexp (car dir))))
	  (if (file-directory-p (car dir))
	      (setq dirs (cons (car dir) dirs))
	    (setq files (cons (car dir) files))))
      (setq dir (cdr dir)))
    (let ((nl (cons (nreverse dirs) (list (nreverse files)))))
      ;;(aput 'speedbar-directory-contents-alist directory nl)
      nl))
  )

(defun ec-file-selected-p (filenumber)
  "Return t if file FILENUMBER is selected"
  (if (assq filenumber ec-selected-files)
      t
    nil)
  )
  

(defun ec-goto-directory-interactive ()
  "Wrapper for ec-goto-directory, because window-configuration 
should be saved first."
  (interactive)
  (ec-delete-isearch-string)
  (setq ec-window-configuration (current-window-configuration))
  (call-interactively 'ec-goto-directory)
  )

(defun ec-goto-directory (dir)
  "Set default-directory to DIR and reread dir"
  (interactive "DSwitch to directory: ")
  (ec-delete-isearch-string)
  (setq default-directory dir)
  (ec-read-directory)
  )
(defun ec-goto-first-file ()
  "Goto the first file displayed"
  (interactive)
  (ec-delete-isearch-string)
  (ec-goto-fileno 0 t)
  )

(defun ec-goto-last-file ()
  "Set the cursor to the last file displayed"
  (interactive)
  (ec-delete-isearch-string)
  (ec-goto-fileno (- ec-number-of-files-and-dirs 1) t)
  )


(defun ec-goto-fileno (number &optional showme)
  "Set cursor to filenumber NUMBER."
  
  ;; don't step over bounds
  (let ((realnum (cond ( (>= number ec-number-of-files-and-dirs) 
			 (1- ec-number-of-files-and-dirs))
		       ( (< number 0 ) 0)
		       ( t number))))
    (goto-char (nth 3 (aref ec-directoryv realnum )))
    ;; for speed issues: ec-select-file uses ec-goto-fileno.
    (if showme
	(progn
	  (ec-show-cursor (point))
	  (ec-refresh-modeline realnum))))
  )

(defun ec-refresh-modeline (fileno)
  "Refresh the modeline. FILENO is the number of file thats attributes
should be printed in the modeline. The mode line is not really
refreshed. The attributes are just copied to the variables
ec-mode-line-xxx." 
  (let ((attribs (nth 2 (aref ec-directoryv fileno)))) ; file-attributes
    (setq ec-mode-line-uid  (user-login-name (nth 2 attribs)))
    ;;    (setq ec-mode-line-gid  (user-login-name (nth 2 attribs)))
    ;; XXX parse /etc/groups (if exists) and put info in an array
    (setq ec-mode-line-mtime (format-time-string 
			      "%b %e %y %k:%M:%S"
			      (nth 5 attribs)))
    (setq ec-mode-line-perm (nth 8 attribs ))
    (setq ec-mode-line-size (format 
			     "%9d"  
			     (nth 7 attribs))))
  )


(defun ec-help ()
  "Display some help on how to use ec"
  (interactive)
  (ec-delete-isearch-string)
  (message "F2: chdir F3: view F4: edit F5: copy F6: move F7: mkdir F8: delete F10: exit")
  )


(defun ec-kill-buffer-hook ()
  "kill-buffer should call ec-quit"
  (remove-hook
   'kill-buffer-hook 'ec-kill-buffer-hook)
  (ec-quit)
  )





;;;; actions and menus

(defun ec-action (vec &optional filename) 
  "Not documented yet XX"
  (let ((action (aref (car vec) 2))
	(thisfile (if filename filename (ec-file-at-point))))
    ;;make cond or sth. else instead of nested if clauses XXX
    (if (stringp action)
	(with-temp-buffer
	  (insert action)
	  (goto-char (point-min))
	  (while (search-forward "%f" nil t)
	    (replace-match thisfile nil t))
	  (apply 'start-process "external" nil (split-string 
			   (buffer-substring (point-min) (point-max)))))
      (if (commandp action)
	  (command-execute 
	   action)))
    )
  )


(defun ec-context-menu (&rest)
  "Display context menu for file at rmb invocation"
  (interactive)
  (let* ((thisfile (ec-file-at-point))
	 (firstaction (ec-default-action thisfile t))
	 (lastactions (nreverse (copy-sequence (cdr (ec-default-action thisfile)))))
	 (map (make-sparse-keymap))
	 (allactions)
	 result)
    (set-keymap-parent map ec-rmb-menu)
    
    ;; check everything in ec-actions if the current file is something
    ;; worth displaying
    (while (car lastactions)
      (define-key map (vector (aref (car lastactions) 0))
	(cons (aref (car lastactions) 1)
	      (aref (car lastactions) 2)))
      (setq lastactions (cdr lastactions)))
    
    (define-key map (vector (aref (car firstaction) 0))
      (cons (aref (car firstaction) 1)
	    'ec-execute-file))
    (setq result  (car (x-popup-menu t (list map))))
    (if result
	(progn
	  (setq allactions  (ec-default-action thisfile))
	  (while (car allactions)
	    (if (equal (aref (car allactions) 0 ) result)
		(progn (ec-action (list (car allactions)) thisfile)
		       (setq allactions nil)))
	    (setq allactions (cdr allactions))))))
  )

(defun ec-execute-file ()
  "Opens a directory, edits a file or for example views a pdf file"
  (interactive)
  (let ((thisfile (ec-file-at-point)))
    (ec-action (ec-default-action thisfile t) thisfile)) 
  )

(defun ec-default-action (file &optional onlyfirst) 
  "blaXXX"
  (let ((actions ec-actions)
	(menuentries)
	(ret (list (vector 'edit "edit" 'ec-edit-file))))
    ;; XX
    (if (file-directory-p file)
	(list (vector 'down "Down directory" 'ec-down-directory))
      (while (car actions)
	(if (string-match (car (car actions)) file)
	    ;; ok, I have found the file type. 
	    (if onlyfirst
		(setq ret (list (car (cdr (car actions)))))
	      (setq ret 
		    (cdr (car actions)))
	      ))
	(setq actions (cdr actions))
	)
      ret)
    ))
(defun ec-mouse-scroll-down-some-files (dir)
  "Call ec-scroll-down-some-files with arg"
  (ec-delete-isearch-string)
  (ec-scroll-down-some-files 14 dir)
  )

(defun ec-next-line (&optional arg) 
  "move cursor one line down or up and show nice cursor. If (optional)
ARG non-nil, move up one line."
  (interactive)
  (ec-delete-isearch-string)
  (ec-goto-fileno (+ (ec-filenumber-at-point)
		     (if arg -1 1)) t)
;;  (forward-line)
;;  (ec-show-cursor (point))
  )
(defun ec-previous-line () 
  "move cursor one line up and shows nice cursor"
  (interactive)
  (ec-delete-isearch-string)
  (forward-line -1)
  (ec-show-cursor (point))
  )



(defun ec-number-of-files (&optional maxonly)
  "Return a list of total number of directories and files and 
number of direcotries and number of files in current directory. 
For example: (15 10 5) means that there are 15 dirs/files totally,  
10 subdirectories and 5 files 
in the current direcotry. WARNING: it includes .."
  (if maxonly 
      (length ec-directoryv)
    (let ((dirs 0)
	  (files 0)
	  (i 0))
      (while (< i (length ec-directoryv))
	(if (ec-fileorsymlink-directory-p i)
	    (setq dirs (1+ dirs))
	  (setq files (1+ files)))
	(setq i (1+ i)))
      (list (+ dirs files) dirs files)))
  )
	
	     

(defun ec-num-to-files (arg)
  "Read list of numbers and return list of corresponding filenames."
  (let ((mylist))
    (mapcar '(lambda (x)
	       (setq mylist 
		     (append mylist 
			     (list (car (aref ec-directoryv x)))))) arg )
    mylist)
  )

(defun ec-other-window ()
  "remove cursor and switch to other window"
  (interactive)
  (ec-delete-isearch-string)
  (ec-delete-cursor)
  (other-window 1)
  (ec-show-cursor (point))
  )




(defun ec-rename-files (source dest)
  "Move all files listed in SOURCE to DEST."
  (if (= (length source) 1)
      ;; ok, target needs not exist, if it exists conact source to dest
      (if (file-directory-p dest)
	  (rename-file (car source) 
		       (concat (file-name-as-directory dest) (car source)))
	(rename-file (car source) dest))
    ;; many sources:
    ;; target must be a directory
    (if (file-directory-p dest)
	;; ok
	(mapcar '(lambda (x)
		   (ec-rename-files (list x) dest)) source)
      )))
(defun ec-restore-buffers (&optional buf)
  "Shows the two ec buffers horizontally and delete buf if not nil"
  (interactive)
  (ec-delete-isearch-string)
  (set-window-configuration ec-window-configuration)
  ;;(message "ec-restore-buffers: restored window-configuration")
  (if (buffer-live-p  buf)
      (kill-buffer buf))
  (if ec-use-scrollbars 
      (toggle-scroll-bar 1)
    (toggle-scroll-bar -1)
    )
  (add-hook 'window-configuration-change-hook 'ec-window-configuration-hook)
  )


(defun ec-reread-directory ()
  "Rereads and redisplays current directory. Should keep selected files selected. (Todo)"
  (interactive)
  (ec-delete-isearch-string)
  (setq ec-window-configuration (current-window-configuration))
  (let ((thisfile (ec-filenumber-at-point)))
    (ec-read-directory)
    (save-excursion
      (set-buffer ec-other-buffer)
      (ec-read-directory)
      )
    (ec-goto-fileno thisfile t))
  )



(defun ec-read-directory ()
  "Read directory which is in local variable default-directory and
 display it after deleting content of buffer."

  ;; suppress bad programming :->>
  (setq default-directory (file-name-as-directory default-directory))
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max))
  (setq ec-selected-files nil)
  (setq ec-selected-dirs nil)
  (let* ((dirlist (ec-file-lists default-directory))
	 (files (car (cdr dirlist)))
	 (dirs (car dirlist))
	 (overlay nil)
	 (beg)
	 (end)
	 (i 0)
	 (count (+ (length dirs)
		   (length files))))
    (setq ec-directoryv (make-vector count nil))
    (insert "--> " default-directory "\n")
    (while dirs
      (progn
	(setq beg (point))
	(insert (car dirs))
	(setq end (point))
	(insert "\n")
	(setq overlay (make-overlay beg end))
	(overlay-put overlay 'category 'ec-cat-directory)
	;; put in vector
	;; 1: name, 2: overlay, 3: file-attributes, 4: point, 5: dir, 
	;; 6: file-or-symlink
	(aset ec-directoryv i 
	      (list (car dirs) 
		    overlay  
		    (file-attributes (car dirs)) 
		    beg 
		    t 
		    (if (file-symlink-p (car dirs)) nil t)))
	(setq i (1+ i))
	(setq dirs (cdr dirs))))
    (while files
      (progn
	(setq beg (point))
	(insert (car files))
	(setq end (point))
	(insert "\n")
	(setq overlay (make-overlay beg end))
	(overlay-put overlay 'category 'ec-cat-file)
	;; put in vector
	(aset ec-directoryv i (list (car files) overlay  (file-attributes (car files)) beg nil nil ))

	(setq i (1+ i))
	(setq files (cdr files))))
    (setq buffer-read-only t)

    (setq i 0)
    (setq ec-number-of-files 0)
    (setq ec-number-of-dirs 0)
    (setq ec-number-of-files-and-dirs 0)
    (while (< i  (length ec-directoryv))
      (if (ec-fileorsymlink-directory-p i)
	  (setq  ec-number-of-dirs (1+ ec-number-of-dirs))
	(setq ec-number-of-files (1+ ec-number-of-files)))
      (setq i (1+ i)))
    (setq ec-number-of-files-and-dirs i))
  (ec-goto-first-file)
  )



(defun ec-show-cursor (&optional pos)
  "Show nice overlay-cursor. If pos is given, then display it at pos,
else at (point)"
  (if pos (goto-char pos))
  (if (< (ec-filenumber-at-point) 0)
      (ec-goto-fileno 0)) ; are your really sure what you're doing?
			  ; (pg) XXX
  (if (>= (ec-filenumber-at-point) ec-number-of-files-and-dirs)
      (ec-goto-fileno (- (length ec-directoryv) 1)))
  (save-excursion 
    (beginning-of-line)
    (move-overlay ec-cursor-overlay 
		  (point)
		  (progn
		    (forward-line)
		    (beginning-of-line)
		    (point)))
    (set-buffer ec-other-buffer)
    (ec-delete-cursor))  
  (overlay-put ec-cursor-overlay  'category 'ec-cat-cursor)
  (setq ec-last-cursor-fileno (ec-filenumber-at-point))
  (ec-refresh-modeline (ec-filenumber-at-point))
  )


(defun ec-show-mouse () 
  "Set point (and cursor) to position where mouse click was"
  (interactive)
  (ec-delete-isearch-string)
  ;;(message "show mouse")
  (mouse-set-point last-nonmenu-event)
  (if (equal ec-last-cursor-fileno (ec-filenumber-at-point))
      (ec-execute-file)
    (ec-show-cursor))
  
  ) 
  


(defun ec-whereis-filename (name)
  "Return number of file NAME. Searches for a string and looksup in 
internal table."
  (let (( i 0 )
	(fileno nil)); filenumber to return
    (while (< i ec-number-of-files-and-dirs)
      (if (equal (car (aref ec-directoryv i)) name)
	  (progn 
	    (setq fileno i)
	    (setq i ec-number-of-files-and-dirs ))
	(setq i (1+ i))))
    fileno)
  )
	
(defun ec-view-file ()
  "View file at current point"
  (interactive)
  (ec-delete-isearch-string)
  (let* ((filename (ec-file-at-point))
	 (full-filename 
	  (expand-file-name (concat default-directory filename))))
    (if (not (file-directory-p full-filename))
	(progn
	  ;; view-mode-enter sets fourth arg to kill-hook,
	  ;; so make it call restore buffers and delete the one
	  ;; visited. Cool!
	  ;; save window-configuration first.
	  (remove-hook 
	   'window-configuration-change-hook 
	   'ec-window-configuration-hook)
	  (setq ec-window-configuration (current-window-configuration))
	  ;; makes scroll bar visible (positive argument)
	  (toggle-scroll-bar 1)
	  (delete-other-windows)
	  (find-file full-filename)
	  (view-mode-enter
	   (cons (get-buffer-window full-filename) 
		 (cons (selected-window) nil))
	   '(lambda (visited-buffer)(ec-restore-buffers visited-buffer)))
	  (add-hook 'window-configuration-change-hook 
		    'ec-window-configuration-hook))
      ))
  )



(defun ec-toggle-display-dotfiles ()
  "Toggles the display of dotfiles (e.g. '.vi') in both buffers,
ec-left and ec-right. When ec-show-dotfiles-flag is not nil, set it to
nil and the other way."
  (interactive)
  (setq ec-show-dotfiles-flag 
	(if ec-show-dotfiles-flag nil
	  t))
  (ec-reread-directory)
  )


;;; windowing stuff, showing cursor

(defun ec-delete-cursor ()
  "deletes cursor in current window"
  (delete-overlay ec-cursor-overlay)
  )


;;; movement

;; this is obsolete, isn't it? (see ec-sc-do-so-fi) --pg 10/2001
(defun ec-scroll-up-some-files (&optional ratio)
  "Set cursor some files closer to the end. XXX (explain more details)"
  (interactive)
  (ec-delete-isearch-string)
  (let ((howmuch (if ratio ratio 7)))
    (ec-goto-fileno (+ (ec-filenumber-at-point) 
		       (/ (- ec-number-of-files-and-dirs 1) howmuch) 1) t))
  )

(defun ec-scroll-down-some-files (&optional ratio dir)
  "Set cursor some files closer to the beginning/end. DIR is the direction,  negative means cursor towards the first file, postive or nil means cursor closer to the end. If the RATIO is x, you have to invoke this command x-times to move to the end (beginning) of the file list, if you are at th beginning (end). If RATIO is 2, the first invocation goes to the middle of the list, the second to the end (beginning)."
  (interactive) 
  (ec-delete-isearch-string)
  (let ((howmuch (if ratio ratio 7))
	(mult    (if (>= dir 0)  1 -1))); mult is positive if cursor
					; goes down  

    ;; looking at this a day after I have written this makes me throw
    ;; up...
    ;; Patrick, you have to make this clearer.... --pg 10/2001
    (ec-goto-fileno (+ (ec-filenumber-at-point) 
		       (* mult (/ (- ec-number-of-files-and-dirs 1) 
				  howmuch)) 
		       mult)
		    t))
  )



;;; Change Log: 

;;; ec.el ends here
