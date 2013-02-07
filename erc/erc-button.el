;;; erc-button.el --- A way of buttonizing certain things in ERC buffers

;; Copyright (C) 2002  Mario Lang

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: irc, button, url, regexp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Heavily borrowed from gnus-art.el. Thanks to the original authors.

;;; Code:

(require 'erc)
(require 'wid-edit)

;;; Variables

(defcustom erc-button-face 'bold
  "Face used for highlighting buttons in ERC buffers.

An button is a piece of text that you can activate by pressing
`RET' or `mouse-2' above it. See also `erc-button-keymap'."
  :type 'face
  :group 'erc-faces)

(defcustom erc-button-mouse-face 'highlight
  "Face used for mouse highlighting in ERC buffers.

Buttons will be displayed in this face when the mouse cursor is
above them."
  :type 'face
  :group 'erc-faces)

(defcustom erc-button-url-regexp "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\)\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?\\([-a-zA-Z0-9_=!?#$@~`%&*+|\\/:;.,]\\|\\w\\)+\\([-a-zA-Z0-9_=#$@~`%&*+|\\/]\\|\\w\\)\\)"
  "Regular expression that matches URLs."
  :group 'erc
  :type 'regexp)

(defcustom erc-button-buttonize-nicks-flag t
  "Flag indicating whether nicks are buttonized or not."
  :group 'erc
  :type 'boolean)

(defcustom erc-button-alist
  `(("(\\(\\([^~\n \t@][^\n \t@]*\\)@\\([a-zA-Z0-9.:-]+\\)\\)" 1 t finger 2 3)
    ("<URL: *\\([^<> ]+\\) *>" 0 t browse-url 1)
    ("EmacsWiki:\\([A-Z][a-z]+\\([A-Z][a-z]+\\)+\\)" 0 t erc-browse-emacswiki 1)
    (erc-button-url-regexp 0 t browse-url 0)
    (erc-nick-regexp 1 erc-button-buttonize-nicks-flag erc-nick-popup 1))
  "*Alist of regexps matching buttons in ERC buffers.

Each entry has the form (REGEXP BUTTON FORM CALLBACK PAR...), where
REGEXP: is the string matching text around the button
or a symbol indicating a variable holding that string,
BUTTON: is the number of the regexp grouping actually matching the button,
FORM: is a lisp expression which must eval to true for the button to
be added,
CALLBACK: is the function to call when the user push this button, and each
PAR: is a number of a regexp grouping whose text will be passed to CALLBACK.

CALLBACK can also be a variable, in that case the value of that
variable is the real callback function."
  :group 'erc
  :type '(repeat (list (choice regexp variable)
		       (integer :tag "Button")
		       (sexp :tag "Form")
		       (function :tag "Callback")
		       (repeat :tag "Par"
			       :inline t
			       (integer :tag "Regexp group")))))

(defcustom erc-emacswiki-url "http://www.emacswiki.org/cgi-bin/wiki.pl?"
  "*URL of the EmacsWiki Homepage."
  :group 'erc
  :type 'string)

(defvar erc-button-keymap
  '(keymap
    (13 . widget-button-press)
    (down-mouse-2 . widget-button-click)))

(defvar erc-button-marker-list nil)
(make-variable-buffer-local 'erc-button-marker-list)

(defun erc-button-add-buttons ()
  "Find external references in the current buffer and make buttons of them.
\"External references\" are things like URLs, as
specified by `erc-button-alist'."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t)
	  (case-fold-search t)
	  (alist erc-button-alist)
	  beg entry regexp)
      ;; Remove all old markers.
      (let (marker entry new-list)
	(while (setq marker (pop erc-button-marker-list))
	  (if (or (< marker (point-min)) (>= marker (point-max)))
	      (push marker new-list)
	    (goto-char marker)
	    (when (setq entry (erc-button-entry))
	      (put-text-property (match-beginning (nth 1 entry))
				 (match-end (nth 1 entry))
				 'erc-callback nil))
	    (set-marker marker nil)))
	(setq erc-button-marker-list new-list))
      (setq beg (point-min))
      (while (setq entry (pop alist))
	(setq regexp (or (and (stringp (car entry)) (car entry))
			 (and (boundp (car entry))
			      (symbol-value (car entry)))))
	(when (stringp regexp)
	  (goto-char beg)
	  (while (re-search-forward regexp nil t)
	    (let* ((start (and entry (match-beginning (nth 1 entry))))
		   (end (and entry (match-end (nth 1 entry))))
		   (from (match-beginning 0)))
	      (when (and (or (eq t (nth 2 entry))
			     (eval (nth 2 entry)))
			 (not (text-property-not-all
			       start end 'erc-callback nil)))
		;; That optional form returned non-nil, so we add the
		;; button.
		(erc-button-add-button
		 start end 'erc-button-push
		 (car (push (set-marker (make-marker) from)
			    erc-button-marker-list)))))))))))

(defun erc-button-add-button (from to fun &optional data)
  "Create a button between FROM and TO with callback FUN and data DATA."
  (when erc-button-face
    (overlay-put (make-overlay from to)
		 'face erc-button-face))
  (add-text-properties
   from to
   (nconc (and erc-button-mouse-face
	       (list 'mouse-face erc-button-mouse-face))
	  (list 'erc-callback fun)
	  (list 'local-map erc-button-keymap)
	  (and data (list 'erc-data data))))
  (widget-convert-button 'link from to :action 'erc-widget-press-button))

(defun erc-widget-press-button (elems el)
  (goto-char (widget-get elems :from))
  (erc-button-press-button))

(defun erc-button-press-button ()
  "Check text at point for a callback function.
If the text at point has a `erc-callback' property,
call it with the value of the `erc-data' text property."
  (interactive)
  (let* ((data (get-text-property (point) 'erc-data))
	 (fun (get-text-property (point) 'erc-callback)))
    (when fun
      (funcall fun data))))

(defun erc-button-push (marker)
  "Push button starting at MARKER."
  (save-excursion
    (goto-char marker)
    (let* ((entry (erc-button-entry))
	   (inhibit-point-motion-hooks t)
	   (fun (nth 3 entry))
	   (args (mapcar (lambda (group)
			   (let ((string (match-string group)))
			     (set-text-properties
			      0 (length string) nil string)
			     string))
			 (nthcdr 4 entry))))
      (cond
       ((fboundp fun)
	(apply fun args))
       ((and (boundp fun)
	     (fboundp (symbol-value fun)))
	(apply (symbol-value fun) args))
       (t
	(message "You must define `%S' to use this button"
		 (cons fun args)))))))

(defun erc-button-entry ()
  "Return the first entry in `erc-button-alist' matching this place."
  (let ((alist erc-button-alist)
	entry)
    (while alist
      (setq entry (pop alist))
      (if (looking-at (or (and (stringp (car entry)) (car entry))
			  (and (boundp (car entry))
			       (symbol-value (car entry)))))
	  (setq alist nil)
	(setq entry nil)))
    entry))

(add-hook 'erc-insert-modify-hook 'erc-button-add-buttons 'append)
(add-hook 'erc-send-modify-hook 'erc-button-add-buttons 'append)
(define-key erc-mode-map "\C-c\C-w\C-b" 'widget-backward)
(define-key erc-mode-map "\C-c\C-w\C-f" 'widget-forward)

(defun erc-browse-emacswiki (thing)
  "Browse to thing in the emacs-wiki."
  (browse-url (concat erc-emacswiki-url thing)))

;;; Nickname buttons:

(defvar erc-nick-regexp nil)
(make-variable-buffer-local 'erc-nick-regexp)

(require 'regexp-opt)
(defun erc-recompute-nick-regexp ()
  (if channel-members
      (setq erc-nick-regexp
	    (concat "[ :,()!\"'<]"
		    (let ((max-specpdl-size 3000))
		      (regexp-opt (mapcar #'car channel-members) t))
		    "[ :,()!\"';>]"))
    (setq erc-nick-regexp nil)))



(add-hook 'erc-channel-members-changed-hook 'erc-recompute-nick-regexp)

(defun erc-nick-popup (nick)
  (let* ((completion-ignore-case t)
	 (action (intern
		  (completing-read (concat "What action to take on '" nick "'? ")
				   '(("Query") ("Op") ("DeOp") ("Whois")
				     ("Kick") ("Msg"))))))
    (cond ((eq action 'DeOp)
	   (setq erc-active-buffer (current-buffer))
	   (erc-cmd-DEOP nick))
	  ((eq action 'Kick)
	   (setq erc-active-buffer (current-buffer))
	   (erc-cmd-KICK (concat nick " "
				 (read-from-minibuffer
				  (concat "Kick " nick ", reason: ")))))
	  ((eq action 'Msg)
	   (setq erc-active-buffer (current-buffer))
	   (erc-cmd-MSG (concat nick " "
				 (read-from-minibuffer
				  (concat "Message to " nick ": ")))))
	  ((eq action 'Op)
	   (setq erc-active-buffer (current-buffer))
	   (erc-cmd-OP nick))
	  ((eq action 'Query)
	   (setq erc-active-buffer (current-buffer))
	   (erc-cmd-QUERY nick))
	  ((eq action 'Whois)
	   (setq erc-active-buffer (current-buffer))
	   (erc-cmd-WHOIS nick))
	  (t (error "Action not define")))))

(provide 'erc-button)

;;; erc-button.el ends here
