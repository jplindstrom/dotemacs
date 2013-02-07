;;; erc-fill.el --- Filling IRC messages in various ways

;; Author: Andreas Fuchs <asf@void.at>
;;         Mario Lang <mlang@delysid.org>

;; This file is not part of GNU Emacs, but the same license applies.

;;; Code:

(defcustom erc-fill-prefix nil
  "Values used as `fill-prefix' for `erc-fill-region'.
nil means fill with space, a string means fill with this string."
  :group 'erc
  :type '(choice (const nil) string))

(defcustom erc-fill-function 'erc-fill-variable
  "Function to use for filling messages.  Choices include
erc-fill-variable and erc-fill-static. You can, of course, define your
own filling function. It needs to have as parameters the start and the
end of the region to be filled and needs to return a list of two buffer
positions (start and end) in case the function changed those through inserting
or deleteing something in the region."
  :group 'erc
  :type 'function)

(defcustom erc-fill-static-center 27
  "Column around which all statically filled messages will be
centered.  This column denotes the point where the ' ' character
between <nickname> and the entered text will be put, thus aligning
nick names right and text left."
  :group 'erc
  :type 'integer)

(defvar erc-fill-column 70
  "The column at which a filled paragraph is broken.")

(defun erc-fill ()
  "Fill a region using the function referenced in `erc-fill-function'."
  (unless (erc-string-invisible-p (buffer-substring (point-min) (point-max)))
    (when erc-fill-function
      (funcall erc-fill-function))))

(defun erc-fill-static ()
  "Fills a text such that messages start at column `erc-fill-static-column'.
The result looks like this:
>From the 2 original messages (with `erc-fill-static-column' set to 27):
<shortnick> foo bar baz
<a-very-long-nick> foo bar baz quuuuux

erc-fill-region-static generates:
		<shortnick> foo bar baz
	 <a-very-long-nick> foo bar baz quuuuux

Of course, overhanging lines are continued at
`erc-fill-static-column', generating:
		<shortnick> this is a very very very long message with no
			    meaning at all"
  (save-match-data
    (let* ((white-space-str " \t")
	   (white-space-lst (string-to-list white-space-str))
	   (white-space-re  (concat "[" white-space-str "]")))
      (goto-char (point-min))
      (let* ((nick (when (looking-at "^\\(\\S-+\\)\\( .*\\)$")
		     (match-string 1)))
	     (message-text (and nick (match-string 2)))
	     (text-start   (max 0 (- erc-fill-static-center (length nick)))))
	(when (and nick message-text)
	  (insert (make-string text-start ? ))
	  (let ((fill-column erc-fill-column)
		(erc-fill-prefix (make-string text-start ? )))
	    (erc-fill-variable)
	    (goto-char (point-min))
	    (let ((lines-to-go (erc-count-lines)))
	      (when (>= lines-to-go 1) ; we have more than one line to fill
		(save-restriction
		  (narrow-to-region (progn (forward-line)
					   (point))
				    (point-max))
		  (let ((fill-column (- fill-column (string-width nick))))
		    (erc-fill-variable)) ; first re-fill for
					; "smaller" margin
		  (goto-char (point-min)) ; now indent behind nick
		  (while (> lines-to-go 0)
		    (insert (make-string (1+ (length nick)) ? ))
		    (forward-line)
		    (setq lines-to-go (1- lines-to-go))))))
	    (let* ((parsed-posn (text-property-not-all (point-min) (point-max) 'erc-parsed nil))
		   (parsed-prop (when parsed-posn
				  (get-text-property parsed-posn 'erc-parsed))))
	      (put-text-property (point-min) (point-max) 'erc-parsed parsed-prop))))))))

(defun erc-count-lines ()
  "Count the lines that lie between (point) and the end of the buffer."
  (let ((i 0)
	(point-before (point)))
    (while (not (= 1 (forward-line)))
      (setq i (1+ i)))
    (goto-char point-before)
    (max 0 (1- i))))

(defun erc-fill-variable ()
  "Fill region from START to END. START and END should be markers."
  (let ((fill-prefix erc-fill-prefix))
    (unless fill-prefix
      (goto-char (point-min))
      (when (looking-at "\\(\\S-+ \\)")
	(let ((len (length (match-string 1))))
	  (setq fill-prefix (make-string len ? )))))
    (fill-region (point-min) (point-max) t t)))
(provide 'erc-fill)
