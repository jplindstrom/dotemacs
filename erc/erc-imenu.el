;; erc-imenu.el -- Imenu support for the Emacs IRC Client

;; Author: Mario Lang <mlang@delysid.org>

;; This file is not part of GNU Emacs. But the same license applies.

(require 'erc)

(defun erc-unfill-notice ()
  "Return text from point to a computed end as a string unfilled.
Dont rely on this function, read it first!"
  (let ((str (buffer-substring
	      (save-excursion
		(re-search-forward (regexp-quote erc-notice-prefix)))
	      (progn
		(while (save-excursion
			 (forward-line 1)
			 (looking-at "    "))
		  (forward-line 1))
		(end-of-line) (point)))))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (replace-regexp "\n[ ]+" " ")
      (buffer-substring (point-min) (point-max)))))

(defun erc-create-imenu-index ()
  (let ((index-alist '())
	(notice-alist '())
	(join-alist '())
	(left-alist '())
	(quit-alist '())
	(message-alist '())
	(mode-change-alist '())
	(topic-change-alist '())
	prev-pos)
    (goto-char (point-max))
    (imenu-progress-message prev-pos 0)
    (while (if (bolp) (> (forward-line -1) -1) (progn (beginning-of-line) t))
      (imenu-progress-message prev-pos nil t)
      (save-match-data
	(when (looking-at (concat (regexp-quote erc-notice-prefix)
				  "\\(.+\\)$"))
	  (let ((notice-text  ;; Ugly hack, but seems to work.
		 (save-excursion (erc-unfill-notice)))
		(pos (point)))
	    (push (cons notice-text pos) notice-alist)
	    (or
	     (when (string-match "^\\(.*\\) has joined channel" notice-text)
	       (push (cons (match-string 1 notice-text) pos) join-alist))
	     (when (string-match "^\\(.+\\) has left channel" notice-text)
	       (push (cons (match-string 1 notice-text) pos) left-alist))
	     (when (string-match "^\\(.+\\) has quit\\(.*\\)$" notice-text)
	       (push (cons (concat (match-string 1 notice-text)
				   (match-string 2 notice-text))
			   (point))
		     quit-alist))
	     (when (string-match
		    "^\\(\\S-+\\) (.+) has changed mode for \\S-+ to \\(.*\\)$"
		    notice-text)
	       (push (cons (concat (match-string 1 notice-text) ": "
				   (match-string 2 notice-text))
			   (point))
		     mode-change-alist))
	     (when (string-match
		    "^\\(\\S-+\\) (.+) has set the topic for \\S-+: \\(.*\\)$"
		    notice-text)
	       (push (cons (concat (match-string 1 notice-text) ": "
				   (match-string 2 notice-text)) pos)
		     topic-change-alist)))))
	(when (looking-at "<\\(\\S-+\\)> \\(.+\\)$")
	  (let ((from (match-string 1))
		(message-text (match-string 2)))
	    (push (cons (concat from ": " message-text) (point)) message-alist)))
	))
    (and notice-alist      (push (cons "notices" notice-alist) index-alist))
    (and join-alist        (push (cons "joined" join-alist) index-alist))
    (and left-alist        (push (cons "parted" left-alist) index-alist))
    (and quit-alist        (push (cons "quit" quit-alist) index-alist))
    (and mode-change-alist (push (cons "mode-change" mode-change-alist) index-alist))
    (and message-alist     (push (cons "messages" message-alist) index-alist))
    (and topic-change-alist (push (cons "topic-change" topic-change-alist) index-alist))
    index-alist))

(provide 'erc-imenu)
