;;; erc-match.el --- Highlight messages matching certain regexps

;; Copyright (C) 2002  Andreas Fuchs <asf@void.at>

;; Author: Andreas Fuchs <asf@void.at>
;; Keywords: comm, faces

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

;; This file includes stuff to work with pattern matching in ERC. If
;; you were used to customizing erc-fools, erc-keywords, erc-pals,
;; erc-dangerous-hosts and the like, this file contains these
;; customizable variables.

;; Usage:
;; put into your .emacs:
;; (eval-after-load "erc"
;;   (require 'erc-match))
;;
;; The rest will be done automatically.

;;; Code:

;; Customisation:

(defgroup erc-match nil
  "Keyword and Friend/Foe/... recognition.
Group containing all things concerning pattern matching in ERC
messages.")

(defcustom erc-pals nil
  "List of pals on IRC."
  :group 'erc-match
  :type '(repeat regexp))

(defcustom erc-fools nil
  "List of fools on IRC."
  :group 'erc-match
  :type '(repeat regexp))

(defcustom erc-keywords nil
  "List of keywords to highlight in all incoming messages."
  :group 'erc-match
  :type '(repeat regexp))

(defcustom erc-dangerous-hosts nil
  "List of regexps for hosts to highlight.
Useful to mark nicks from dangerous hosts."
  :group 'erc-match
  :type '(repeat regexp))

(defcustom erc-pal-highlight-type 'nick
  "*Determines how to highlight messages by pals.
See `erc-pals'.

The following values are allowed:

    'nick - highlight pal's nickname only
    'all  - highlight the entire message from pal

Any other value disables pal highlighting altogether."
  :group 'erc-match
  :type '(choice (const :tag "highlight pal's nickname only" nick)
		 (const :tag "highlight pal's entire message" all)
		 (const :tag "don't highlight pal's messages" nil)))

(defcustom erc-fool-highlight-type 'nick
  "*Determines how to highlight messages by fools.
See `erc-fools'.

The following values are allowed:

    'nick - highlight fool's nickname only
    'all  - highlight the entire message from fool

Any other value disables fool highlighting altogether."
  :group 'erc-match
  :type '(choice (const :tag "highlight fool's nickname only" nick)
		 (const :tag "highlight fool's entire message" all)
		 (const :tag "don't highlight fool's messages" nil)))

(defcustom erc-keyword-highlight-type 'keyword
  "*Determines how to highlight messages containing keywords.
See variable `erc-keywords'.

The following values are allowed:

    'keyword - highlight keyword only
    'all     - highlight the entire message containing keyword

Any other value disables keyword highlighting altogether."
  :group 'erc-match
  :type '(choice (const :tag "highlight keywords only" keyword)
		 (const :tag "highlight entire message containing keywords" all)
		 (const :tag "don't highlight messages containing keywords" nil)))

(defcustom erc-dangerous-host-highlight-type 'nick
  "*Determines how to highlight messages by nicks from dangerous-hosts.
See `erc-dangerous-hosts'.

The following values are allowed:

    'nick - highlight nick from dangerous-host only
    'all  - highlight the entire message from dangerous-host

Any other value disables dangerous-host highlighting altogether."
  :group 'erc-match
  :type '(choice (const :tag "highlight nick from dangerous-host only" nick)
		 (const :tag "highlight entire message from dangerous-host" all)
		 (const :tag "don't highlight messages from dangerous-hosts" nil)))


(defcustom erc-log-matches-types-alist '((keyword . "ERC Keywords"))
  "Alist telling ERC where to log which match types.
Valid match type keys are:
- keyword
- pal
- dangerous-host
- fool

The other element of each cons pair in this lits is the buffer name to
use for the logged message."
  :group 'erc-match
  :type '(repeat (cons (choice :tag "Key"
			       (const keyword)
			       (const pal)
			       (const dangerous-host)
			       (const fool))
		       (string :tag "Buffer name"))))

(defcustom erc-log-matches-flag 'away
  "Flag specifying when matched message logging should happen.
When nil, don't log any matched messages.
When t, log messages.
When 'away, log messages only when away."
  :group 'erc-match
  :type '(choice (const away)
		 (const nil)
		 (const t)))

(defcustom erc-log-match-format "[%t] <%n:%c> %m"
  "Format for matched Messages.
This variable specifies how messages in the corresponding log buffers will
be formatted. The various format specs are:

%t Timestamp
%n Nickname of sender
%u Nickname!user@host of sender
%c Channel in which this was received
%m Message"
  :group 'erc-match
  :type 'string)

(defcustom erc-text-matched-hook '(erc-log-matches erc-hide-fools)
  "Hook run when text matches a given match-type.
Functions in this hook are passed as arguments:
(match-type nick!user@host message) where MATCH-TYPE is a symbol of:
keyword, pal, dangerous-host, fool"
  :group 'erc-match
  :type 'hook)

;; Faces:

(defface erc-dangerous-host-face '((t (:foreground "red")))
  "ERC face for people on dangerous hosts.
See `erc-dangerous-hosts'."
  :group 'erc-faces)
(defface erc-pal-face '((t (:bold t :foreground "Magenta")))
  "ERC face for your pals.
See `erc-pals'."
  :group 'erc-faces)
(defface erc-fool-face '((t (:foreground "dim gray")))
  "ERC face for fools on the channel.
See `erc-fools'."
  :group 'erc-faces)
(defface erc-keyword-face '((t (:bold t :foreground "pale green")))
  "ERC face for your keywords.
See `erc-keywords'."
  :group 'erc-faces)

;; Functions:

(defun erc-add-entry-to-list (list prompt &optional completions)
  "Add an entry interactively to a list.
LIST must be passed as a symbol
The query happens using PROMPT.
Completion is performed on the optional alist COMPLETIONS.
Before adding the new entry, it is tested using TEST.
TEST must be a function accepting a regular expression."
  (let ((entry (completing-read
		prompt
		completions
		(lambda (x)
		  (not (member (car x) (eval list)))))))
    (if (member entry (eval list))
	(error (format "\"%s\" is already on the list" entry))
      (set list (cons entry (symbol-value list))))))

(defun erc-remove-entry-from-list (list prompt)
  "Remove an entry interactively from a list.
LIST must be passed as a symbol
The query happens using PROMPT and predicate TEST."
  (let ((entry (completing-read
		prompt
		(mapcar 'list (symbol-value list))
		(lambda (x)
		  (member (car x) (eval list)))
		t)))
    (set list (delete entry (symbol-value list)))))

(defun erc-add-pal ()
  "Add pal interactively to `erc-pals'."
  (interactive)
  (erc-add-entry-to-list 'erc-pals "Add pal: " channel-members))

(defun erc-delete-pal ()
  "Delete pal interactively to `erc-pals'."
  (interactive)
  (erc-remove-entry-from-list 'erc-pals "Delete pal: "))

(defun erc-add-fool ()
  "Add fool interactively to `erc-fools'."
  (interactive)
  (erc-add-entry-to-list 'erc-fools "Add fool: " channel-members))

(defun erc-delete-fool ()
  "Delete fool interactively to `erc-fools'."
  (interactive)
  (erc-remove-entry-from-list 'erc-fools "Delete fool: "))

(defun erc-add-keyword ()
  "Add keyword interactively to `erc-keywords'."
  (interactive)
  (erc-add-entry-to-list 'erc-keywords "Add keyword: "))

(defun erc-delete-keyword ()
  "Delete keyword interactively to `erc-keywords'."
  (interactive)
  (erc-remove-entry-from-list 'erc-keywords "Delete keyword: "))

(defun erc-add-dangerous-host ()
  "Add dangerous-host interactively to `erc-dangerous-hosts'."
  (interactive)
  (erc-add-entry-to-list 'erc-dangerous-hosts "Add dangerous-host: "))

(defun erc-delete-dangerous-host ()
  "Delete dangerous-host interactively to `erc-dangerous-hosts'."
  (interactive)
  (erc-remove-entry-from-list 'erc-dangerous-hosts "Delete dangerous-host: "))

(defun erc-pal-p (nickuserhost msg)
  "Check whether NICKUSERHOST is in `erc-pals'.
MSG will be ignored."
  (and nickuserhost
       (erc-list-match erc-pals nickuserhost)))

(defun erc-fool-p (nickuserhost msg)
  "Check whether NICKUSERHOST is in `erc-fools' or MSG is directed at a fool."
  (and msg nickuserhost
       (or (erc-list-match erc-fools nickuserhost)
	   (erc-directed-at-fool-p msg))))

(defun erc-keyword-p (nickuserhost msg)
  "Check whether any keyword of `erc-keywords' matches for MSG.
NICKUSERHOST will be ignored."
  (and msg
       (erc-list-match erc-keywords msg)))

(defun erc-dangerous-host-p (nickuserhost msg)
  "Check whether NICKUSERHOST is in `erc-dangerous-hosts'.
MSG will be ignored."
  (and nickuserhost
       (erc-list-match erc-dangerous-hosts nickuserhost)))

(defun erc-directed-at-fool-p (msg)
  "Check wether MSG is directed at a fool.
In order to do this, every entry in `erc-fools' will be used.
In any of the following situations, MSG is directed at an entry FOOL:

- MSG starts with \"FOOL: \"
- MSG contains \", FOOL.\" (actually, \"\\s. FOOL\\s.\")"
  (let ((fools-beg (mapcar (lambda (entry)
				 (concat "^" entry ": "))
			   erc-fools))
	(fools-end (mapcar (lambda (entry)
				 (concat "\\s. " entry "\\s."))
			       erc-fools)))
    (or (erc-list-match fools-beg msg)
	(erc-list-match fools-end msg))))

(defun erc-get-parsed-vector (point)
  "Return the whole parsed vector on POINT."
  (get-text-property point 'erc-parsed))

(defun erc-get-parsed-vector-nick (vect)
  "Return nickname in the parsed vector VECT."
  (let* ((untreated-nick (when (vectorp vect)
			   (aref vect 1)))
	 (maybe-nick (when untreated-nick
		       (car (split-string untreated-nick "!")))))
    (when (and (not (null maybe-nick))
	       (erc-is-valid-nick-p maybe-nick))
      untreated-nick)))

(defun erc-get-parsed-vector-type (vect)
  "Return message type in the parsed vector VECT."
  (and vect
       (aref vect 0)))

(defun erc-match-message ()
  "Mark certain keywords in a region. Use this defun with `erc-insert-modify-hook'."
  (goto-char (point-min))
  (let* ((to-match-nick-dep '("pal" "fool" "dangerous-host"))
	 (to-match-nick-indep '("keyword"))
	 (vector (erc-get-parsed-vector (point-min)))
	 (nickuserhost (erc-get-parsed-vector-nick vector))
	 (nickname     (and nickuserhost
			    (nth 0 (erc-parse-user nickuserhost))))
	 (old-pt (point))
	 (nick-beg (and nickname
			(re-search-forward (regexp-quote nickname) (point-max) t)
			(match-beginning 0)))
	 (nick-end (when nick-beg
		     (match-end 0)))
	 (message (buffer-substring (if nick-end
					(+ 2 nick-end)
				      (point-min))
				    (point-max))))
    (when vector
      (mapc
       (lambda (match-type)
	 (goto-char (point-min))
	 (let* ((match-prefix (concat "erc-" match-type))
		(match-pred (intern (concat match-prefix "-p")))
		(match-htype (eval (intern (concat match-prefix "-highlight-type"))))
		(match-regex (eval (intern (concat match-prefix "s"))))
		(match-face (intern (concat match-prefix "-face"))))
	   (when (funcall match-pred nickuserhost message)
	     (cond
	      ((and (eq match-htype 'nick)
		    nick-end)
	       (erc-put-text-property nick-beg nick-end
				      'face match-face (current-buffer)))
	      ((eq match-htype 'all)
	       (erc-put-text-property (point-min) (point-max)
				      'face match-face (current-buffer)))
	      ((and (string= match-type "keyword")
		    (eq match-htype 'keyword))
	       (mapc (lambda (regex)
		       (goto-char (+ 2 (or nick-end
					   (point-min))))
		       (while (re-search-forward regex nil t)
			 (erc-put-text-property (match-beginning 0) (match-end 0)
						'face match-face)))
		     match-regex))
	      (t nil))
	     (run-hook-with-args 'erc-text-matched-hook
				 (intern match-type)
				 (or nickuserhost
				     (concat "Server:" (erc-get-parsed-vector-type vector)))
				 message))))
       (if nickuserhost
	   (append to-match-nick-dep to-match-nick-indep)
	 to-match-nick-indep)))))

(add-hook 'erc-insert-modify-hook 'erc-match-message)

(defun erc-log-matches (match-type nickuserhost message)
  "Log matches in a separate buffer, determined by MATCH-TYPE.
The behaviour of this function is controlled by the variables
`erc-log-matches-types-alist' and `erc-log-matches-flag'. Specify the
match types which should be logged in the former, and
deactivate/activate match logging in the latter. See
`erc-log-match-format'."
  (let  ((match-buffer-name (cdr (assq match-type erc-log-matches-types-alist)))
	 (nick (nth 0 (erc-parse-user nickuserhost))))
    (when (and
	   (or (eq erc-log-matches-flag t)
	       (and (eq erc-log-matches-flag 'away)
		    away))
	   match-buffer-name)
      (let ((line (format-spec erc-log-match-format
		   (format-spec-make ?n nick
				     ?t (format-time-string "%Y-%m-%d %H:%M")
				     ?c (erc-default-target)
				     ?m message
				     ?u nickuserhost))))
	(with-current-buffer (erc-log-matches-make-buffer match-buffer-name)
	  (toggle-read-only -1)
	  (point-max)
	  (insert line)
	  (toggle-read-only 1))))))

(defun erc-log-matches-make-buffer (name)
  "Create or get a log-matches buffer named NAME and return it."
  (let* ((buffer-already (get-buffer name))
	 (buffer         (or buffer-already
			     (get-buffer-create name))))
    (with-current-buffer buffer
      (unless buffer-already
	(insert " == Type \"q\" to dismiss messages ==\n")
	(view-mode-enter nil (lambda (buffer)
			       (when (y-or-n-p "Discard messages?")
				 (kill-buffer buffer)))))
      buffer)))

(defun erc-log-matches-come-back (proc parsed)
  "Display a notice that messages were logged while away."
  (when (and away
	     (eq erc-log-matches-flag 'away))
    (mapc
     (lambda (match-type)
       (let ((buffer (get-buffer (cdr match-type)))
	     (buffer-name (cdr match-type)))
	 (when buffer
	   (let* ((last-msg-time (erc-emacs-time-to-erc-time
				  (with-current-buffer buffer
				    (get-text-property (1- (point-max))
						       'timestamp))))
		  (away-time     (erc-emacs-time-to-erc-time away)))
	     (when (and away-time last-msg-time
			(erc-time-gt last-msg-time away-time))
	       (erc-display-message nil 'notice 'active
				    (format "You have logged messages waiting in \"%s\"." buffer-name))
	       (erc-display-message nil 'notice 'active
				    (format "Type \"C-c C-k %s RET\" to view them." buffer-name)))))))
     erc-log-matches-types-alist))
  nil)

; This handler must be run _before_ erc-process-away is.
(add-hook 'erc-server-305-hook 'erc-log-matches-come-back nil)

(defun erc-go-to-log-matches-buffer ()
  "Interactively open an erc-log-matches buffer."
  (interactive)
  (let ((buffer-name (completing-read "Switch to ERC Log buffer: "
				      (mapcar (lambda (x)
						(cons (cdr x) t))
					      erc-log-matches-types-alist)
				      (lambda (buffer-cons)
					(get-buffer (car buffer-cons))))))
    (switch-to-buffer buffer-name)))

(define-key erc-mode-map "\C-c\C-k" 'erc-go-to-log-matches-buffer)

(defun erc-hide-fools (match-type nickuserhost message)
 "Hide foolish comments.
This function should be called from `erc-text-matches-hook'."
 (when (eq match-type 'fool)
   (erc-put-text-properties (point-min) (point-max) '(invisible intangible) (current-buffer))))

(provide 'erc-match)
;;; erc-match.el ends here
