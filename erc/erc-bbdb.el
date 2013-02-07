;;; erc-bbdb.el --- Integrating the BBDB into ERC

;; Original Author: Andreas Fuchs <asf@void.at>
;; Adapted from zenirc-bbdb-whois.el: Mario Lang <mlang@delysid.org>
;; Created: 2001-11-19 - last update: 2001-11-19
;; Version: 0.9
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Copyright (C) 2001 Andreas Fuchs <asf@void.at>
;Copyright (C) 2002 by Mario Lang <mlang@delysid.org>

;;
;; zenirc-bbdb-whois.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; zenirc-bbdb-whois.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;
;; This file was adapted from a file called zenirc-bbdb-whois.el. Thanks
;; to the original author for the great idea!
;;
;; Just add a (require 'erc-bbdb) in your .emacs file, and
;; you should be all set. Whenever you do a /whois on a person whose
;; IRCNAME is in your .bbdb, the notes field irc-nick will get
;; updated.
;;
;;; Code:

(require 'erc)
(require 'bbdb)
(require 'bbdb-com)
(require 'bbdb-gui)
(require 'bbdb-hooks)

(defgroup erc-bbdb nil
  "Variables related to BBDB usage."
  :group 'erc)

(defcustom erc-bbdb-auto-create-on-whois-p nil
  "*If nil, don't create bbdb records automatically when a WHOIS is done.
Leaving this at nil is a good idea, but you can turn it
on if you want to have lots of People named \"John Doe\" in your BBDB."
  :group 'erc-bbdb
  :type 'boolean)

(defcustom erc-bbdb-auto-create-on-join-p nil
  "*If nil, don't create bbdb records automatically when a person joins a channel.
Leaving this at nil is a good idea, but you can turn it
on if you want to have lots of People named \"John Doe\" in your BBDB."
  :group 'erc-bbdb
  :type 'boolean)

(defcustom erc-bbdb-auto-create-on-nick-p nil
  "*If nil, don't create bbdb records automatically when a person changes her nick.
Leaving this at nil is a good idea, but you can turn it
on if you want to have lots of People named \"John Doe\" in your BBDB."
  :group 'erc-bbdb
  :type 'boolean)

(defcustom erc-bbdb-popup-type 'visible
  "*If t, pop up a BBDB buffer showing the record of a WHOISed person
or the person who has just joined a channel.
If set to 'visible, the BBDB buffer only pops up when someone was WHOISed
or a person joined a channel visible on any frame."
  :group 'erc-bbdb
  :type 'sexp)

(defcustom erc-bbdb-irc-nick-field 'irc-nick
  "The notes field name to use for annotating IRC nicknames."
  :group 'erc-bbdb
  :type 'symbol)

(defcustom erc-bbdb-irc-channel-field 'irc-channel
  "The notes field name to use for annotating IRC channels."
  :group 'erc-bbdb
  :type 'symbol)

(defcustom erc-bbdb-irc-highlight-field 'irc-highlight
  "The notes field name to use for highlighting of a person's messages"
  :group 'erc-bbdb
  :type 'symbol)

(defcustom erc-bbdb-elide-display nil
  "*If t, show BBDB popup buffer elided"
  :group 'erc-bbdb
  :type 'boolean)

(defun erc-bbdb-search-name-and-create (create-p name nick finger-host)
  (let* ((ircnick (cons erc-bbdb-irc-nick-field (concat "^"
							(regexp-quote nick))))
	 (finger (cons bbdb-finger-host-field (regexp-quote finger-host)))
	 (record (or (bbdb-search (bbdb-records) nil nil nil ircnick)
		     (and name (bbdb-search-simple name nil))
		     (bbdb-search (bbdb-records) nil nil nil finger)
		     (when create-p
		       (bbdb-create-internal (or name
						 "John Doe")
					     nil nil nil nil nil)))))
    (if (listp record)   ; sometimes, the record will be a list. I don't know why.
	(car record)
      record)))

(defun erc-bbdb-show-entry (record channel proc)
  (let ((bbdb-display-layout (bbdb-grovel-elide-arg erc-bbdb-elide-display)))
    (when (and record (or (eq erc-bbdb-popup-type t)
			  (and (eq erc-bbdb-popup-type 'visible)
			       (and channel
				    (get-buffer-window (erc-get-buffer channel proc)
						  'visible)))))
      (bbdb-display-records (list record)))))

(defun erc-bbdb-insinuate-and-show-entry (create-p proc nick name finger-host &optional chan new-nick)
  (let ((record (erc-bbdb-search-name-and-create create-p name nick finger-host)))
    (when record
      (bbdb-annotate-notes record (or new-nick nick) erc-bbdb-irc-nick-field)
      (bbdb-annotate-notes record finger-host bbdb-finger-host-field)
      (and chan
	   (bbdb-annotate-notes record chan erc-bbdb-irc-channel-field))
      (erc-bbdb-highlight-record record)
      (erc-bbdb-show-entry record chan proc))))

(defun erc-bbdb-whois (proc parsed)
  (let (; We could use server name too, probably
	(nick (aref parsed 3))
	(name (aref parsed 7))
	(finger-host (concat (aref parsed 4) "@" (aref parsed 5))))
    (erc-bbdb-insinuate-and-show-entry erc-bbdb-auto-create-on-whois-p proc
				       nick name finger-host)))

(defun erc-bbdb-JOIN (proc parsed)
  (let* ((sender (erc-parse-user (aref parsed 1)))
	 (nick (nth 0 sender)))
    (unless (string= nick (erc-current-nick))
      (let* ((channel (aref parsed 2))
	     (finger-host (concat (nth 1 sender) "@" (nth 2 sender))))
	  (erc-bbdb-insinuate-and-show-entry erc-bbdb-auto-create-on-join-p proc
					     nick nil finger-host channel)))))

(defun erc-bbdb-NICK (proc parsed)
  "Annotate new nick name to a record in case it already exists."
  (let* ((sender (erc-parse-user (aref parsed 1)))
	 (nick (nth 0 sender)))
    (unless (string= nick (erc-current-nick))
      (let* ((finger-host (concat (nth 1 sender) "@" (nth 2 sender))))
	(erc-bbdb-insinuate-and-show-entry erc-bbdb-auto-create-on-nick-p proc
					   nick nil finger-host nil (aref parsed 2))))))

(defun erc-bbdb-init-highlighting-hook-fun (proc parsed)
  (erc-bbdb-init-highlighting))

(defun erc-bbdb-init-highlighting ()
  "Initialize the highlighting based on BBDB fields.
This function typically gets called on a successful server connect.
The field name in the BBDB which controls highlighting is specified by
`erc-bbdb-irc-highlight-field'. Fill in either \"pal\"
\"dangerous-host\" or \"fool\". They work exactly like their
counterparts `erc-pals', `erc-dangerous-hosts' and `erc-fools'."
  (let* ((irc-highlight (cons erc-bbdb-irc-highlight-field
			      ".+"))
	(matching-records (bbdb-search (bbdb-records) nil nil nil irc-highlight)))
    (mapcar 'erc-bbdb-highlight-record matching-records)))

(defun erc-bbdb-highlight-record (record)
  (let* ((notes (bbdb-record-raw-notes record))
	 (highlight-field (assoc erc-bbdb-irc-highlight-field notes))
	 (nick-field      (assoc erc-bbdb-irc-nick-field notes)))
    (if (and highlight-field
	     nick-field)
	(let ((highlight-types (split-string (cdr highlight-field) bbdb-notes-default-separator))
	      (nick-names      (split-string (cdr nick-field)      (concat "[\n" bbdb-notes-default-separator "]"))))
	  (mapcar (lambda (highlight-type)
		    (mapcar (lambda (nick-name)
			      (if (member highlight-type '("pal" "dangerous-host" "fool"))
				  (add-to-list (intern (concat "erc-" highlight-type "s")) (regexp-quote nick-name))
				(error (format "\"%s\" (in \"%s\") is not a valid highlight type!" highlight-type nick-name))))
			    nick-names))
		  highlight-types)))))

(add-hook 'erc-server-311-hook 'erc-bbdb-whois t)
(add-hook 'erc-server-JOIN-hook 'erc-bbdb-JOIN t)
(add-hook 'erc-server-NICK-hook 'erc-bbdb-NICK t)
(add-hook 'erc-server-376-hook 'erc-bbdb-init-highlighting-hook-fun t) ; any MOTD hook.

(provide 'erc-bbdb)
