;;; erc-notify.el --- Online status change notification

;; Copyright (C) 2002  Mario Lang

;; Author: Mario Lang <mlang@lexx.delysid.org>
;; Keywords: comm

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

;; This module defines a new command, /NOTIFY

;;; Code:

(require 'erc)

;;;; Customizable variables

(defcustom erc-notify-list nil
  "*List of nicknames you want to be notificated about online/offline status change."
  :group 'erc
  :type '(repeat string))

(defcustom erc-notify-by-userhost t
  "not implemented"
  :group 'erc
  :type 'boolean)

(defcustom erc-notify-interval 60
  "*Time interval (in seconds) for checking online status of notificated
people."
  :group 'erc
  :type 'integer)

;;;; Internal variables

(defvar erc-last-ison nil
  "Last ISON information received through erc-notify-timer.")
(make-variable-buffer-local 'erc-last-ison)
(defvar erc-last-ison-time 0
  "Last time ISON was sent to the server in `erc-notify-timer'.")
(make-variable-buffer-local 'erc-last-ison-time)

;;;; Setup

(erc-define-catalog
 'english
 '((notify_current . "Notificated people online: %l")
   (notify_list    . "Current notify list: %l")
   (notify_on      . "Detected %n on IRC")
   (notify_off     . "%n left IRC")))

(add-hook 'erc-timer-hook 'erc-notify-timer)

;;;; Timer handler

(defun erc-notify-timer (now)
  (when (and erc-notify-list
	     (> (erc-time-diff
		 erc-last-ison-time now)
		erc-notify-interval))
    (erc-once-with-server-event 303
      (let* ((ison-list (split-string (aref parsed 3)))
	     (new-list ison-list)
	     (old-list erc-last-ison))
	(while new-list
	  (when (not (member (car new-list) old-list))
	    (erc-display-message
	     parsed 'notice (process-buffer proc)
	     'notify_on ?n (car new-list)))
	  (setq new-list (cdr new-list)))
	(while old-list
	  (when (not (member (car old-list) ison-list))
	    (erc-display-message
	     parsed 'notice (process-buffer proc)
	     'notify_off ?n (car old-list)))
	  (setq old-list (cdr old-list)))
	(setq erc-last-ison ison-list)
	t))
    (erc-send-command (concat "ISON " (mapconcat 'identity erc-notify-list " ")))
    (setq erc-last-ison-time now)))

;;;; User level command

(defun erc-cmd-NOTIFY (line &optional force)
  "Change notify-list or list current notify-list members online.
Without args, list the current list of notificated people online,
with args, toggle notify status of people."
  (cond
   ((string-match "^\\s-*$" line)
    ;; Print current notificated people (online)
    (let ((ison (with-current-buffer (erc-server-buffer) erc-last-ison)))
      (if (not ison)
	  (erc-display-message
	   nil 'notice 'active "No ison-list yet!")
	(erc-display-message
	 nil 'notice 'active
	 'notify_current ?l ison)))
    t)
   (t
    (let ((args (split-string line)))
      (while args
	(if (member (car args) erc-notify-list)
	    (setq erc-notify-list (delete (car args) erc-notify-list))
	  (setq erc-notify-list (cons (car args) erc-notify-list)))
	(setq args (cdr args)))
      (erc-display-message
       nil 'notice 'active
       'notify_list ?l (mapconcat 'identity erc-notify-list " "))
      t))))

(provide 'erc-notify)

;;; erc-notify.el ends here
