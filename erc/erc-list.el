;;; erc-list.el --- Proivde a faster channel listing mechanism

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

;; This file provides a simple derived mode for viewing Channel lists.
;; It also serves as a demonstration of how the new server hook fascility
;; can be used.

;;; Code:

(require 'erc)

(defcustom erc-chanlist-progress-message t
  "*Show progress message while accumulating channel list."
  :group 'erc
  :type 'boolean)

(defvar erc-chanlist-buffer nil)
(make-variable-buffer-local 'erc-chanlist-buffer)

(defvar erc-chanlist-last-time 0)

(define-derived-mode erc-chanlist-mode fundamental-mode "ChanList"
  "Mode for viewing a channel list of a particular server.

\\{erc-chanlist-mode-map}")

(define-key erc-chanlist-mode-map "j" 'erc-join-channel)
(define-key erc-chanlist-mode-map "\C-c\C-j" 'erc-join-channel)

(defun erc-chanlist ()
  "Show a channel listing of the current server in a special mode.
Please note that this function only works with IRC servers which conform
to RFC and send the LIST header (#321) at start of list transmission."
  (interactive)
  (with-current-buffer (erc-server-buffer)
    (erc-once-with-server-event 321
      (add-hook 'erc-server-322-hook 'erc-chanlist-322 nil t)
      (erc-once-with-server-event 323
	(remove-hook 'erc-server-322-hook 'erc-chanlist-322 t)
	(let ((buf erc-chanlist-buffer))
	  (if (buffer-live-p buf)
	      (progn
		(set-buffer buf)
		(let (buffer-read-only)
		  (sort-lines nil (point-min) (point-max))
		  (let ((sum (count-lines (point-min) (point-max))))
		    (goto-char (point-max))
		    (insert (format "\nTotal %d channels" sum)))
		  (goto-char (point-min))
		  (insert (format "%-15s%5s %s\n-------------- ----- ----------\n"
				  "Channel"
				  "Users"
				  "Topic"))
		  (view-buffer buf)))
	    (error "`erc-chanlist-buffer' does not refer to a live buffer")))
	t)
      (setq erc-chanlist-buffer (get-buffer-create (format "*Channels on %s*"
							   (aref parsed 1))))
      (with-current-buffer erc-chanlist-buffer
	(setq buffer-read-only nil)
	(erase-buffer)
	(erc-chanlist-mode)
	(setq erc-process proc)
	(setq buffer-read-only t))
      t)
    ;; Now that we setup our callbacks, we pull the trigger
    (erc-send-command "LIST")))

(defun erc-chanlist-322 (proc parsed)
  (let ((chnl (aref parsed 3))
	(nv (aref parsed 4))
	(topic (aref parsed 5)))
    (with-current-buffer erc-chanlist-buffer
      (save-excursion
	(goto-char (point-max))
	(let (buffer-read-only)
	  (insert (format "%-16s%4s %s\n" chnl nv (erc-interpret-controls topic))))
	(when (and erc-chanlist-progress-message
		 (> (erc-time-diff
		     erc-chanlist-last-time (erc-current-time))
		    1))
	  (setq erc-chanlist-last-time (erc-current-time))
	  (message "Accumulating information... %c"
		   (aref [?/ ?| ?\\ ?- ?! ?O ?o] (random 7))))
	;; Return success to prevent other hook functions from being run.
	t))))

(provide 'erc-list)

;;; erc-list.el ends here
