;;; erc.el --- An Emacs Internet Relay Chat client

;; Author: Alexander L. Belikoff (abel@bfr.co.il)
;; Contributors: Sergey Berezin (sergey.berezin@cs.cmu.edu),
;;               Mario Lang (mlang@delysid.org),
;;               Alex Schroeder (alex@gnu.org)
;;               Andreas Fuchs (afs@void.at)
;;               Gergely Nagy (algernon@midgard.debian.net)
;;               David Edmondson (dme@dme.org)
;; Maintainer: Mario Lang (mlang@delysid.org)
;; Version: 2.93 ($Revision: 1.255 $)
;; Keywords: IRC, chat, client, Internet

;; Copyright (C) 1997  Alexander L. Belikoff
;; Copyright (C) 2001, 2002  Mario Lang

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; ERC is an IRC client for Emacs.

;; For more information, see the following URLs:
;; * http://sf.net/projects/erc/
;; * http://www.emacswiki.org/cgi-bin/wiki.pl?EmacsIRCClient

;; Jul-26-2001. erc.el is now in CVS on SourceForge. I invite everyone
;; who wants to hack it to contact me <mlang@delysid.org> in order to
;; get write access on the CVS.

;; Installation:

;; Put erc.el in your load-path, and put (require 'erc) in your .emacs.

;; Configuration:

;; Use M-x customize-group RET erc RET to get an overview
;; of all the variables you can tweak.

;; Usage:

;; To connect to an IRC server, do
;;
;; M-x erc-select RET
;;
;; After you are connected to a server, you can use C-h m or have a look at
;; the IRC menu.


;;; History:
;;

;;; Code:

(require 'cl)
(require 'font-lock)
(require 'format-spec)

(defconst erc-version-string "2.93 $Revision: 1.255 $"
  "ERC version.  This is used by function `erc-version'.")

(defvar erc-official-location "http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/erc/erc/erc.el?rev=HEAD&content-type=text/plain Comments: mailto://mlang@delysid.org"
  "Location of the ERC client on the Internet.")

(defgroup erc nil
  "Emacs Internet Relay Chat client."
  :link '(url-link "http://www.emacswiki.org/cgi-bin/wiki.pl?EmacsIRCClient")
  :prefix "erc-"
  :group 'processes)

;; tunable connection and authentication parameters

(defcustom erc-server nil
  "IRC server to use.
See function `erc-compute-server' for more details on connection
parameters and authentication."
  :group 'erc
  :type '(choice (const nil) string))

(defcustom erc-port nil
  "IRC port to use."
  :group 'erc
  :type '(choice (const nil) string))

(defcustom erc-nick nil
  "Nickname to use.

Can be either a string, or a list of strings.
In the latter case, if the first nick in the list is already in use,
other nicks are tried in the list order.

See function `erc-compute-nick' for more details on connection
parameters and authentication."
  :group 'erc
  :type '(choice (const nil)
		 (string :tag "Nickname")
		 (repeat string)))

(defcustom erc-nick-uniquifier "`"
  "The character to append to the nick if it is already in use."
  :type 'string)

(defcustom erc-user-full-name nil
  "User full name.

See function `erc-compute-full-name' for more details on connection
parameters and authentication."
  :group 'erc
  :type '(choice (const nil) string function)
  :set (lambda (sym val)
	 (if (functionp val)
	     (set sym (funcall val))
	   (set sym val))))

(defvar erc-password nil
  "ERC password to use in authentication (not necessary).")

(defcustom erc-prompt-for-password t
  "Asks before using the default password, or whether to enter a new one."
  :group 'erc
  :type 'boolean)

(defcustom erc-warn-about-blank-lines t
  "Warn the user if they attempt to send a blank line."
  :group 'erc
  :type 'boolean)

;; tunable GUI stuff

(defun erc-string-no-properties (string)
  (with-temp-buffer
    (insert string)
    (buffer-substring-no-properties (point-min) (point-max))))

(if (not (fboundp 'propertize))
    (defun erc-propertize (string &rest props)
      (while props
	(put-text-property 0 (length string)
			   (nth 0 props) (nth 1 props) string)
	(setq props (cddr props)))
      string)
  (defalias 'erc-propertize 'propertize))



(defcustom erc-prompt "ERC>"
  "Prompt used by ERC.  Trailing whitespace is not required."
  :group 'erc
  :type '(or string function)
  :get #'(lambda (symbol) (erc-string-no-properties (symbol-value symbol)))
  :set #'(lambda (symbol value) (set symbol (erc-propertize value 'read-only t 'rear-nonsticky t 'front-nonsticky t))))
(defun erc-prompt ()
  (if (functionp erc-prompt)
      (funcall erc-prompt)
      erc-prompt))

; Hmm, is this useful at all. If so, we would need to do it better, because
; one looses nickname completion when turning this variable on.
(defcustom erc-prompt-interactive-input nil
  "*If non-nil, input can be typed in the minibuffer instead.
This uses a local-map text-property to detect you started typing
at the prompt, and copies the last key as initial input into the minibuffer."
  :group 'erc
  :type 'boolean)

(defcustom erc-timestamp-format nil
  "*If set to a string, incoming messages will be timestampted.
This string is processed using `format-time-string'.
Good examples are \"%T \" and \"%H:%M \".

If nil, timestamping is turned off."
  :group 'erc
  :type '(choice (const nil)
		 (string)))

(defcustom erc-away-timestamp-format "<%H:%M>"
  "*Timestamp format used when marked as being away."
  :group 'erc
  :type 'string)

(defcustom erc-hide-timestamps nil
  "*If non-nil, timestamps will be invisible.

This is useful for logging, because, although timestamps will be
hidden, they will still be present in the logs."
  :group 'erc
  :type 'boolean)

(defcustom erc-echo-timestamps nil
  "*If non-nil, print timestamp in the minibuffer when point is moved.
Using this variable, you can turn off normal timestamping,
and simply move point to a irc message to see it's timestampt
printed in the minibuffer."
  :group 'erc
  :type 'boolean)

(defcustom erc-echo-timestamp-format "Timestamped %A, %H:%M:%S"
  "*Format string to be used when `erc-echo-timestamps' is non-nil.
This string specifies the format of the timestamp being echoed in
the minibuffer."
  :group 'erc
  :type 'string)

(defun erc-echo-timestamp (before now)
  "Print timestamp text-property of a IRC message.
Argument BEFORE is where point was before it got moved and
NOW is position of point currently."
  (when erc-echo-timestamps
    (let ((stamp (get-text-property now 'timestamp)))
      (when stamp
	(message (format-time-string erc-echo-timestamp-format
				     stamp))))))

(defcustom erc-notice-prefix "*** "
  "*Prefix for all notices."
  :group 'erc
  :type 'string)

(defcustom erc-notice-highlight-type 'all
  "*Determines how to highlight notices.
See `erc-notice-prefix'.

The following values are allowed:

    'prefix - highlight notice prefix only
    'all    - highlight the entire notice

Any other value disables notice's highlighting altogether."
  :group 'erc
  :type '(choice (const :tag "highlight notice prefix only" prefix)
		 (const :tag "highlight the entire notice" all)
		 (const :tag "don't highlight notices at all" nil)))

(defvar erc-uncontrol-input-line t
  "*If non-nil, interpret control characters on input.

The \"uncontrolled\" line usually looks much nicer (with bold and
other fonts).  There is no point in disabling it any more, since all
the color and font information is now preserved.  Consider it
deprecated.")

(defvar erc-interpret-controls-p t
  "*Whether to interpret the colors and other highlighting info or not.
The interpreting can require a lot of resources and in chatty
channels, or in an emergency (message flood) it can be turned off to
save processing time.")

;; other tunable parameters

(defcustom erc-auto-discard-away t
  "*If non-nil, sending anything when away automatically discards away state."
  :group 'erc
  :type 'boolean)

(defcustom erc-public-away-p nil
  "*Let others know you are back when you are no longer marked away.
This happens in this form:
* <nick> is back (gone for <time>)

Many consider it impolite to do so automatically."
  :group 'erc
  :type 'boolean)

(defcustom erc-away-nickname nil
  "*The nickname to take when you are marked as being away."
  :group 'erc
  :type '(choice (const nil)
		 string))

(defcustom erc-play-sound t
  "*Play sound on SOUND ctcp requests (used in ICQ chat)."
  :group 'erc
  :type 'boolean)

(defcustom erc-sound-path nil
  "List of directories that contain sound samples to play on SOUND events."
  :group 'erc
  :type '(repeat directory))

(defcustom erc-default-sound nil
  "Play this sound if the requested file was not found."
  :group 'erc
  :type '(choice (const nil)
		 file))

(defcustom erc-play-command "play"
  "Command for playing sound samples."
  :group 'erc
  :type 'string)

(defcustom erc-page-function nil
  "A function name to process a \"page\" request.
The function must take two arguments: SENDER and MSG, both strings.  Value
nil for this variable will cause the page message to appear in the minibuffer
window."
  :group 'erc
  :type '(choice (const nil)
		 (function-item)))

(defcustom erc-paranoid nil
  "If non-nil, then all incoming CTCP requests will be shown."
  :group 'erc
  :type 'boolean)

(defcustom erc-disable-ctcp-replies nil
  "Disable replies to CTCP requests that require a reply.
If non-nil, then all incoming CTCP requests that normally require
an automatic reply (like VERSION or PING) will be ignored.  Good to
set if some hacker is trying to flood you away."
  :group 'erc
  :type 'boolean)

(defcustom erc-anonymous-login t
  "Be paranoid, don't give away your machine name."
  :group 'erc
  :type 'boolean)

(defcustom erc-email-userid "user"
  "Use this as your email user ID."
  :group 'erc
  :type 'string)

(defcustom erc-ignore-list nil
  "*List of regexps matching user identifiers to ignore.
A user identifier has the form \"nick!login@host\".  If an id matches,
the message from the person will not be processed."
  :group 'erc
  :type '(repeat regexp))

(defvar erc-flood-protect 'normal
  "*If non-nil, flood protection is enabled.
Flooding is sending too much information to the server in too short time,
which may result in loosing connection.

If the value is 'strict, use a more strict limits provided in
`erc-flood-limit2', otherwise use \"normal\" limits from `erc-flood-limit'.")

(defvar erc-flood-limit '(1000 25 5)
  "Is a 3-element list (BYTES LINES SEC), defining the flood threshold:
at most BYTES bytes or LINES lines in messages within SEC seconds from
each other.  When either byte or line limit is exceeded, ERC stops
sending anything to the server, except for pings and one-line manual
user's commands.")

(defvar erc-flood-limit2 '(300 10 5)
  "Similar to `erc-flood-protect', but normally much more strict.
It will be used instead of `erc-flood-protect' in critical situations
\(detected flood, explicit user request, etc.).  Currently, it is
turned on when the flood limit is exceeded for the first time.")

;; Script parameters

(defcustom erc-startup-file-list
  '("~/.ercrc.el" "~/.ercrc" ".ercrc.el" ".ercrc")
  "List of files to try for a startup script.
The first existant and readable one will get executed.

If the filename ends with `.el' it is presumed to be an emacs-lisp
script and it gets (load)ed.  Otherwise is is treated as a bunch of
regular IRC commands"
  :group 'erc
  :type '(repeat file))

(defcustom erc-script-path nil
  "List of directories to look for a script in /load command.
The script is first searched in the current directory, then in each
directory in the list."
  :group 'erc
  :type '(repeat directory))

(defcustom erc-script-echo t
  "*If not-NIL, echo the IRC script commands locally."
  :group 'erc
  :type 'boolean)

(defcustom erc-max-buffer-size 30000
  "*Maximum size of each ERC buffer.  Used only when auto-truncation is enabled.
\(see `erc-truncate-buffer' and `erc-insert-hook')."
  :group 'erc
  :type 'integer)

(defcustom erc-log-channels-directory nil
  "The directory to place log files for channels.
If not nil, all the channel buffers are logged in separate files in that
directory.  The directory must exist, it will not be created, and it should
*not* end with a trailing slash."
  :group 'erc
  :type '(choice (const nil)
		 (directory :must-match t)))

(defcustom erc-log-channels nil
  "Enables/disables logging of channel buffers.  Nil means do not log."
  :group 'erc
  :type 'boolean)

(defvar erc-last-saved-position 1
  "The position in which appears last in the log file for the channel buffer.")

(defcustom erc-log-insert-log-on-open t
  "*Insert log file contents into the buffer if a log file exists."
  :group 'erc
  :type 'boolean)

(defcustom erc-truncate-buffer-on-save nil
  "Truncate any ERC (channel, query, server) buffer when it is saved.
When nil, no buffer is ever truncated.  Nonetheless, only the relevant
part of the buffer will be saved.")

(defcustom erc-generate-log-file-name-function 'erc-generate-log-file-name-long
  "*A function to generate a log filename.
The function must take five arguments: BUFFER, TARGET, NICK, SERVER and PORT.
BUFFER is the buffer to be saved,
TARGET is the name of the channel, or the target of the query,
NICK is the current nick,
SERVER and PORT are the parameters used to connect BUFFERs `erc-process'."
  :group 'erc
  :type '(choice (const erc-generate-log-file-name-long)
		 (const erc-generate-log-file-name-short)
		 (symbol)))

(defcustom erc-save-buffer-on-part nil
  "*Save the channel buffer content using `erc-save-buffer-in-logs' on PART."
  :group 'erc
  :type 'boolean)

(defcustom erc-kill-buffer-on-part nil
  "Kill the channel buffer on PART.
This variable should probably stay nil, as ERC can reuse buffers if
you rejoin them later."
  :group 'erc
  :type 'boolean)

(defcustom erc-kill-queries-on-quit nil
  "Kill all query (also channel) buffers of this server on QUIT.
See the variable `erc-kill-buffer-on-part' for details."
  :group 'erc
  :type 'boolean)

(defcustom erc-save-queries-on-quit nil
  "Save all query (also channel) buffers of the server on QUIT.
See the variable `erc-save-buffer-on-part' for details."
  :group 'erc
  :type 'boolean)

(defcustom erc-quit-reason-various-alist nil
  "Alist of possible arguments to the /quit command and what should happen
as a result.  For example:
  (setq erc-quit-reason-various-alist
      '((\"zippy\" erc-quit-reason-zippy)
	(\"xmms\" dme:now-playing)
	(\"version\" erc-quit-reason-normal)
	(\"home\" \"Gone home !\")))
If the user types \"/quit zippy\", then a Zippy the Pinhead quotation
will be used as the quit message.  The car of each element should be
a regexp.  If none match, the string is inserted directly."
  :group 'erc
  :type '(repeat (list regexp (choice (string) (function)))))

(defcustom erc-quit-reason 'erc-quit-reason-normal
  "A function which returns the reason for quitting.  Passed a single
argument, which is the string typed by the user after \"/quit\"."
  :group 'erc
  :type '(choice (const erc-quit-reason-normal)
		 (const erc-quit-reason-zippy)
		 (const erc-quit-reason-various)
		 (symbol)))

(defvar erc-grab-buffer-name "*erc-grab*"
  "The name of the buffer created by `erc-grab-region'.")

;; variables available for IRC scripts

(defvar erc-user-information "ERC User"
  "USER_INFORMATION IRC variable.")

(defvar erc-autogreet-script "hello.el"
  "Script name to use for autogreeting.")

;; Hooks

(defgroup erc-hooks nil
  "Hook variables for fancy customizations of ERC."
  :group 'erc)

(defcustom erc-mode-hook nil
  "Hook run after `erc-mode' setup is finished."
  :group 'erc-hooks
  :type 'hook
  :options '(erc-add-scroll-to-bottom))

(defcustom erc-timer-hook nil
  "Put functions which should get called more or less periodically here.
The idea is that servers always play ping pong with the client, and so there
is no need for any idle-timer games with Emacs."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-insert-pre-hook '(erc-nickserv-identify-autodetect)
  "Hook called first when some text is inserted through `erc-display-line'.
It gets called with one argument, STRING.
To be able to modify the inserted text, use `erc-insert-modify-hook' instead.
Filtering functions can set `erc-insert-this' to nil to avoid
display of that particular string at all."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-send-pre-hook '(erc-send-distinguish-noncommands)
  "Hook called first when some text is sent through `erc-send-current-line'.
It gets called with one argument, STRING.  To be able to modify the inserted
text, use `erc-send-modify-hook' instead.  Filtering functions can set
`erc-send-this' to nil to avoid sending of that particular string at
all and `erc-insert-this' to prevent inserting that particular string
into the buffer.

Note that it's useless to set `erc-send-this' to nil and
`erc-insert-this' to t.  ERC is sane enough to not insert the text
anyway."
  :group 'erc-hooks
  :type 'hook)

(defvar erc-insert-this t
   "Insert the text into the target buffer or not.
Functions on `erc-insert-pre-hook' can set this variable to nil
if they wish to avoid insertion of a particular string.")

(defvar erc-send-this t
  "Send the text to the target or not.
Functions on `erc-send-pre-hook' can set this variable to nil
if they wish to avoid sending of a particular string.")

(defcustom erc-insert-modify-hook '()
  "Insertion hook for functions that will change the text's appearance.
This hook is called just after `erc-insert-pre-hook' when the value
of `erc-insert-this' is t.
While this hook is run, narrowing is in effect and `current-buffer' is
the buffer where the text got inserted.  One possible value to add here
is `erc-fill'."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-insert-post-hook nil
  "This hook is called just after `erc-insert-modify-hook'.
At this point, all modifications from prior hook functions are done."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-send-modify-hook nil
  "Sending hook for functions that will change the text's appearance.
This hook is called just after `erc-send-pre-hook' when the values
of `erc-send-this' and `erc-insert-this' are both t.
While this hook is run, narrowing is in effect and `current-buffer' is
the buffer where the text got inserted.

Note that no function in this hook can change the appearance of the
text that is sent.  Only changing the sent text's appearance on the
sending user's screen is possible.  One possible value to add here
is `erc-fill'."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-send-post-hook nil
  "This hook is called just after `erc-send-modify-hook'.
At this point, all modifications from prior hook functions are done."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-insert-hook nil
  "Hook to run on any text insertion into an ERC buffer.
This hook is obsolescent.  See `erc-insert-pre-hook', `erc-insert-modify-hook'
and `erc-insert-post-hook' instead."
  :group 'erc
  :type 'hook
  :options '(erc-truncate-buffer
	     erc-scroll-to-bottom
	     erc-smiley))

(defcustom erc-input-hook nil
  "This HOOK is called with a string as argument.
The string is user input received.  This hook is obsolete.  See
`erc-send-pre-hook', `erc-send-modify-hook' and `erc-send-post-hook'
instead."
  :group 'erc-hooks
  :type 'hook)

(defcustom erc-message-hook nil
  "Hook run on all messages that arrive.
Run this hook on every message that arrives, from a channel or a
person, with arguments (MESSAGE TARGET NICK BUFFER-NAME IP LOGIN
SPEC).  This hook is obsolete.  See `erc-server-PRIVMSG-hook'
and `erc-server-NOTICE-hook'."
  :group 'erc
  :type 'hook)

(defcustom erc-disconnected-hook nil
  "Run this hook with arguments (NICK IP REASON) when disconnected.
This happens before automatic reconnection.  Note, that `erc-server-QUIT-hook'
might not be run when we disconnect, simply because we do not necessarily
receive the QUIT event."
  :group 'erc
  :type 'hook)

;; mode-specific tables

(defvar erc-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\" ".   " syntax-table)
    (modify-syntax-entry ?\\ ".   " syntax-table)
    (modify-syntax-entry ?' "w   " syntax-table)
    ;; Make dabbrev-expand useful for nick names
    (modify-syntax-entry ?< "." syntax-table)
    (modify-syntax-entry ?> "." syntax-table)
    syntax-table)
  "Syntax table used while in ERC mode.")

(defvar erc-mode-abbrev-table nil
  "Abbrev table used while in ERC mode.")
(define-abbrev-table 'erc-mode-abbrev-table ())

(defvar erc-mode-map
  (let ((map (make-sparse-keymap)))
;    (define-key map [(control return)] 'erc-send-paragraph)
    (define-key map "\C-m" 'erc-send-current-line)
    (define-key map "\t" 'erc-complete)
    (define-key map "\C-c\C-a" 'erc-bol)
    (define-key map "\C-c\C-b" 'erc-iswitchb)
    (define-key map "\C-c\C-c" 'erc-toggle-interpret-controls)
    (define-key map "\C-c\C-d" 'erc-input-action)
    (define-key map "\C-c\C-e" 'erc-toggle-ctcp-autoresponse)
    (define-key map "\C-c\C-f" 'erc-toggle-flood-control)
    (define-key map "\C-c\C-g" 'erc-grab-region)
    (define-key map "\C-c\C-i" 'erc-invite-only-mode)
    (define-key map "\C-c\C-j" 'erc-join-channel)
    (define-key map "\C-c\C-l" 'erc-save-buffer-in-logs)
    (define-key map "\C-c\C-m" 'erc-insert-mode-command)
    (define-key map "\C-c\C-n" 'erc-channel-names)
    (define-key map "\C-c\C-p" 'erc-part-from-channel)
    (define-key map "\C-c\C-r" 'erc-remove-text-properties-region)
    (define-key map "\C-c\C-s" 'erc-toggle-sound)
    (define-key map "\C-c\C-t" 'erc-set-topic)
    (define-key map "\C-c\C-u" 'erc-kill-input)
    (define-key map "\M-\t" 'ispell-complete-word)
    map)
  "ERC keymap.")

(defvar erc-info-mode-map (make-sparse-keymap)
  "Keymap used in `erc-info-mode'.")

;; Faces

; Honestly, I have a horrible sense of color and the "defaults" below
; are supposed to be really bad. But colors ARE required in IRC to
; convey different parts of conversation. If you think you know better
; defaults - send them to me.

;; Now colors are a bit nicer, at least to my eyes.
;; You may still want to change them to better fit your background.-- S.B.

(defgroup erc-faces nil
  "Faces for ERC."
  :group 'erc)

(defface erc-default-face '((t))
  "ERC default face."
  :group 'erc-faces)
(defface erc-direct-msg-face '((t (:foreground "IndianRed")))
  "ERC face used for messages you receive in the main erc buffer."
  :group 'erc-faces)
(defface erc-input-face '((t (:foreground "brown")))
  "ERC face used for your input."
  :group 'erc-faces)
(defface erc-bold-face '((t (:bold t)))
  "ERC bold face."
  :group 'erc-faces)
(defface erc-inverse-face
  '((t (:foreground "White" :background "Black")))
  "ERC inverse face."
  :group 'erc-faces)
(defface erc-underline-face '((t (:underline t)))
  "ERC underline face."
  :group 'erc-faces)
(defface erc-prompt-face
  '((t (:bold t :foreground "Black" :background"lightBlue2")))
  "ERC face for the prompt."
  :group 'erc-faces)
(defface erc-notice-face '((t (:bold t :foreground "SlateBlue")))
  "ERC face for notices."
  :group 'erc-faces)
(defface erc-action-face '((t (:bold t)))
  "ERC face for actions generated by /ME."
  :group 'erc-faces)
(defface erc-error-face '((t (:foreground "White" :background "Red")))
  "ERC face for errors."
  :group 'erc-faces)
(defface erc-timestamp-face '((t (:bold t :foreground "green")))
  "ERC timestamp face."
  :group 'erc-faces)

(defface fg:erc-color-face0 '((t (:foreground "White")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face1 '((t (:foreground "black")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face2 '((t (:foreground "blue4")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face3 '((t (:foreground "green4")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face4 '((t (:foreground "red")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face5 '((t (:foreground "brown")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face6 '((t (:foreground "purple")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face7 '((t (:foreground "orange")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face8 '((t (:foreground "yellow")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face9 '((t (:foreground "green")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face10 '((t (:foreground "lightblue1")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face11 '((t (:foreground "cyan")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face12 '((t (:foreground "blue")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face13 '((t (:foreground "deeppink")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face14 '((t (:foreground "gray50")))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face15 '((t (:foreground "gray90")))
  "ERC face."
  :group 'erc-faces)

(defface bg:erc-color-face0 '((t (:background "White")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face1 '((t (:background "black")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face2 '((t (:background "blue4")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face3 '((t (:background "green4")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face4 '((t (:background "red")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face5 '((t (:background "brown")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face6 '((t (:background "purple")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face7 '((t (:background "orange")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face8 '((t (:background "yellow")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face9 '((t (:background "green")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face10 '((t (:background "lightblue1")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face11 '((t (:background "cyan")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face12 '((t (:background "blue")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face13 '((t (:background "deeppink")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face14 '((t (:background "gray50")))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face15 '((t (:background "gray90")))
  "ERC face."
  :group 'erc-faces)

(defun erc-get-bg-color-face (n)
  "Fetches the right face for background color N (0-15)."
  (if (stringp n) (setq n (string-to-number n)))
  (if (not (numberp n))
      (progn
	(message (format "erc-get-bg-color-face: n is NaN: %S" n))
	(beep)
	'default)
    (when (> n 16)
      (erc-log (format "   Wrong color: %s" n))
      (setq n (mod n 16)))
    (cond
     ((and (>= n 0) (< n 16))
      (intern (concat "bg:erc-color-face" (number-to-string n))))
     (t (erc-log (format "   Wrong color: %s" n)) 'default))))

(defun erc-get-fg-color-face (n)
  "Fetches the right face for foreground color N (0-15)."
  (if (stringp n) (setq n (string-to-number n)))
  (if (not (numberp n))
      (progn
	(message (format "erc-get-fg-color-face: n is NaN: %S" n))
	(beep)
	'default)
    (when (> n 16)
      (erc-log (format "   Wrong color: %s" n))
      (setq n (mod n 16)))
    (cond
     ((and (>= n 0) (< n 16))
      (intern (concat "fg:erc-color-face" (number-to-string n))))
     (t (erc-log (format "   Wrong color: %s" n)) 'default))))

;; Debugging support

(defvar erc-log-p nil
  "When set to t, generate debug messages in a separate debug buffer.")

(defvar erc-debug-log-file (concat (expand-file-name ".") "/ERC.debug")
  "Debug log file name.")

(defvar erc-dbuf nil)
(make-variable-buffer-local 'erc-dbuf)

(defmacro erc-once-with-server-event (event &rest forms)
  "Execute FORMS the next time EVENT occurs in current buffer.
After FORMS are run (EVENT was triggered) EVENTs hook will be
restored to the previous state.
Last expression of FORMS should be either nil or t. nil indicates
that the other functions on that EVENT should be run too, t indicates
that other functions should not be run.
Please be sure to use that macro in server-buffers.  In channel-buffers it may
not work at all, as it uses the LOCAL argument of `add-hook' and `remove-hook'
to ensure multiserver capabilities."
  (let ((fun (gensym))
	(hook (intern
	       (concat "erc-server-"
		       (cond
			((symbolp event) (symbol-name event))
			((numberp event)
			 (setq event (number-to-string event))
			 (cond ((< (length event) 2)
				(setq event (concat "00" event))))
			 event))
		       "-hook"))))
    `(progn
       (defun ,fun (proc parsed)
	 (remove-hook (quote ,hook) (quote ,fun) t)
	 (fmakunbound (quote ,fun))
	 ,@forms)
       (add-hook (quote ,hook) (quote ,fun) nil t))))

(defmacro erc-log (string)
  "Logs STRING if logging is on (see `erc-log-p')."
  `(when erc-log-p
     (erc-log-aux ,string)))

(defun erc-server-buffer ()
  "Return the buffer object of the server buffer related to the current-buffer.
The buffer local variable `erc-process' is used to find the process buffer."
  (and (boundp 'erc-process)
       (processp erc-process)
       (process-buffer erc-process)))

(defun erc-server-buffer-p ()
  (eq (current-buffer) (erc-server-buffer)))

(defun erc-log-aux (string)
  "Do the debug logging of STRING."
  (let ((cb (current-buffer))
	(point 1)
	(was-eob nil)
	(session-buffer (erc-server-buffer)))
    (if session-buffer
	(progn
	  (set-buffer session-buffer)
	  (if (not (and erc-dbuf (bufferp erc-dbuf) (buffer-live-p erc-dbuf)))
	      (progn
		(setq erc-dbuf (get-buffer-create
				(concat "*ERC-DEBUG: "
					erc-session-server "*")))))
	  (set-buffer erc-dbuf)
	  (setq point (point))
	  (setq was-eob (eobp))
	  (goto-char (point-max))
	  (insert (concat "** " string "\n"))
	  (if was-eob (goto-char (point-max))
	    (goto-char point))
	  (set-buffer cb))
      (message (concat "ERC: ** " string)))))

;; Last active buffer, to print server messages in the right place

(defvar erc-active-buffer nil
  "The current active buffer, the one where the user typed the last command.")

;; Mode activation routines

;;;###autoload
(defun erc-mode ()
  "Major mode for Emacs IRC.
Special commands:

\\{erc-mode-map}

Turning on `erc-mode' runs the hook `erc-mode-hook'."
  (kill-all-local-variables)
  (use-local-map erc-mode-map)
  (setq mode-name "ERC"
	major-mode 'erc-mode
	local-abbrev-table erc-mode-abbrev-table)
  (set-syntax-table erc-mode-syntax-table)
  (when (boundp next-line-add-newlines)
    (set (make-local-variable 'next-line-add-newlines) nil))
  (make-variable-buffer-local 'paragraph-separate)
  (make-variable-buffer-local 'paragraph-start)
  (make-variable-buffer-local 'erc-last-saved-position)
  (erc-munge-invisibility-spec)
  (setq paragraph-separate (concat "\C-l\\|\\(^" (regexp-quote (erc-prompt))
				   "\\)"))
  (setq paragraph-start (concat "\\(" (regexp-quote (erc-prompt)) "\\)"))
  ;; Run the mode hooks
  (run-hooks 'erc-mode-hook))

;;;###autoload
(defun erc-info-mode ()
  "Major mode for Emacs IRC Channel Info buffers.
Special commands:

\\{erc-mode-map}."
  (kill-all-local-variables)

  (use-local-map erc-info-mode-map)
  (setq mode-name "ERC Info")
  (setq major-mode 'erc-info-mode)
  (toggle-read-only 1))


;; activation

(defconst erc-default-server "irc.openprojects.net"
  "IRC server to use if it cannot be detected otherwise.")

(defconst erc-default-port "ircd"
  "IRC port to use if it cannot be detected otherwise.")

(defcustom erc-join-buffer 'buffer
  "Determines how to display the newly created IRC buffer.
'window - in another window,
'frame - in another frame,
'bury - bury it in a new buffer,
any other value - in place of the current buffer"
  :group 'erc
  :type '(choice (const window)
		 (const frame)
		 (const bury)
		 (const buffer)))

(defcustom erc-join-info-buffer 'disable
  "Determines how to display the INFO buffer for a channel on join.
Values can be 'frame - in another frame, 'window - another window in
the same frame, 'split - split the current window in two and display
it in the lower subwindow.  Any other value will leave the info buffer
invisible."
  :group 'erc
  :type '(choice (const frame)
		 (const window)
		 (const split)
		 (const disable)))

(defcustom erc-frame-alist nil
  "*Alist of frame parameters for creating erc frames.
A value of `nil means to use `default-frame-alist'."
  :group 'erc
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Parameter")
		       (sexp :tag "Value"))))

(defcustom erc-frame-dedicated-flag nil
  "*Non-nil means the erc frames are dedicated to that buffer.
This only has effect when `erc-join-buffer' and `erc-join-info-buffer'
are set to `frame'."
  :group 'erc
  :type 'boolean)

(defun erc-channel-p (channel)
  "Return t if CHANNEL seems to be an IRC channel name."
  (cond ((stringp channel)
	 (memq (aref channel 0) '(?# ?& ?+ ?!)))
	((and (bufferp channel) (buffer-live-p channel))
	 (with-current-buffer channel
	   (erc-channel-p (erc-default-target))))
	(t nil)))

(defcustom erc-reuse-buffers t
  "*If nil, create new buffers on joining a channel/query.
If non-nil, a new buffer will only be created when you join
channels with same names on different servers, or have query buffers
open with nicks of the same name on different servers.  Otherwise,
the existing buffers will be reused."
  :group 'erc
  :type 'boolean)

(defun erc-normalize-port (port)
  "Normalize the port specification PORT to integer form.
PORT may be an integer, a string or a symbol.  If it is a string or a
symbol, it may have these values:
* irc         -> 194
* ircs        -> 994
* ircd        -> 6667
* ircd-dalnet -> 7000"
  (cond
   ((symbolp port)
    (erc-normalize-port (symbol-name port)))
   ((stringp port)
    (let ((port-nr (string-to-number port)))
      (cond
       ((> port-nr 0)
	port-nr)
       ((string-equal port "irc")
	194)
       ((string-equal port "ircs")
	994)
       ((string-equal port "ircd")
	6667)
       ((string-equal port "ircd-dalnet")
	7000)
       (t
	nil))))
   ((numberp port)
    port)
   (t
    nil)))

(defun erc-port-equal (a b)
  "Check whether ports A and B are equal."
  (= (erc-normalize-port a) (erc-normalize-port b)))

(defun erc-generate-new-buffer-name (server port target &optional proc)
  "Create a new buffer name based on the arguments."
  (when (numberp port) (setq port (number-to-string port)))
  (let* ((buf-name (or target
		       (or (let ((name (concat server ":" port)))
			     (when (> (length name) 1)
			       name))
			   ; This fallback should infact never happen
			   "*erc-server-buffer*"))))
    (if (and erc-reuse-buffers
	     (get-buffer buf-name)
	     (with-current-buffer (get-buffer buf-name)
	       (and (string= erc-session-server server)
		    (erc-port-equal erc-session-port port))))
	buf-name
      (generate-new-buffer-name buf-name))))

(defun erc-get-buffer-create (server port target &optional proc)
  "Creates a new buffer based on the arguments."
  (get-buffer-create (erc-generate-new-buffer-name server port target proc)))

(defun erc-downcase (string)
  "Convert STRING to IRC standard conforming downcase."
  (let ((s (downcase string))
	(c '((?\[ . ?\{)
	     (?\] . ?\})
	     (?\\ . ?\|)
	     (?~  . ?^)))
	(posn 0))
    (save-match-data
      (while (string-match "[]\\[~]" s posn)
	(aset s (match-beginning 0)
	      (cdr (assq (aref s (match-beginning 0)) c)))
	(setq p (match-end 0))))
    s))

;; (defun erc-get-buffer (target &optional proc)
;;   "Return the buffer matching TARGET in the process PROC.  If PROC is
;;  not supplied, all processes are searched."
;;   (save-excursion
;;     (let ((downcased-target (erc-downcase target)))
;;       (find-if (lambda (buf)
;;		 (set-buffer buf)
;;		 (and (eq major-mode 'erc-mode)
;;		      (or (not proc)
;;			  (equal proc erc-process))
;;		      (let ((target (erc-default-target)))
;;			(and target
;;			     (equal downcased-target (erc-downcase target))))))
;;	       (buffer-list)))))

(defun erc-get-buffer (target &optional proc)
  "Return the buffer matching TARGET in the process PROC.
If PROC is not supplied, all processes are searched."
  (let ((downcased-target (erc-downcase target))
	(bufs (buffer-list))
	res)
    (save-current-buffer
      (while bufs
	(set-buffer (car bufs))
	(setq bufs (cdr bufs))
	(when (and (or (not proc)
		       (and (boundp 'erc-process) (processp erc-process)
			    (eq proc erc-process)))
		   (let ((target (erc-default-target)))
		     (and target
			  (equal downcased-target (erc-downcase target)))))
	  (setq res (current-buffer))
	  (setq bufs nil)))
      res)))

(defun erc-buffer-filter (predicate &optional proc)
  "Return a list of `erc-mode' buffers matching certain criteria.
PREDICATE is a function executed with each buffer, if it returns t, that buffer
is considered a valid match.
PROC is either an `erc-process', identifying a certain server connection,
or nil which means all open connections."
  (save-excursion
    (delq
     nil
     (mapcar (lambda (buf)
	       (set-buffer buf)
	       (and (eq major-mode 'erc-mode)
		    (or (not proc)
			(eq proc erc-process))
		    (funcall predicate)
		    buf))
	     (buffer-list)))))

(defun erc-buffer-list (&optional predicate proc)
  "Return a list of ERC buffers.
PREDICATE is a function which executes with every buffer satisfying
the predicate.  If PREDICATE is passed as nil, return a list of all ERC
buffers.  If PROC is given, the buffers local variable `erc-process'
needs to match PROC."
  (unless predicate
    (setq predicate (lambda () t)))
  (erc-buffer-filter predicate proc))

(defmacro erc-with-all-buffers-of-server (process pred &rest forms)
  "Run FORMS in all buffers which have same process as this server.
FORMS will be evaluated in all buffers having the process PROCESS and
where PRED matches or in all buffers of the server process if PRED is
nil."
  `(mapcar (lambda (buffer)
	    (with-current-buffer buffer
	      ,@forms))
	   (erc-buffer-list ,pred
			    ,process)))

(defun erc-iswitchb (arg)
  "Use `iswitchb-read-buffer' to prompt for a ERC buffer to switch to.
When invoked with prefix argument, use all erc buffers.  Without prefix
ARG, allow only buffers related to same session server.
If `erc-track-modified-channels-mode' is in use, put the last
element of `erc-modified-channels-alist' in front of the buffer list.

Due to some yet unresolved reason, global function `iswitchb-mode'
needs to be active for this function to work."
  (interactive "P")
  (require 'iswitchb)
  (let ((iswitchb-make-buflist-hook
	 (lambda ()
	   (setq iswitchb-temp-buflist
		 (mapcar 'buffer-name
			 (erc-buffer-list
			  nil
			  (when (and arg (boundp 'erc-process))
			    erc-process)))))))
    (switch-to-buffer
     (iswitchb-read-buffer
      "Switch-to: "
      (car (last erc-modified-channels-alist))
      t))))

(defun erc-channel-list (proc)
  "Return a list of channel buffers.
PROC is the process for the server connection. If PROC is nil, return
all channel buffers on all servers."
  (erc-buffer-filter
   (lambda ()
     (and (erc-default-target)
	  (erc-channel-p (erc-default-target))))
   proc))

(defun erc-buffer-list-with-nick (nick &optional proc)
  "Return buffers containing NICK in the `channel-members' list.
If PROC is passed, only match buffers of given process."
  (setq nick (downcase nick))           ; Is this really necessary?
  (erc-buffer-filter
   (lambda ()
     (and (erc-default-target)
;	  (erc-channel-p (erc-default-target))
	  (boundp 'channel-members)
	  (find-if (lambda (n)
		     (string= (downcase (car n)) nick))
		   channel-members)))
   proc))

(defun erc (&optional server port nick full-name
		      connect passwd tgt-list channel process)
  "Run the Emacs Internet Relay Chat Client.
Connect to SERVER on PORT as NICK with FULL-NAME If CONNECT is
non-nil, connect to the server.  Otherwise assume already connected
and just create a separate buffer for the new target CHANNEL.  Use
PASSWD as user password on the server.  Returns the buffer for the
given channel."
  (let ((announced-server-name (when (and (boundp 'erc-session-server)
					  (string= server erc-session-server)
					  (boundp 'erc-announced-server-name))
				 erc-announced-server-name))
	(buffer (erc-get-buffer-create server port channel)))
    (when (not (eq buffer (current-buffer)))
      (when erc-log-p
	;; we can't log to debug buffer, it may not exist yet
	(message (format "erc: current buffer %s, switching to %s"
			 (current-buffer) buffer)))
      (cond ((eq erc-join-buffer 'window)
	     (switch-to-buffer-other-window buffer))
	    ((eq erc-join-buffer 'bury)
	     nil)
	    ((eq erc-join-buffer 'frame)
	     (funcall '(lambda (frame)
			 (raise-frame frame)
			 (select-frame frame))
		      (make-frame (or erc-frame-alist
				      default-frame-alist)))
	     (switch-to-buffer buffer)
	     (when erc-frame-dedicated-flag
	       (set-window-dedicated-p (selected-window) t)))
	    (t (switch-to-buffer buffer))))
    (set-buffer buffer)
    (setq erc-active-buffer buffer)
    (erc-mode)
    (set (make-local-variable 'erc-announced-server-name)
	 announced-server-name)
    ;; go to the end of the buffer and open a new line
    ;; (the buffer may have existed)
    (goto-char (point-max))
    (open-line 1)
    (goto-char (point-max))

    ;; make local variables

    ;; connection parameters
    (make-variable-buffer-local 'erc-session-server)
    (make-variable-buffer-local 'erc-session-port)
    (make-variable-buffer-local 'erc-session-user-full-name)
    (make-variable-buffer-local 'erc-process)
    (setq erc-process process)
    (make-variable-buffer-local 'erc-insert-marker)
    (setq erc-insert-marker (make-marker))
    (set-marker erc-insert-marker (point))
    ;; stack of default recepients
    (make-variable-buffer-local 'erc-default-recipients)
    (setq erc-default-recipients tgt-list)
    ;; stack for user's nicknames
    (make-variable-buffer-local 'nick-stk)
    (setq nick-stk ())
    ;; assoc list of pairs (TIME-OF-PING-REQUEST-SENT . DESTINATION)
    (make-variable-buffer-local 'pings)
    (setq pings ())
    ;; clear last incomplete line read
    (setq erc-previous-read "")
    ;; Channel members: only used in channel buffers.  It's a list of
    ;; user info entries '(nick op host email full-name ...).  Only
    ;; nick and op predicate must be present, other fields are not
    ;; required.
    (make-variable-buffer-local 'channel-members)
    (setq channel-members nil)
    ;; A topic string for the channel
    (make-variable-buffer-local 'channel-topic)
    (setq channel-topic "")
    ;; list of strings representing modes.  E.g. '("i" "m" "s" "b Quake!*@*")
    ;; (not sure the ban list will be here, but why not)
    (make-variable-buffer-local 'channel-modes)
    (setq channel-modes nil)
    ;; limit on the number of users on the channel (mode +l)
    (make-variable-buffer-local 'channel-user-limit)
    (setq channel-user-limit nil)
    ;; last peers (sender and receiver)
    (make-variable-buffer-local 'last-peers)
    (setq last-peers '(nil . nil))
    ;; last invitation channel
    (make-variable-buffer-local 'invitation)
    (setq invitation nil)
    ;; away flag
    ;; Should only be used in session-buffers
    (make-variable-buffer-local 'away)
    (setq away nil)
    ;; Server channel list
    (make-variable-buffer-local 'channel-list)
    (setq channel-list ())
;;;; Some flood protection stuff
    ;; time of last command sent
    (make-variable-buffer-local 'last-sent-time)
    (make-variable-buffer-local 'last-ping-time)
    ;; time of last CTCP response/request sent
    (make-variable-buffer-local 'last-ctcp-time)
    (setq last-sent-time (erc-current-time))
    (setq last-ping-time (erc-current-time))
    (setq last-ctcp-time (erc-current-time))
    (make-variable-buffer-local 'erc-lines-sent)
    (setq erc-lines-sent 0)
    (make-variable-buffer-local 'erc-bytes-sent)
    (setq erc-bytes-sent 0)
    ;; user requested quit
    (make-variable-buffer-local 'quitting)
    (setq quitting nil)
    ;; login-time 'nick in use' error
    (make-variable-buffer-local 'bad-nick)
    (setq bad-nick nil)
    ;; whether we have logged in
    (make-variable-buffer-local 'erc-logged-in)
    (setq erc-logged-in nil)
    ;; The local copy of `erc-nick' - the list of nicks to choose
    (make-variable-buffer-local 'erc-default-nicks)
    (setq erc-default-nicks (if (consp erc-nick) erc-nick (list erc-nick)))
    ;; password stuff
    (make-variable-buffer-local 'password)
    (setq password passwd)
    ;; debug output buffer
    (setq erc-dbuf
	  (when erc-log-p
	    (get-buffer-create (concat "*ERC-DEBUG: " server "*"))))

    (erc-determine-parameters server port nick full-name)

    ;; Saving log file on exit
    (when erc-log-channels-directory
      (auto-save-mode -1)
      (setq buffer-offer-save t
	    buffer-file-name (funcall erc-generate-log-file-name-function
				      buffer (erc-default-target)
				      (erc-current-nick) erc-session-server
				      erc-session-port))
      (if (boundp 'local-write-file-hooks)
	  (setq local-write-file-hooks
		'(erc-save-buffer-in-logs)) ;Emacs >=19
	(make-local-variable 'write-file-hooks)
	(setq write-file-hooks              ;Emacs 18
	      '(erc-save-buffer-in-logs)))
      (when erc-log-insert-log-on-open
	(ignore-errors (insert-file-contents buffer-file-name)))
      (setq erc-last-saved-position (point-max)))

    (if connect (erc-connect))
    (erc-update-mode-line)
    (goto-char (point-max))
    (open-line 1)
    (goto-char (point-max))
    (set-marker (process-mark erc-process) (point))
    (set-marker erc-insert-marker (point))
    (erc-display-prompt)
    (goto-char (point-max))

    buffer))

;; interactive startup

(defvar erc-server-history-list nil
  "IRC server interactive selection history list.")

(defvar erc-nick-history-list nil
  "Nickname interactive selection history list.")

(defun erc-already-logged-in (server port nick)
  "Return the buffers corresponding to a NICK on PORT of a session SERVER.
This is determined by looking for the appropriate buffer and checking
whether the connection is still alive.
If no buffer matches, return nil."
  (erc-buffer-list
   (lambda ()
     (and (boundp 'erc-process)
	  (processp erc-process)
	  (member (process-status erc-process) '(open run))
	  (string= erc-session-server server)
	  (erc-port-equal erc-session-port port)
	  (string= (erc-downcase nick) (erc-downcase (erc-current-nick)))))))

(if (not (fboundp 'read-passwd))
    (defun read-passwd (prompt)
      "Substitute for read-passwd in early emacsen"
      (read-from-minibuffer prompt)))

(defcustom erc-before-connect nil
  "Hook called before connecting to a server.
This hook gets executed before `erc-select' actually invokes `erc-mode'
with your input data.  The functions in here get called with three
parameters, SERVER, PORT and NICK."
  :group 'erc
  :type 'hook)

;;;###autoload
(defun erc-select (&optional server port nick)
  "Interactively select connection parameters and run ERC.
Optional argument SERVER uses server as default for the input query.
Optional argument PORT uses passed port as default for the input query.
Optional argument NICK uses the passed nick as default for the input query."
  (interactive)
  (if (null server) (setq server erc-server))
  (if (null port) (setq port erc-port))
  (setq nick (erc-compute-nick nick))
  (let* ((server (read-from-minibuffer
		  "IRC server: " server nil nil 'erc-server-history-list))
	 (port
	  (erc-string-to-port
	   (read-from-minibuffer "IRC port: "
				 (erc-port-to-string (or port "ircd")))))
	 (nick
	  (if (erc-already-logged-in server port nick)
	      (read-from-minibuffer
	       (erc-format-message 'nick-in-use ?n nick)
	       nick
	       nil nil 'erc-nick-history-list)
	    (read-from-minibuffer
	     "Nickname: " nick
	     nil nil 'erc-nick-history-list)))
	 (passwd (if erc-prompt-for-password
		     (if (and erc-password
			      (y-or-n-p "Use the default password? "))
			 erc-password
		       (read-passwd "Password: "))
		   erc-password)))
    (if (and passwd (string= "" passwd))
	(setq passwd nil))
    (while (erc-already-logged-in server port nick)
      ;; hmm, this is a problem when using multiple connections to a bnc
      ;; with the same nick. Currently this code prevents using more than one
      ;; bnc with the same nick. actually it would be nice to have
      ;; bncs transparent, so that erc-compute-buffer-name displays
      ;; the server one is connected to.
      (setq nick (read-from-minibuffer
		  (erc-format-message 'nick-in-use ?n nick)
		  nick
		  nil nil 'erc-nick-history-list)))
    (run-hook-with-args 'erc-before-connect server port nick)
    (erc server port nick erc-user-full-name t passwd)))

(defun erc-select-ssl (&optional server port nick)
  "Interactively select SSL connection parameters and run ERC.
Optional argument SERVER uses server as default for the input query.
Optional argument PORT uses passed port as default for the input query.
Optional argument NICK uses the passed nick as default for the input query."
  (interactive)
  (let ((erc-connect-function 'erc-open-ssl-stream))
    (erc-select server port nick)))

(defun erc-open-ssl-stream (name buffer host port)
  "Open an SSL stream to an IRC server..
The process will be given the name NAME, its target buffer will be
BUFFER.  HOST and PORT specify the connection target."
  (when (require 'ssl)
    (let ((proc (open-ssl-stream name buffer host port)))
      ;; Ugly hack, but it works for now. Problem is it is
      ;; very hard to detect when ssl is established, because s_client
      ;; doesnt give any CONNECTIONESTABLISHED kind of message, and
      ;; most IRC servers send nothing and wait for you to identify.
      (sit-for 5)
      proc)))

;;; process management

(defcustom erc-connect-function 'open-network-stream
  "Function used to initiate a connection.
It should take same arguments as `open-network-stream' does."
  :group 'erc
  :type 'function)

(defun erc-connect ()
  "Perform the connection and login."
  (let ((msg (erc-format-message 'connect
			     ?S erc-session-server
			     ?p erc-session-port)))
    (message msg)
    (setq erc-process
	  (funcall erc-connect-function
		   (format "erc-%s-%s" erc-session-server erc-session-port)
		   (current-buffer)
		   erc-session-server
		   erc-session-port))
    (message (concat msg "done")))
  (set-process-sentinel erc-process 'erc-process-sentinel)
  (set-process-filter erc-process 'erc-process-filter)
  (set-marker (process-mark erc-process) (point))
  (set-marker erc-insert-marker (point))
  (erc-log "\n\n\n********************************************\n")
  (message (erc-format-message 'login ?n (erc-current-nick)))
  (erc-login)
  ;; wait with script loading until we receive a confirmation (first
  ;; MOTD line)
  )

(defvar erc-previous-read ""
  "Variable used to cache partially received lines.")
(make-variable-buffer-local 'erc-previous-read)

(defun erc-split-multiline (string)
  "Split STRING, containing multiple lines and return them in a list.
This function is closely tied in to `erc-process-filter'.
It uses a buffer local varialbe called `erc-previous-read' to handle partial
strings."
  (let ((i0 0)
	(doit t) l)
    (while doit
      (let ((i (string-match "\r?\n" string i0))
	    (s (substring string i0)))
	(cond (i
	       (setq l (cons (concat erc-previous-read (substring string i0 i))
			     l))
	       (setq erc-previous-read "")
	       (setq i0 (match-end 0)))
	      ((> (length s) 0)
	       (setq erc-previous-read (concat erc-previous-read s))
	       (setq doit nil))
	      (t (setq doit nil)))))
    (nreverse l)))

(defun erc-process-filter (proc string)
  "Filter function for incoming server traffic.
PROC is the process where input came from and
STRING is the string received."
  (with-current-buffer (process-buffer proc)
    (mapc (lambda (line)
	    (erc-parse-line-from-server proc line))
	  (erc-split-multiline string))))

(defun erc-process-sentinel (cproc event)
  "Sentinel function for ERC process."
  (erc-log (format
	    "SENTINEL: proc: %S  status: %S  event: %S (quitting: %S)"
	    erc-process (process-status erc-process) event quitting))
  (save-excursion
    (set-buffer (process-buffer cproc))
;    (goto-char (point-max))

    (run-hook-with-args 'erc-disconnected-hook
			(erc-current-nick) (system-name) "")
    (if (string= event "exited abnormally with code 256\n")

	;; Sometimes (eg on a /LIST command) ERC happens to die with
	;; an exit code 256. The icrii client also shows this behavior
	;; and it restarts itself. So do I.

	(cond
	 ((not quitting)
	  (open-line 1)
	  (goto-char (point-max))
	  (insert
	   (erc-highlight-error
	    "Connection failed! Re-establishing connection...\n"))
	  (erc-update-mode-line)
	  (setq erc-active-buffer (current-buffer))
	  (setq last-sent-time 0)
	  (setq last-lines-sent 0)
	  ;; Insert code for rejoin mgmt here.
	  (erc-connect))

	 (t
	  (let* ((wd (window-width))
		(msg "*** ERC finished ***")
		(off (/ (- wd (length msg)) 2))
		(s ""))
	    (if (> off 0)
		(setq s (make-string off ? )))
	    (insert (concat "\n\n" s msg "\n")))))
      (insert (concat "\n\n*** ERC terminated: " event "\n"))))
  (goto-char (point-max))
  (erc-update-mode-line))


;;; I/O interface

;; send interface

;;; Mule stuff: send international characters the right way in Mule
;; capable emacsen, and don't screw those who can't do ISO code pages.

;; encode ISO to clear text, if possible
(or (fboundp 'encode-coding-string)
    (defun encode-coding-string (x y &optional z) x))
;; decode string from possibly ISO encoding
(or (fboundp 'decode-coding-string)
    (defun decode-coding-string (x y &optional z) x))

(defun erc-encode-coding-string (s)
  "Supposed to encode ISO-characters in \"clear text\" format.
Doesn't seem to work with colors, so does nothing at the moment"
  (encode-coding-string s 'ctext)
;   (let ((res "")
;	(i 0)
;	(nexti 0)
;	(l (length s)))
;     (while nexti
;       (setq nexti (string-match "[\C-b\C-_\C-c\C-v]" (substring s i)))
;       (if nexti (setq nexti (+ nexti i)))
;       (message "nexti = %s" nexti)
;       (setq res (concat res (encode-coding-string (substring s i nexti)
;						  'ctext)))
;       (if nexti
;	  (progn
;	    (setq res (concat res (substring s nexti (1+ nexti))))
;	    (setq i (1+ nexti)))))
;     res))
;;  s
)

(defun erc-decode-coding-string (s)
  "Supposed to decode ISO-characters from \"clear text\" format.
Doesn't seem to work with colors, so does nothing at the moment"
  ;; (decode-coding-string s 'ctext)
  s)

(defun erc-flood-exceeded-p (line)
  "Determines whether the flood limits are exceeded or not by the LINE.
It also maintains all the flood control variables."
  ;; First, clean up if no messages for long enough time
  (let ((flood (cond ((eq erc-flood-protect 'strict) erc-flood-limit2)
		     (erc-flood-protect erc-flood-limit)
		     (t nil))))
    (when (or (not flood)
	      (< (+ last-sent-time (nth 2 flood)) (erc-current-time)))
      (setq erc-lines-sent 0
	    erc-bytes-sent 0))
    ;; Update the variables for the new line
    (setq erc-lines-sent (1+ erc-lines-sent)
	  erc-bytes-sent (+ (length line) erc-bytes-sent)
	  last-sent-time (erc-current-time))
    ;; Now do what they ask
    (and flood
	 (let ((bytes (nth 0 flood))
	       (lines (nth 1 flood)))
	   (or (and lines (> erc-lines-sent lines))
	       (and bytes (> erc-bytes-sent bytes)))))))

(defun erc-send-command (l &optional force)
  "Send command line L to IRC server.
If the optional FORCE is non-nil, send the command even if the flood guard is
in effect and the limit is exceeded.

Return non-nil if the line is actually sent, nil otherwise.
The command line must contain neither prefix nor trailing `\\n'"
  (erc-log (concat "erc-send-command: " l "(" (buffer-name) ")"))
  (when (and (save-excursion
	       (and (boundp 'erc-process)
		    (processp erc-process)
		    (set-buffer (process-buffer erc-process))
		    away))
	     erc-auto-discard-away
	     (or (string-match "^PRIVMSG" l) (string-match "^NOTICE" l)))
    (erc-send-command "AWAY" t))
  (let ((ob (current-buffer))
	(buf (erc-server-buffer))
	res)
    (if buf
	(progn
	  (set-buffer buf)
	  (let ((exceeded (erc-flood-exceeded-p l)))
	    (if (or force (not exceeded))
		(progn
		  (when exceeded
		    (message "Warning: flood exceeded, but send forced")
		    (erc-log-aux
		     (format "ERC FLOOD PROTECT: flood exceeded, but send forced on %S\n"
			     l)))
		  (process-send-string erc-process
				       (concat (erc-encode-coding-string l) "\n"))
		  (setq res t))
	      (when (not erc-disable-ctcp-replies)
		(setq erc-disable-ctcp-replies t)
		(erc-display-line
		 (erc-highlight-error
		  "FLOOD PROTECTION: Automatic CTCP responses turned off.\n")
		 ob))
	      (when (not (eq erc-flood-protect 'strict))
		(setq erc-flood-protect 'strict)
		(erc-display-line
		 (erc-highlight-error
		  "FLOOD PROTECTION: Switched to Strict Flood Control mode.\n")
		 ob))
	      (message "ERC FLOOD PROTECT: line not sent: %S" l)
	      (erc-log-aux (format "ERC FLOOD PROTECT: line not sent: %S" l))))
	  (set-buffer ob))
      (message "ERC: No process running")
      (beep))
    res))

(defun erc-send-ctcp-message (tgt l &optional force)
  "Send CTCP message L to TGT. If TGT is nil the message is not sent.
The command must contain neither prefix nor trailing `\\n'"
  (cond
   (tgt
    (erc-log (format "erc-send-CTCP-message: [%s] %s" tgt l))
    (erc-send-command (format "PRIVMSG %s :\C-a%s\C-a" tgt l) force))))

(defun erc-send-ctcp-notice (tgt l &optional force)
  "Send CTCP notice L to TGT. If TGT is nil the message is not sent.
The command must contain neither prefix nor trailing `\\n'"
  (cond
   (tgt
    (erc-log (format "erc-send-CTCP-notice: [%s] %s" tgt l))
    (erc-send-command (format "NOTICE %s :\C-a%s\C-a" tgt l) force))))

(defun erc-send-action (tgt str &optional force)
  "Send CTCP ACTION information described by STR to TGT."
  (erc-send-ctcp-message tgt (format "ACTION %s" str) force)
  (erc-put-text-property 0 (length str)
			 'face 'erc-action-face str)
  (erc-display-line (concat (erc-format-timestamp)
			    (erc-format-message 'ACTION
						?n (erc-current-nick)
						?a str ?u "" ?h ""))
		    (current-buffer)))

(autoload 'erc-add-fool "erc-match" "Add a fool" t)
(autoload 'erc-add-pal "erc-match" "Add a pal" t)
(autoload 'erc-add-keyword "erc-match" "Add a keyword" t)
(autoload 'erc-add-dangerous-host "erc-match" "Add a dangerous host." t)
(autoload 'erc-delete-fool "erc-match" "Delete a fool" t)
(autoload 'erc-delete-pal "erc-match" "Delete a pal" t)
(autoload 'erc-delete-keyword "erc-match" "Delete a keyword" t)
(autoload 'erc-delete-dangerous-host "erc-match" "Delete a dangerous host" t)

(defconst erc-noncommands-list '(erc-cmd-ME erc-cmd-COUNTRY erc-cmd-SV)
  "List of commands that are just aliases for CTCP ACTION or for erc messages.")

;; Display interface

(defun erc-string-invisible-p (string)
  "Check whether STRING is invisible or not.
I.e. any char in it has the `invisible' property set."
  (text-property-any 0 (length string) 'invisible t string))

(defun erc-display-line-1 (string buffer)
  "Display STRING in `erc-mode' BUFFER.
Auxiliary function used in `erc-display-line'.  The line gets filtered to
interpret the control characters.  Then, `erc-insert-pre-hook' gets called.
If `erc-insert-this' is still t, STRING gets inserted into the buffer.
Afterwards, `erc-insert-modify' and `erc-insert-post-hook' get called.
If STRING is nil, the function does nothing."
  (when string
    (save-excursion
      (set-buffer (or buffer (process-buffer erc-process)))
      (let ((insert-position (or (marker-position erc-insert-marker)
				       (point-max)))
	    (string (erc-interpret-controls string))
	    (buffer-undo-list t)
	    (inhibit-read-only t))
	(unless (string-match "\n$" string)
	  (setq string (concat string "\n"))
	  (when (erc-string-invisible-p string)
	    (erc-put-text-properties 0 (length string) string '(invisible intangible))))
	(erc-log (concat "erc-display-line: " string
			   (format "(%S)" string) " in buffer "
			   (format "%s" buffer)))
	(setq erc-insert-this t)
	(run-hook-with-args 'erc-insert-pre-hook string)
	(when erc-insert-this
	  (save-excursion ;; to restore point in the new buffer
	    (goto-char insert-position)
	    ;; decode the ISO code page before inserting (for mule'ed emacsen)
	    (insert-before-markers (erc-decode-coding-string string))
;	    (when (and (boundp 'erc-insert-marker)
;		       (markerp erc-insert-marker))
;	      (set-marker erc-insert-marker (point)))
	    ;; Timestamp
	    (add-text-properties insert-position (point)
				 (list 'timestamp (current-time)))
	    (add-text-properties insert-position (point)
				 (list 'point-entered 'erc-echo-timestamp))
	    ;; run insertion hook, with point at restored location
	    (save-restriction
	      (narrow-to-region insert-position (point))
	      (run-hooks 'erc-insert-modify-hook)
	      (run-hooks 'erc-insert-post-hook))))))))

(autoload 'erc-fill "erc-fill" "Filling code for messages." t)

(defun erc-smiley ()
  "Smilify a region.  This function should be used with `erc-insert-modify-hook'."
  (when (fboundp 'smiley-region)
    (smiley-region (point-min) (point-max))))

(defvar erc-valid-nick-regexp "[]a-zA-Z^[;\\`_{}|][]^[;\\`_{}|a-zA-Z0-9-]*"
  "Regexp which matches all legal characters in a IRC nickname.")

(defun erc-is-valid-nick-p (nick)
  "Check if NICK is a valid IRC nickname."
  (and (<= (length nick) 9)
       (string-match (concat "^" erc-valid-nick-regexp "$") nick)))

(defun erc-scroll-to-bottom (window display-start)
  "This is added to `window-scroll-functions' by erc-add-scroll-to-bottom."
  (if (and window (window-live-p window))
      (save-selected-window
	(select-window window)
	(save-restriction
	  (widen)
	  (when (eobp)
	    (save-excursion
	    (recenter -1)
	      (sit-for 0))
	    (goto-char (point-max)))))))

(defun erc-add-scroll-to-bottom ()
  "A hook function for `erc-mode-hook' to recenter output at bottom of window.
This works whenever scrolling happens, so it's added to
`window-scroll-functions' rather than `erc-insert-post-hook'."
  (make-local-hook 'window-scroll-functions)
  (add-hook 'window-scroll-functions 'erc-scroll-to-bottom nil t))

(defun erc-display-line (string &optional buffer)
  "Display STRING in the ERC BUFFER.
All screen output must be done through this function.  If BUFFER is nil
or omitted, the default ERC buffer for the `erc-session-server' is used.
The BUFFER can be an actual buffer, a list of buffers, 'all or 'active.
If BUFFER = 'all, the string is displayed in all the ERC buffers for the
current session.  'active means the current active buffer
\(`erc-active-buffer').  If the buffer can't be resolved, the current
buffer is used.  `erc-display-line-1' is used to display STRING.

If STRING is nil, the function does nothing."
  (let ((inhibit-point-motion-hooks t)
	new-bufs)
    (dolist (buf (cond
		  ((bufferp buffer) (list buffer))
		  ((listp buffer) buffer)
		  ((eq 'all buffer)
		   (and (boundp 'erc-process)
			;; Hmm, or all of the same session server?
			(erc-buffer-list nil erc-process)))
		  ((eq 'active buffer)
		   (and erc-active-buffer (list erc-active-buffer)))
		  ((and (boundp 'erc-process) (processp erc-process))
		   (list (process-buffer erc-process)))
		  (t (list (current-buffer)))))
      (when (buffer-live-p buf)
	(erc-display-line-1 string buf)
	(add-to-list 'new-bufs buf)))
    (when (null new-bufs)
      (if (and (boundp 'erc-process) (processp erc-process))
	  (erc-display-line-1 string (process-buffer erc-process))
	(erc-display-line-1 string (current-buffer))))))

(defun erc-display-message-highlight (type string)
  (cond
   ((eq type 'notice)
    (erc-make-notice string))
   ((eq type 'error)
    (erc-highlight-error string))))

(defun erc-display-message (parsed type buffer msg &optional &rest args)
  (let ((string (if (symbolp msg)
		    (apply 'erc-format-message msg args)
		  msg)))
    (setq string
	  (cond
	   ((listp type)
	    (mapc (lambda (type)
		    (setq string (erc-display-message-highlight type string)))
		    type)
	    string)
	   ((symbolp type)
	    (erc-display-message-highlight type string))))

    (when (vectorp parsed)
      (erc-put-text-property 0 (length string) 'erc-parsed parsed string)
      (erc-put-text-property 0 (length string) 'rear-sticky t string))
    (erc-display-line
     string buffer)))

(defun erc-strip-erc-parsed-property ()
  "Strip the erc-parsed property from received messages.
This function was written to be used in `erc-insert-post-hook', to
reduce the memory usage of each ERC buffer."
  (remove-text-properties (point-min) (point-max) '(erc-parsed)))

(defun erc-process-input-line (line &optional force no-command)
  "Translate LINE to an RFC1459 command and send it based.
Returns non-nil if the command is actually sent to the server, and nil
otherwise.

If the command in the LINE is not boundp as a function `erc-cmd-COMMAND',
it is passed to `erc-cmd-default'.  If LINE is not a command (ie. doesn't
start with /<COMMAND>) then it is sent as a message.


An optional FORCE argument forces sending the line when flood
protection is in effect.  The optional NO-COMMAND argument prohibits
this function from interpreting the line as a command."
  (let ((command-list (erc-extract-command-from-line line)))
    (if	(and command-list
	     (not no-command))
	(let* ((cmd  (nth 0 command-list))
	       (args (nth 1 command-list))
	       (e (intern (concat "erc-cmd-" cmd)))
	       (fn (if (and cmd (fboundp e))
		       e
		     #'erc-cmd-default)))
	  (erc-log (format "proc-input: lookup: [%S] [%S] -> %S" cmd args e))
	  (if (not (funcall fn (concat (unless (and cmd (fboundp e)) cmd) args) force))
	      (progn
		(erc-display-line (erc-highlight-error "Bad syntax\n")
				  (current-buffer))
		nil)
	  t))
      (let ((r (erc-default-target)))
	(if r
	    (erc-send-command (format "PRIVMSG %s :%s" r line) force)
	  (erc-display-line (erc-highlight-error "No target\n")
			    (current-buffer))
	  nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Input commands handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erc-cmd-SET (line &optional force)
  "Set the variable named by the first word in LINE to some VALUE.
VALUE is computed by evaluating the rest of LINE in lisp."
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\s-+\\(.*\\)$" line)
    (let ((var (read (match-string 1 line)))
	  (val (read (match-string 2 line))))
      (if (boundp var)
	  (progn
	    (set var (eval val)) t)
	(erc-display-line (erc-highlight-error "Variable not bound!\n")
			  'active)
	nil)))
   ((string-match "^\\s-*$" line)
    (erc-display-line
     (concat "Available user variables:\n"
	     (apply
	      'concat
	      (mapcar
	       (lambda (x)
		 (format "%s\n" x))
	       (apropos-internal "^erc-" 'user-variable-p))))
     (current-buffer)) t)
   (t nil)))

(defun erc-cmd-default (line &optional force)
  "Fallback command.  Commands for which no erc-cmd-xxx exists, are
tunneled through this function.  LINE is sent to the server verbatim,
and therefore has to contain the command itself as well."
  (erc-log (format "cmd: DEFAULT: %s" line))
  (erc-send-command line force)
  t)

(defun erc-cmd-default-channel (line &optional force)
  "FIXME: no clue what this is supposed to do."
  (let ((tgt (erc-default-target))
	(msg line))
    (when (string-match "^\\s-*\\([&#+!]\\S-+\\)\\(.*\\)$" line)
      (setq tgt (match-string 1 line)
	    msg (match-string 2 line)))
    (if tgt
	(progn
	  (erc-log (format "cmd: DEFAULT: %s %s %s" cmd tgt line))
	  (erc-send-command (concat cmd " " tgt line) force))
      (erc-display-line (erc-highlight-error "No target\n")
			(current-buffer))))
  t)

(defun erc-cmd-CLEAR (line &optional force)
  "Clear the window content."
  (recenter 0)
  t)

(defun erc-cmd-COUNTRY (line &optional force)
  "Display the country name for the country code given by LINE."
  (require 'mail-extr)
  (erc-display-line (what-domain (erc-trim-string line))
		    (current-buffer))
  t)

(defun erc-cmd-AWAY (line &optional force)
  "Mark the user as being away, the reason being indicated by LINE."
  (cond
   ((string-match "^\\s-*\\(.*\\)$" line)
    (let ((reason (match-string 1 line)))
      (erc-log (format "cmd: AWAY: %s" reason))
      (erc-send-command
       (if (string= reason "")
	   "AWAY"
	 (concat "AWAY :" reason)) force))
    t)
   (t nil)))

(defun erc-cmd-GAWAY (line &optional force)
  "Mark the user as being away everywhere, the reason being indicated by LINE."
  ;; on all server buffers.
  (erc-with-all-buffers-of-server nil #'erc-server-buffer-p
				  (erc-cmd-AWAY line force)))

(defun erc-cmd-CTCP (line &optional force)
  "Send the Client To Client Protocol message indicated by LINE."
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\s-+\\(.*\\)$" line)
    (let ((nick (match-string 1 line))
	  (cmd (match-string 2 line)))
      (erc-log (format "cmd: CTCP [%s]: [%s]" nick cmd))
      (erc-send-ctcp-message nick cmd force))
    t)
   (t nil)))

(defun erc-cmd-HELP (line &optional force)
  "Popup help information.
If LINE contains a valid function or variable, help about that will be displayed.
If LINE is empty, display an apropos about erc commands.
Otherwise, do apropos in erc-namespace (\"erc-.*LINE\".

Examples:
To find out about erc and bbdb, do
  /help bbdb.*

For help about the WHOIS command, do:
  /help whois

For a list of user commands (/join /part, ...):
  /help
"
  (cond
   ((string-match "\\([^ \t\n()'\"]+\\)" line)
    (let* ((frag (match-string 1 line))
	   (sym (or (let ((sym (intern frag)))
		      (if (or (boundp sym) (fboundp sym))
			  sym
			nil))
		    (let ((sym (intern (concat "erc-" frag))))
		      (if (or (boundp sym) (fboundp sym))
			  sym
			nil))
		    (let ((sym (intern (concat "erc-cmd-" (upcase frag)))))
		      (if (or (boundp sym) (fboundp sym))
			  sym
			nil)))))
      (if sym
	  (cond
	   ((boundp sym) (describe-variable sym))
	   ((fboundp sym) (describe-function sym))
	   (t nil))
	(apropos-command (concat "erc-.*" frag) nil
			 (lambda (x)
			   (or (commandp x)
			       (get x 'custom-type))))
	t)))
   (t (apropos "erc-cmd-")
      (message "Type C-h m to get additional information about keybindings.")
      t)))

(defalias 'erc-cmd-H 'erc-cmd-HELP)


(defun erc-cmd-JOIN (line &optional force)
  "Join the channel given in LINE."
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\s-+\\(.+\\)$" line)
    (let ((chnl (erc-ensure-channel-name (match-string 1 line)))
	  (key (match-string 2 line)))
      (erc-log (format "cmd: JOIN: %s" s))
      (when (and chnl key)
	(erc-send-command (format "JOIN %s %s" chnl key) force)
	(erc-send-command (format "MODE %s" chnl) force))
      t))
   ((string-match "^\\s-*\\(\\S-+\\)\\s-*$" line)
    (let ((s (erc-ensure-channel-name (match-string 1 line)))
	  chnl)
      (erc-log (format "cmd: JOIN: %s" s))
      (cond
       ((string= (upcase s) "-INVITE")
	(if invitation
	    (setq chnl invitation
		  invitation nil)
	  (message "You've got no invitation")))
       (t
	(setq chnl s)))
      (cond
       (chnl
	(erc-send-command (format "JOIN %s" chnl) force)
	(erc-send-command (format "MODE %s" chnl) force))))
    t)
   (t nil)))
(defalias 'erc-cmd-CHANNEL 'erc-cmd-JOIN)
(defalias 'erc-cmd-J 'erc-cmd-JOIN)

(defvar erc-last-channel-names nil
  "If non-nil, it is a list of names in a channel whose names are about
to be listed.  On channels with too many people, the server sends
names in several lines, and this variable is used to save the extra
information (host IP, login name, etc.) that has been collected so
far, and that /names #channel doesn't provide.")

(defun erc-cmd-NAMES (line &optional force)
  "Display the users in the channel indicated by LINE.
If LINE is the empty string, display the users in the current channel.
This function clears the channel name list first, then sends the
command"
  (let ((tgt (erc-default-target))
	(msg line))
    (when (string-match "^\\s-*\\([&#+!]\\S-+\\)\\(.*\\)$" line)
      (setq tgt (match-string 1 line)
	    msg (match-string 2 line)))
    (if (and tgt (erc-channel-p tgt))
	(progn
	  (erc-log (format "cmd: DEFAULT: NAMES %s %s" tgt line))
	  (setq erc-last-channel-names (erc-get-channel-members tgt))
	  (erc-refresh-channel-members tgt "")
	  (erc-send-command (concat "NAMES " tgt msg) force))
      (erc-display-line (erc-highlight-error "No default channel\n")
			(current-buffer))))
  t)
(defalias 'erc-cmd-N 'erc-cmd-NAMES)

(defun erc-cmd-KICK (line &optional force)
  "Kick the user indicated in LINE from the current channel.
LINE has the format: \"#CHANNEL NICK REASON\" or \"NICK REASON\"."
  (cond
   ((string-match "^\\s-*\\([&#+!]\\S-+\\)\\s-+\\(\\S-+\\)\\(\\s-.*\\)?$" line)
    (let ((ch (match-string 1 line))
	  (nick (match-string 2 line))
	  (reason (match-string 3 line)))
      (if (not reason) (setq reason ""))
      (erc-log (format "cmd: KICK: %s/%s: %s" nick ch reason))
      (erc-send-command (format "KICK %s %s :%s" ch nick reason) force))
    t)
   ((string-match "^\\s-*\\(\\S-+\\)\\(\\s-.*\\)?$" line)
    (let ((ch (erc-default-target))
	  (nick (match-string 1 line))
	  (reason (match-string 2 line)))
      (if ch
	  (progn
	    (if (not reason) (setq reason ""))
	    (erc-log (format "cmd: KICK: %s/%s: %s" nick ch reason))
	    (erc-send-command (format "KICK %s %s :%s" ch nick reason) force))
	(erc-display-line (erc-highlight-error "No default channel\n")
			  (current-buffer))))
    t)
   (t nil)))

(defun erc-cmd-LOAD (line &optional force)
  "Load the script provided in the LINE.
If LINE continues beyond the file name,
the rest of it is put in a (local) variable
`erc-script-args', which can be used in elisp scripts.

The optional FORCE argument is ignored here - you can't force loading
a script after exceeding the flood threshold."
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\(.*\\)$" line)
    (let* ((file0 (match-string 1 line))
	   (erc-script-args (match-string 2 line))
	   (file (erc-find-file file0 erc-script-path)))
      (erc-log (format "cmd: LOAD: %s" file))
      (cond
       ((not file)
	(erc-display-line
	 (erc-highlight-error (concat "Cannot find file " file0))))
       ((not (file-readable-p file))
	(erc-display-line
	 (erc-highlight-error (concat "Cannot read file " file))))
       (t
	(message "Loading \'%s\'..." file)
	(erc-load-script file)
	(message "Loading \'%s\'...done" file))))
    t)
   (t nil)))

(defun erc-cmd-WHOIS (line &optional force)
  "Display whois information for the user given in LINE."
  (erc-log (format "cmd: WHOIS: %s" line))
  (erc-send-command (format "WHOIS %s" line) force)
  t)
(defalias 'erc-cmd-WI 'erc-cmd-WHOIS)

(defun erc-cmd-DESCRIBE (line &optional force)
  "Pose some action to a certain user.
LINE has the format \"USER ACTION\"."
  (cond
   ((string-match
     "^\\s-*\\(\\S-+\\)\\s-\\(.*\\)$" line)
    (let ((dst (match-string 1 line))
	  (s (match-string 2 line)))
      (erc-log (format "cmd: DESCRIBE: [%s] %s" dst s))
      (erc-send-action dst s force))
    t)
   (t nil)))

(defun erc-cmd-ME (line &optional force)
  "Pose some action."
  (cond
   ((string-match "^\\s-\\(.*\\)$" line)
    (let ((s (match-string 1 line)))
      (erc-log (format "cmd: ME: %s" s))
      (erc-send-action (erc-default-target) s force))
    t)
   (t nil)))

(defun erc-message (message-command line &optional force)
  "Send LINE to the server as a privmsg or a notice.
MESSAGE-COMMAND should be either \"PRIVMSG\" or \"NOTICE\"."
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\(\\s-*$\\|\\s-+.*\\)" line)
    (let ((tgt (match-string 1 line))
	  (s (match-string 2 line)))
      (erc-log (format "cmd: MSG(%s): [%s] %s" message-command tgt s))
      (if (string-match "^ " s) (setq s (substring s 1)))
      (cond
       ((string= tgt ",")
	(if (car last-peers)
	    (setq tgt (car last-peers))
	  (setq tgt nil)))
       ((string= tgt ".")
	(if (cdr last-peers)
	    (setq tgt (cdr last-peers))
	  (setq tgt nil))))
      (cond
       (tgt
	(setcdr last-peers tgt)
	(erc-send-command
	 (format "%s %s :%s" message-command tgt s)) force)
       (t
	(erc-display-line (erc-highlight-error "No target\n")))))
    t)
   (t nil)))

(defun erc-send-message (line &optional force)
  "Send a message to the current channel or user, and display it."
  (erc-message "PRIVMSG" (concat (erc-default-target) " " line) force)
  (erc-display-line
   (concat (erc-format-timestamp)
	   "<" (erc-current-nick) "> "
	   line)
   (current-buffer))
  ;; FIXME - run hooks, add face property
  t)

(defun erc-cmd-NOTICE (line &optional force)
  "Send a notice to the channel or user given as the first word in LINE.
The rest of LINE is the message to send."
  (erc-message "NOTICE" line force))

(defun erc-cmd-MSG (line &optional force)
  "Send a private message to the channel or user given as the first
word in LINE.  The rest of LINE is the message to send."
  (erc-message "PRIVMSG" line force))
(defalias 'erc-cmd-M 'erc-cmd-MSG)

(defun erc-cmd-NICK (line &optional force)
  "Change current nick to LINE."
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\s-*$" line)
    (let ((nick (match-string 1 line)))
      (erc-log (format "cmd: NICK: %s (bad-nick: %S)" nick bad-nick))
      (erc-send-command (format "NICK %s" nick) force)
      (cond (bad-nick
	     (erc-push-nick nick)
	     (erc-update-mode-line)
;	     (erc-rename-all-buffers);; no need so far
	     (setq bad-nick nil))))
    t)
   (t nil)))

(defun erc-cmd-PART (line &optional force)
  "When LINE is an empty string, leave the current channel.
Otherwise leave the channel indicated by LINE."
  (cond
   ((string-match "^\\s-*\\([&#+!]\\S-+\\)\\(\\s-+.*\\)?$" line)
    (let ((ch (match-string 1 line))
	  (msg (match-string 2 line)))
      (if msg
	  (setq msg (substring msg 1))
	(setq msg ""))
      (erc-log (format "cmd: PART: %s: %s" ch msg))
      (erc-send-command
       (if (string= msg "")
	   (format "PART %s" ch)
	 (format "PART %s :%s" ch msg)) force))
    t)
   ((string-match "^\\(.*\\)$" line)
    (let ((ch (erc-default-target))
	  (msg (match-string 1 line)))
      (if (not msg) (setq msg ""))
      (if (and ch (erc-channel-p ch))
	  (progn
	    (erc-log (format "cmd: PART: %s: %s" ch msg))
	    (erc-send-command
	     (if (string= msg "")
		 (format "PART %s" ch)
	       (format "PART %s :%s" ch msg)) force))
	(erc-display-line (erc-highlight-error "No target\n")
			  (current-buffer))))
    t)
   (t nil)))
(defalias 'erc-cmd-LEAVE 'erc-cmd-PART)

(defun erc-cmd-PING (line &optional force)
  "Ping the host given in LINE."
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\s-*$" line)
    (let* ((s (match-string 1 line))
	   (ct (erc-current-time))
	   (str (format "%f" ct))
	   (nn (if (string-match "^\\([0-9]+\\)" str)
	       (match-string 1 str) "0"))
	   (ob (current-buffer)))
      (erc-log (format "cmd: PING: %s" s))
      (erc-send-ctcp-message s (format "PING %s" nn) force)
      (set-buffer (erc-server-buffer))
      (setq pings (cons (cons ct (erc-trim-string s)) pings))
      (set-buffer ob))
    t)
   (t nil)))

(defun erc-cmd-QUOTE (line &optional force)
  "Send text directly to the server.
All the text given as argument is sent to the sever as unmodified, just as
you provided it. Use this command with care!"
  (cond
   ((string-match "^\\s-\\(.+\\)$" line)
    (erc-send-command (match-string 1 line)))
   (t nil)))

(defun erc-cmd-QUERY (line &optional force)
  "Open a query to the user given in LINE."
  (cond
   ((string-match "^\\s-*\\(\\S-*\\)\\s-*$" line)
    (let ((target (match-string 1 line))
	  (session-buffer (erc-server-buffer)))
      (if (string= target "")
	  (erc-delete-query)
	(if (and session-buffer
		 (buffer-live-p session-buffer))
	    (set-buffer session-buffer))
	(let ((buffer (erc erc-session-server
			   erc-session-port
			   (erc-current-nick)
			   erc-session-user-full-name
			   nil
			   nil
			   erc-default-recipients
			   target
			   erc-process)))
	  (when buffer
	    (set-buffer buffer)))
	(erc-add-query target))
      (erc-update-mode-line))
    t)
   (t nil)))

(defun erc-quit-reason-normal (&optional s)
  "Normal quit message."
  (or s
      (format "\C-bERC\C-b v%s (IRC client for Emacs)"; - \C-b%s\C-b"
	      erc-version-string) ; erc-official-location)
  ))

(defun erc-quit-reason-zippy (&optional s)
  "Zippy quit message."
  (or s
      (replace-regexp-in-string "\n" "" (yow))))

(defun erc-quit-reason-various (s)
  "Choose a quit reason based on the string \"s\"."
  (let ((res (car (assoc-default
		   s erc-quit-reason-various-alist 'string-match))))
    (cond
     ((functionp res) (funcall res))
     ((stringp res) res)
     (s))))

(defalias 'erc-cmd-Q 'erc-cmd-QUERY)

(defun erc-cmd-QUIT (line &optional force)
  "Disconnect from the current server.
If LINE is an empty string, display a default quit message, otherwise display
the message given by LINE."
  (cond
   ((string-match "^\\s-*\\(.*\\)$" line)
    (let* ((s (match-string 1 line))
	   (ob (current-buffer))
	   (buffer (erc-server-buffer))
	   (reason (funcall erc-quit-reason (if s s ""))))
      (if (and buffer (bufferp buffer)) (set-buffer buffer))
      (erc-log (format "cmd: QUIT: %s" reason))
      (setq quitting t)
      (erc-send-command (format "QUIT :%s" reason) force)
      (set-buffer ob)
      (when erc-save-queries-on-quit
	(erc-save-query-buffers erc-process))
      (when erc-kill-queries-on-quit
	(erc-kill-query-buffers erc-process)))
    t)
   (t nil)))
(defalias 'erc-cmd-BYE 'erc-cmd-QUIT)
(defalias 'erc-cmd-EXIT 'erc-cmd-QUIT)
(defalias 'erc-cmd-SIGNOFF 'erc-cmd-QUIT)

(defun erc-cmd-SERVER (line &optional force)
  "Make the server given by LINE the active server."
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\s-*$" line)
    (let ((nserver (match-string 1 line)))
      (erc-log (format "cmd: SERVER: %s" nserver))
      (erc-select nserver nil (erc-current-nick)))
    t)
   (t nil)))

(defun erc-cmd-SV (line &optional force)
  "Say the current ERC and Emacs version into channel."
  (erc-send-message (format "I'm using ERC %s with %s %s!"
			    erc-version-string
			    (if (member 'xemacs features) "XEmacs" "Emacs")
			    emacs-version) force)
  t)

(defun erc-cmd-DEOP (line &optional force)
  "Remove the operator setting from user(s) given in LINE."
  (let ((ppl (split-string line " ")))
    (when (> (length ppl) 0)
      (erc-send-command (concat "MODE " (erc-default-target)
				" -"
				(make-string (length ppl) ?o)
				" "
				(reduce (lambda (&optional a b)
					  (concat a " " b))
					ppl)) force))
    t))

(defun erc-cmd-OP (line &optional force)
  "Add the operator setting to users(s) given in LINE."
  (let ((ppl (split-string line " ")))
    (when (> (length ppl) 0)
      (erc-send-command (concat "MODE " (erc-default-target)
				" +"
				(make-string (length ppl) ?o)
				" "
				(reduce (lambda (&optional a b)
					  (concat a " " b))
					ppl)) force))
    t))

(defun erc-cmd-TIME (line &optional force)
  "Request the current time and date from the current server."
  (cond
   ((string-match "^\\s-*\\(.*\\)$" line)
    (let ((args (match-string 1 line)))
      (erc-log (format "cmd: TIME: %s" args))
      (erc-send-command (concat "TIME " args) force))
    t)
   (t nil)))
(defalias 'erc-cmd-DATE 'erc-cmd-TIME)

(defun erc-cmd-TOPIC (line &optional force)
  "Set or request the topic for a channel.
LINE has the format: \"#CHANNEL TOPIC\", \"#CHANNEL\", \"TOPIC\"
or the empty string.

If no #CHANNEL is given, the default channel is used.  If TOPIC is
given, the channel topic is modified, otherwise the current topic will
be displayed."
  (cond
   ;; /topic #channel
   ((string-match "^\\s-*\\([&#+!]\\S-+\\)" line)
    (let ((ch (match-string 1 line)))
      (erc-send-command (format "TOPIC %s" ch))
      t))
   ;; /topic
   ((string-match "^\\s-*$" line)
    (let ((ch (erc-default-target)))
      (erc-send-command (format "TOPIC %s" ch))
      t))
   ;; /topic #channel TOPIC
   ((string-match "^\\s-*\\([&#+!]\\S-+\\)\\s-\\(.*\\)$" line)
    (let ((ch (match-string 1 line))
	  (topic (match-string 2 line)))
      (erc-log (format "cmd: TOPIC [%s]: %s" ch topic))
      (erc-send-command (format "TOPIC %s :%s" ch topic) force))
    t)
   ;; /topic TOPIC
   ((string-match "^\\s-*\\(.*\\)$" line)
    (let ((ch (erc-default-target))
	  (topic (match-string 1 line)))
      (if (and ch (erc-channel-p ch))
	  (progn
	    (erc-log (format "cmd: TOPIC [%s]: %s" ch topic))
	    (erc-send-command (format "TOPIC %s :%s" ch topic) force))
	(erc-display-line (erc-highlight-error "No target\n")
			  (current-buffer))))
    t)
   (t nil)))
(defalias 'erc-cmd-T 'erc-cmd-TOPIC)

(defun erc-cmd-CLEARTOPIC (line &optional force)
  "Clear the topic for a channel.
If LINE is the empty string, clear the topic for the default channel."
  (let ((channel (cond ((string-match "^\\s-*$" line)
			(erc-default-target))
		       ((string-match "^\\s-*\\([&#+!]\\S-+\\)" line)
			(match-string 1 line))
		       (t
			nil))))
    (when channel
      (erc-send-command (format "TOPIC %s :" channel))
      t)))

(defun erc-cmd-SOUND (line &optional force)
  "Play the sound given in LINE."
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\(\\s-.*\\)?$" line)
    (let ((file (match-string 1 line))
	  (msg (match-string 2 line))
	  (tgt (erc-default-target)))
      (if (null msg)
	  (setq msg "")
	;; remove the first white space
	(setq msg (substring msg 1)))
      (if tgt
	  (progn
	    (erc-send-ctcp-message tgt (format "SOUND %s %s" file msg) force)
	    (if erc-play-sound (erc-play-sound file)))
	(erc-display-line (erc-highlight-error "No target\n")
			  (current-buffer)))
      t))
   (t nil)))

;;;; End of IRC commands

(defun erc-ensure-channel-name (channel)
  "Return CHANNEL if it is a valid channel name.
Eventually add a # in front of it, if that turns it into a valid channel name."
  (if (erc-channel-p channel)
      channel
    (concat "#" channel)))

(defun erc-reformat-command (line)
  "Re-format the command in LINE.
Removes leading [and trailing?] spaces and converts a command to uppercase.
Returns a string untouched, if it doesn't contain a command"

  (if (string-match "^\\s-*/\\([A-Za-z]+\\)\\(\\s-+.*\\|\\s-*\\)$" line)
      (concat "/" (upcase (match-string 1 line)) " " (match-string 2 line))
    line))

(defun erc-make-property-list (fg bg bold ul inv)
  "Compiles the list of properties from flags.
Compiles the list of properties from the FG, BG colors and BOLD, UL (underline)
and INV (inverse) flags."
  (let (res)
    (if ul (setq res (cons 'erc-underline-face res)))
    (if bold (setq res (cons 'erc-bold-face res)))
    (if fg (setq res (cons (if inv
			       (erc-get-bg-color-face fg)
			     (erc-get-fg-color-face fg))
			   res)))
    (if bg (setq res (cons (if inv
			       (erc-get-fg-color-face bg)
			     (erc-get-bg-color-face bg))
			   res)))
    (if (and inv (not (or fg bg))) (setq res (cons 'erc-inverse-face res)))
    res))

(defun erc-prepend-properties (obj start end fg bg bold ul inv)
  "Prepends text properties found in the flags to the OBJ.
Prepends text properties from START to END to the OBJ (string or buffer) that
are determined by BG, FG colors (0-15) and BOLD, UL (underline) and INV
\(inverse) flags"
  (let ((props (erc-make-property-list fg bg bold ul inv)))
    (when props
      ;;(erc-log (format "prepending property list: %s" props))
      (if fg (erc-put-text-property start end 'erc-fg fg obj))
      (if bg (erc-put-text-property start end 'erc-bg bg obj))
      (if bold (erc-put-text-property start end 'erc-bold bold obj))
      (if ul (erc-put-text-property start end 'erc-ul ul obj))
      (if inv (erc-put-text-property start end 'erc-inv inv obj))
      (font-lock-prepend-text-property start end 'face props obj))))

(defun erc-decode-controls (line)
  "Convert properties of LINE into explicit control characters.
Convert all 'erc-* properties of LINE into explicit control characters that can
be sent back to the server.  Useful for resending a colored line just by pasting
it at the prompt, or for grabbing color pop-ups that other people send."
  (let ((pos 0)
	(next-pos 0)
	fg bg bold ul inv
	;; previous values
	col bg0 bold0 ul0 inv0
	(res ""))
    (while pos
      (setq fg   (get-text-property pos 'erc-fg line)
	    bg   (get-text-property pos 'erc-bg line)
	    bold (get-text-property pos 'erc-bold line)
	    ul   (get-text-property pos 'erc-ul line)
	    inv  (get-text-property pos 'erc-inv line))
      (setq next-pos (next-property-change pos line))
      ;; put "end of color" only if not at the beginning of line,
      ;; and the color was on.  Also put it if the bg color dissapeared
      (if (or (and col (not fg) (not bg) (/= pos 0))
	      (and (not bg) bg0))
	  (setq res (concat res "\C-c")))
      (when fg
	(setq res (concat res (char-to-string ?\^c) (format "%02d" fg)))
	(if bg (setq res (concat res (format ",%02d" bg)))))
      (if (or (and bold (not bold0))
	      (and (not bold) bold0))
	  (setq res (concat res (char-to-string ?\^b))))
      (if (or (and ul (not ul0))
	      (and (not ul) ul0))
	  (setq res (concat res (char-to-string ?\^_))))
      (if (or (and inv (not inv0))
	      (and (not inv) inv0))
	  (setq res (concat res (char-to-string ?\^v))))
      (setq col (or fg bg)
	    bg0 bg
	    bold0 bold
	    ul0 ul
	    inv0 inv)
      (setq res (concat res (substring line pos next-pos)))
      (setq pos next-pos))
    res))

(defun erc-interpret-controls (line)
  "Translate control characters in the LINE to faces and beeps."
  (cond
   ((and (not (null line)) erc-interpret-controls-p)
    (let ((res "")
	  (i 0)
	  (j 0)
	  (len (length line))
	  (parts nil);; for partitioning the string into segments
	  (bg nil) ;; number 0-15 (color)
	  (fg nil) ;; number 0-15 (color)
	  (bold nil)    ;; boolean
	  (ul nil);; boolean (underline)
	  (inv nil);; boolean (inverse) - not sure how it'll work...
	  (hl-start 0);; position in the string where it starts
	  (hl-face nil);; the list of faces ready for
		       ;; "font-lock-prepend-text-property"
	  (prop-list nil));; list of arguments for `erc-prepend-properties'

      ;; First, decompose the string into parts without controls, and
      ;; the controls by themselves, and make a list of corresponding
      ;; substrings.  Then we'll combine them one chunk at a time, not
      ;; by one character, as was before.
      (setq j (string-match "\\(\C-b\\|\C-c\\|\C-_\\|\C-v\\|\C-g\\|\C-o\\)"
			    (substring line i)))
      (while j
	(let* ((rest (substring line j))
	       (s1 (substring line i j)) ;; "clean" part of the string
	       (ctrl (cond ((string-match "\\(\\(\C-c\\([0-9]\\([0-9]\\)?\\(,[0-9]\\([0-9]\\)?\\)?\\)?\\)\\|\C-b\\|\C-_\\|\C-v\\|\C-g\\|\C-o\\)" rest)
			    (setq i (+ j (match-end 1)))
			    (match-string 1 rest))
			   (t (erc-log-aux (concat "ERROR: erc-interpret-controls: no match in " rest))
			      (setq j nil)
			      ""))))
	  (if (not (string= s1 "")) (setq parts (cons s1 parts)))
	  (setq parts (cons ctrl parts))
	  (setq j (string-match "\\(\C-b\\|\C-c\\|\C-_\\|\C-v\\|\C-g\\|\C-o\\)"
			    (substring line i)))
	  (if j (setq j (+ i j)))))

      (if (< i len) (setq parts (cons (substring line i) parts)))
      (setq parts (nreverse parts))
      (setq i 0 j 0)
      ;;(erc-log (format "computed list of parts: %S" (current-time)))

      ;;(erc-log (format "erc-interpret-controls: parts = %S" parts))

      ;; The parts of the string are ready:
      (while parts
	(let ((c (string-to-char (car parts))))
;;	(erc-log (format "intp-c: i %d c [%c] st %S fc %S"
;;			 i c hl-start hl-face))
	  (cond ((char-equal c ?\^g)	; beep
		 (ding t))

		;; bold
		((char-equal c ?\^b)
;;	       (erc-log	(format "intp-c: BOLD i %d j %d st %S" i j hl-start))
		 (setq prop-list (cons (list hl-start j fg bg bold ul inv)
				       prop-list))
		 ;;(erc-prepend-properties hl-start j res fg bg bold ul inv)
		 (setq bold (not bold)
		       hl-start j))

		;; underline
		((char-equal c ?\^_)
		 (setq prop-list (cons (list hl-start j fg bg bold ul inv)
				       prop-list))
		 ;;(erc-prepend-properties hl-start j res fg bg bold ul inv)
		 (setq ul (not ul)
		       hl-start j))

		;; inverse
		((char-equal c ?\^v)
		 (setq prop-list (cons (list hl-start j fg bg bold ul inv)
				       prop-list))
		 ;;(erc-prepend-properties hl-start j res fg bg bold ul inv)
		 (setq inv (not inv)
		       hl-start j))
		;; mIRC color -- S.B.
		((char-equal c ?\^c)
		 (setq prop-list (cons (list hl-start j fg bg bold ul inv)
				       prop-list))
		 ;;(erc-prepend-properties hl-start j res fg bg bold ul inv)
		 ;; clear the fg color specs by default (keep the bg)
		 (setq hl-start j
		       fg nil
;;		       bg nil
		       )
		 (let ((ccode (substring (car parts) 1)))
		   (if (string-match
			"^\\([0-9]\\([0-9]\\)?\\(,[0-9]\\([0-9]\\)?\\)?\\)"
			ccode)
		       (let ((cspec (match-string 1 ccode)))
			 (cond ((string-match "^\\([0-9]+\\),\\([0-9]+\\)" cspec)
				(setq fg (string-to-number
					  (match-string 1 cspec))
				      bg (string-to-number
					  (match-string 2 cspec))))
			       (t (setq fg (string-to-number cspec)))))
		     ;; otherwise, reset the colors
		     (setq bg nil))))
		;; End of text property -- S.B.
		((char-equal c ?\^o);; - end of all properties (??)
		 (setq prop-list (cons (list hl-start j fg bg bold ul inv)
				       prop-list))
		 ;;(erc-prepend-properties hl-start j res fg bg bold ul inv)
		 (setq bg nil
		       fg nil
		       bold nil
		       ul nil
		       inv nil
		       hl-start j))
;		((char-equal c ?\^q)
;		 ;; I don't know how to interpret this.  Just skip it for now.
;		 nil)
;		((char-equal c ?\^r)
;		 ;; I don't know how to interpret this.  Just skip it for now.
;		 nil)
		(t
		 (setq res (concat res (car parts)))
		 (setq j (+ j (length (car parts)))))))
	(setq parts (cdr parts)))
      (setq prop-list (cons (list hl-start j fg bg bold ul inv)
			    prop-list))
      ;;(erc-prepend-properties hl-start j res fg bg bold ul inv)
      ;; now prepend all the properties
      (while prop-list
	(apply 'erc-prepend-properties (cons res (car prop-list)))
	(setq prop-list (cdr prop-list)))
      ;;(erc-log (format "done: %S" (current-time)))
      ;;(erc-log (concat "erc-interpret-controls: res = " res))
      res))
   ((not (null line)) line)
   (t nil)))

;(defun erc-interpret-controls (line)
;  "Translate control characters in the line to faces and beeps"

;  (cond
;   ((not (null line))
;    (let ((res "")
;	  (i 0)
;	  (j 0)
;	  (len (length line))
;	  (bg nil) ;; number 0-15 (color)
;	  (fg nil) ;; number 0-15 (color)
;	  (bold nil)    ;; boolean
;	  (ul nil);; boolean (underline)
;	  (inv nil);; boolean (inverse) - not sure how it'll work...
;	  (hl-start 0);; position in the string where it starts
;	  (hl-face nil));; the list of faces ready for
;                        ;; "font-lock-prepend-text-property"
;      (while (< i len)
;	(let ((c (string-to-char (substring line i))))
;;;	(erc-log (format "intp-c: i %d c [%c] st %S fc %S"
;;			 i c hl-start hl-face))
;	  (cond ((char-equal c ?\^g)	; beep
;		 (ding t))

;		;; bold
;		((char-equal c ?\^b)
;;;	       (erc-log	(format "intp-c: BOLD i %d j %d st %S" i j hl-start))
;		 (erc-prepend-properties hl-start j res fg bg bold ul inv)
;		 (setq bold (not bold)
;		       hl-start j))

;		;; underline
;		((char-equal c ?\^_)
;		 (erc-prepend-properties hl-start j res fg bg bold ul inv)
;		 (setq ul (not ul)
;		       hl-start j))

;		;; inverse
;		((char-equal c ?\^v)
;		 (erc-prepend-properties hl-start j res fg bg bold ul inv)
;		 (setq inv (not inv)
;		       hl-start j))
;		;; ICQ color -- S.B.
;		((char-equal c ?\^c)
;		 (erc-prepend-properties hl-start j res fg bg bold ul inv)
;		 ;; clear the fg color specs by default (keep the bg)
;		 (setq hl-start j
;		       fg nil
;;;		       bg nil
;		       )
;		 (let* ((rest (substring line (1+ i)))
;			(add-i 0)) ;; how much to advance 'i'
;		   (if (string-match
;			"^\\([0-9]\\([0-9]\\)?\\(,[0-9]\\([0-9]\\)?\\)?\\)"
;			rest)
;		       (let ((cspec (match-string 1 rest)))
;			 (setq add-i (length cspec))
;			 (cond ((string-match "^\\([0-9]+\\),\\([0-9]+\\)" cspec)
;				(setq fg (string-to-number
;					  (match-string 1 cspec))
;				      bg (string-to-number
;					  (match-string 2 cspec))))
;			       (t (setq fg (string-to-number cspec)))))
;		     ;; otherwise, reset the colors
;		     (setq bg nil))
;		   (setq i (+ i add-i))))
;		;; End of text property -- S.B.
;		((char-equal c ?\^o);; - end of all properties (??)
;		 (erc-prepend-properties hl-start j res fg bg bold ul inv)
;		 (setq bg nil
;		       fg nil
;		       bold nil
;		       ul nil
;		       inv nil
;		       hl-start j))
;;		((char-equal c ?\^q)
;;		 ;; I don't know how to interpret this.  Just skip it for now.
;;		 nil)
;;		((char-equal c ?\^r)
;;		 ;; I don't know how to interpret this.  Just skip it for now.
;;		 nil)
;		(t
;		 (setq res (concat res (char-to-string c)))
;		 (if (not (char-equal (string-to-char "\n") c))
;		     (put-text-property j (1+ j) 'face
;					(get-char-property i 'face line) res))
;		 (setq j (1+ j)))))
;	(setq i (1+ i)))
;      (erc-prepend-properties hl-start j res fg bg bold ul inv)
;      (erc-log (concat "erc-interpret-controls: res = " res))
;      res))

;   (t nil)))


(defun erc-merge-controls (line)
  "Translate and merge control characters and faces in LINE.
Translate control characters in the LINE to faces and beeps, then
converts all properties back into control symbols (to mix both user
entered and existing colors), and removes all the text properties from
the string.

This allows copying a colored line and sending it preserving the
colors, and also to edit it without decoding the colors first."
  (erc-decode-controls line)
;  (erc-decode-controls
;   (erc-interpret-controls line))
  )

(defun erc-grab-region (start end)
  "Save colors, faces, and text from the region in a recreatable format.
Converts all the IRC text properties in each line of the region
into controls and writes them to a separate buffer.  The resulting
text can be used directly as a script to generate this text again."
  (interactive "r")
  (setq erc-active-buffer (current-buffer))
  (save-excursion
    (let* ((cb (current-buffer))
	   (buf (generate-new-buffer erc-grab-buffer-name))
	   (region (buffer-substring start end))
	   (lines (erc-split-multiline-safe region)))
      (set-buffer buf)
      (dolist (line lines)
	(insert (concat (erc-decode-controls line) "\n")))
      (set-buffer cb)
      (switch-to-buffer-other-window buf))))

(defun erc-display-prompt (&optional buffer pos)
  "Display prompt in BUFFER at position POS.
Display ERC prompt (defined by `erc-prompt' variable) in BUFFER (if NIL or not
provided - current buffer) using `erc-prompt-face' at the POS (if not NIL) or
current position."
  (let ((ob (current-buffer))
	(pnt nil))
    (if (and buffer (bufferp buffer)) (set-buffer buffer))
    (setq pnt (point))
    ;; convert pos from mark-or-point to point
    (when pos
      (goto-char pos)
      (setq pos (point))
      (goto-char pnt))
    (when (> (length (erc-prompt)) 0)
      (let ((s (concat (erc-prompt) " ")))
	(erc-put-text-property 0 (length (erc-prompt)) 'face 'erc-prompt-face s)
	(erc-put-text-property (length (erc-prompt)) (length s)
			       'face 'erc-input-face s)
	(erc-put-text-property (length (erc-prompt)) (length s)
			       'read-only t s)
	(erc-put-text-property (length (erc-prompt)) (length s)
			       'rear-nonsticky t s)
	(when erc-prompt-interactive-input
	  (erc-put-text-property (length (erc-prompt)) (length s)
				 'local-map (erc-interactive-input-map) s)
	  (erc-put-text-property (length (erc-prompt)) (length s)
				 'front-sticky '(local-map) s))
	(if pos
	    (progn
	      (goto-char pos)
	      (insert s)
	      (if (>= pnt pos) (setq pnt (+ pnt (length s))))
	      (goto-char pnt))
	  (insert s))))
    (set-buffer ob)))

;; interactive operations

(defun erc-interactive-input-map ()
  "FIXME: add a docstring"
  (let ((lm (make-sparse-keymap)))
    (loop for c from 32 to 127
	  do (define-key lm (vector c) 'erc-input-message))
;    (define-key lm [?\C-i] 'erc-complete)
    lm))

(defun erc-input-message ()
  "FIXME: add a docstring."
  (interactive)
  (let ((minibuffer-allow-text-properties t)
	;; This doesnt help, seems as hippie-expand insists on the point
	;; of the buffer ???
;	(channel-members channel-members)
	(read-map minibuffer-local-map))
    (define-key read-map [?\C-i] 'erc-complete)
    (insert (read-from-minibuffer "Message: " (string last-command-char) read-map))
    (erc-send-current-line)))

(defvar erc-action-history-list ()
  "History list for interactive action input.")

(defun erc-input-action ()
  "Interactively input a user action and send it to IRC."
  (interactive "")
  (setq erc-active-buffer (current-buffer))
  (let ((action (read-from-minibuffer
		 "Action: " nil nil nil erc-action-history-list)))
    (if (not (string-match "^\\s-*$" action))
	(erc-send-action (erc-default-target) action))))

(defun erc-join-channel (channel)
  "Prompt for a channel name to join.
If POINT is at beginning of a channel name, use that as default."
  (interactive
   (list
    (let ((chnl (if (looking-at "\\([&#+!][^ ]+\\)") (match-string 1) ""))
	  (table (when (and (boundp 'erc-process) (processp erc-process))
		   (set-buffer (process-buffer erc-process))
		   channel-list)))
      (completing-read "Join channel: " table nil nil nil nil chnl))))
  (erc-cmd-JOIN channel))

(defun erc-part-from-channel (reason)
  "Part from the current channel and prompt for a REASON."
  (interactive
   (list
    (if (and (boundp 'reason) (stringp reason) (not (string= reason "")))
	reason
      (read-from-minibuffer (concat "Leave " (erc-default-target) ", Reason? ") (cons "No reason" 0)))))
  (erc-cmd-PART (concat (erc-default-target)" " reason)))

(defun erc-set-topic (topic)
  "Prompt for a TOPIC for the current channel."
  (interactive
   (list
    (read-from-minibuffer (concat "Set topic of "
				  (erc-default-target)
				  ": ")
			  (when (boundp 'channel-topic)
			    (cons channel-topic 0)))))
  (let ((topic-list (split-string topic ""))) ; strip off the topic setter
    (erc-cmd-TOPIC (car topic-list))))

(defun erc-nickserv-identify-autodetect (string)
  "Detect NickServ requesting identification."
  (let (promptp)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      ;; Refine regexp for other NickServ using networks if necessary please
      (setq promptp (re-search-forward "-NickServ-\\s-If\\s-this\\s-is\\s-your\\s-nickname,\\s-type\\s-/msg\\s-NickServ\\s-IDENTIFY\\s-<password>" nil t)))
    (and promptp (call-interactively 'erc-nickserv-identify))))

(defun erc-nickserv-identify (&optional password)
  "Send a \"identify <PASSWORD>\" to NickServ.
When called interactively, read the password using `read-passwd'."
  (interactive (list (read-passwd "Password: ")))
  (let ((erc-auto-discard-away nil))
    (erc-message "PRIVMSG"
		 (concat "NickServ IDENTIFY " password))))

;; Nick Completion

(autoload 'erc-complete "erc-complete" "Provide nick completion." t)

;; Track hidden channel buffers for new messages

(autoload 'erc-track-modified-channels-mode "erc-track"
  "Track modified channel buffers." t)

;; Movement of point

(defun erc-bol ()
  "Place point at the beginning of the line just after the `erc-prompt'."
  (interactive)
  (beginning-of-line)
  (when (and (looking-at (concat "^" (regexp-quote (erc-prompt))))
	     (<= (match-end 0) (save-excursion (end-of-line) (point))))
    (goto-char (match-end 0))
    (if (looking-at " ")
	(forward-char 1))
    t))

(defun erc-kill-input ()
  "Kill current input line using `erc-bol' followed by `kill-line'."
  (interactive)
  (and (erc-bol) (kill-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                        IRC SERVER INPUT HANDLING
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; New Input parsing

; Stolen from ZenIRC. I just wanna test this code, so here is
; experiment area.

(defcustom erc-default-server-hook '(erc-debug-missing-hooks
				     erc-default-server-handler)
  "*Default for server messages which aren't covered by `erc-server-hooks'."
  :group 'erc-server-hooks
  :type 'hook)

(defun erc-default-server-handler (proc parsed)
  (erc-display-message
   parsed 'notice (process-buffer proc)
   (mapconcat
    'identity
    (let (res)
      (mapc (lambda (x)
	      (if (stringp x)
		  (setq res (append res (list x)))))
	    parsed)
      res)
    " ")))

(defun erc-debug-missing-hooks (proc parsed)
  (nconc erc-server-vectors (list parsed))
  nil)

(defun erc-parse-line-from-server (proc string)
  "Parse and act upon a complete line from a IRC server.
PROC is the process (connection) from which STRING was received.
PROCs process-buffer is current-buffer when this function is called."
  (save-match-data
    (let ((posn (if (eq (aref string 0) ?:)
		    (string-match " " string)
		  0))
	  (msg (make-vector 32 nil))
	  (n 2))
      (aset msg 1 (if (eq posn 0)
		      erc-session-server
		    (substring string 1 posn)))

      (aset msg 0 (let* ((bposn (string-match "[^ ]" string posn))
			 (eposn (string-match " " string bposn)))
		    (setq posn (and eposn
				    (string-match "[^ ]" string eposn)))
		    (substring string bposn eposn)))

      (while (and posn
		  (not (eq (aref string posn) ?:)))
	(aset msg n (let* ((bposn posn)
			   (eposn (string-match " " string bposn)))
		      (setq posn (and eposn
				      (string-match "[^ ]" string eposn)))
		      (substring string bposn eposn)))
	(setq n (1+ n)))
      (if posn
	  (aset msg n (substring string (1+ posn))))
      (let* ((hook-name (concat "erc-server-" (aref msg 0) "-hook"))
	     (hook (intern-soft hook-name)))
	(if (and hook (symbol-value hook))
	    (progn
	      (run-hook-with-args-until-success hook proc msg)
	      nil)
	  (run-hook-with-args-until-success 'erc-default-server-hook proc msg)
	  nil)
	(with-current-buffer (erc-server-buffer)
	  (run-hook-with-args 'erc-timer-hook (erc-current-time)))))))

(defvar erc-server-vectors '(["msgtype" "sender" "to" "arg1" "arg2" "arg3" "..."]))
;(make-variable-buffer-local 'erc-server-vectors)

(defcustom erc-auto-query nil
  "If non-nil, create a query buffer everytime you receive a private message.
in case a query buffer doesn't already exist."
  :group 'erc
  :type 'boolean)

(defcustom erc-query-on-unjoined-chan-privmsg t
  "If non-nil create query buffer on receiving any PRIVMSG at all.
This includes PRIVMSGs directed to channels.  If you are using an IRC
bouncer, such as dircproxy, to keep a log of channels when you are
disconnected, you should set this option to t."
  :group 'erc
  :type 'boolean)

(defcustom erc-minibuffer-notice nil
  "If non-nil, print notices in the minibuffer.
Only happens when the session buffer isn't visible."
  :group 'erc
  :type 'boolean)

(defcustom erc-minibuffer-privmsg t
  "If non-nil, print private messages in the minibuffer.
Only happens when the session buffer isn't visible."
  :group 'erc
  :type 'boolean)

(defun erc-wash-quit-reason (reason nick login host)
  "Remove duplicate text from quit REASON.
Specifically in relation to NICK (user@host) information.  Returns REASON
unmodified if nothing can be removed.
FIXME: does user@host really mean login@host in this docstring?"
  (or (when (string-match (concat "^\\(Read error\\) to "
				  nick "\\[" host "\\]: "
				  "\\(.+\\)$") reason)
	(concat (match-string 1 reason) ": " (match-string 2 reason)))
      (when (string-match (concat "^\\(Ping timeout\\) for "
				  nick "\\[" host "\\]$") reason)
	(match-string 1 reason))
      reason))

;;; Server messages


(defgroup erc-server-hooks nil
  "Server hooks."
  :group 'erc-hooks)

(defmacro erc-server-hook-list (list)
  "FIXME: add a docstring."
  `(progn
     ,@(mapcar
	#'(lambda (pair)
	    `(defcustom
	       ,(intern
		 (concat "erc-server-"
			 (let ((name (car pair)))
			   (cond
			    ((symbolp name) (symbol-name name))
			    ((numberp name)
			     (setq name (number-to-string name))
			     (cond ((< (length name) 2)
				    (setq name (concat "00" name))))
			     name)))
			 "-hook"))
	       (quote ,(cdr pair))
	       ,(concat (if (numberp (car pair))
			    (concat "Numeric server reply #"
				    (number-to-string (car pair)) "\n")
			  (concat (symbol-name (car pair))
				  " server reply.\n"))
			"Arguments are (PARSED PROC).
PARSED is a vector containing the server message fields as strings and
PROC is the process object of the server that message came from.")
	       :group (quote erc-server-hooks)
	       :type (quote hook)))
	list)))

(erc-server-hook-list
 ((ERROR erc-server-ERROR)
  (INVITE erc-server-INVITE)
  (JOIN erc-server-JOIN)
  (KICK erc-server-KICK)
  (MODE erc-server-MODE)
  (NICK erc-server-NICK)
  (NOTICE erc-server-PRIVMSG-or-NOTICE)
  (PART erc-server-PART)
  (PING erc-server-PING)
  (PRIVMSG erc-auto-query erc-server-PRIVMSG-or-NOTICE)
  (QUIT erc-server-QUIT)
  (TOPIC erc-server-TOPIC)
  (004 erc-server-004)
  (221 erc-server-221)
  (301 erc-server-301)
  (303 erc-server-303)
  (305 erc-server-305-or-306)
  (306 erc-server-305-or-306)
  (311 erc-server-311-or-314)
  (312 erc-server-312)
  (314 erc-server-311-or-314)
  (317 erc-server-317)
  (319 erc-server-319)
  (320 erc-server-320)
  (321 erc-server-321)
  (322 erc-server-322)
  (324 erc-server-324)
  (329 erc-server-329)
  (331 erc-server-331)
  (332 erc-server-332)
  (333 erc-server-333)
  (352 erc-server-352)
  (353 erc-server-353)
  (379 erc-server-379)
  (401 erc-server-401)
  (403 erc-server-403)
  (405 erc-server-405)
  (421 erc-server-421)
  (433 erc-server-433)
  (461 erc-server-461)
  (474 erc-server-474)
  (481 erc-server-481)
  (501 erc-server-501)
))

;;; ERROR

(defun erc-server-ERROR (proc parsed)
  "FIXME: add a docstring."
(erc-update-mode-line))

;;; INVITE

(defun erc-server-INVITE (proc parsed)
  "FIXME: add a docstring."
  (let* ((target (aref parsed 2))
	 (chnl (aref parsed 3))
	 (sndr (erc-parse-user (aref parsed 1)))
	 (nick (nth 0 sndr))
	 (login (nth 1 sndr))
	 (host (nth 2 sndr)))
    (setq invitation chnl)
    (when (string= target (erc-current-nick))
      (erc-display-message
       parsed 'notice 'active
       'INVITE ?n nick ?u login ?h host ?c chnl))))

;;; JOIN

(defun erc-server-JOIN (proc parsed)
  "FIXME: add a docstring"
  (let* ((chnl (aref parsed 2))
	 (sndr (erc-parse-user (aref parsed 1)))
	 (nick (nth 0 sndr))
	 (login (nth 1 sndr))
	 (host (nth 2 sndr))
	 (buffer nil))
    ;; strip the stupid combined JOIN facility (IRC 2.9)
    (if (string-match "^\\(.*\\)?\^g.*$" chnl)
	(setq chnl (match-string 1 chnl)))
    (let* ((ob (current-buffer))
	   (info-buf nil)
	   (str (cond
		 ;; If I have joined a channel
		 ((string= (erc-downcase nick) (erc-downcase (erc-current-nick)))
		  (setq buffer (erc erc-session-server erc-session-port
				    nick erc-session-user-full-name
				    nil nil
				    erc-default-recipients chnl erc-process))
		  (when buffer
		    (set-buffer buffer)
		    (erc-add-default-channel chnl)
		    ;; display the channel info buffer
		    (setq info-buf (erc-find-channel-info-buffer chnl))
		    (cond ((eq erc-join-info-buffer 'frame)
			   (switch-to-buffer-other-frame info-buf))
			  ((eq erc-join-info-buffer 'window)
			   (switch-to-buffer-other-window info-buf))
			  ((eq erc-join-info-buffer 'split)
			   (split-window-vertically)
			   (switch-to-buffer-other-window info-buf)))
		    ;; and return to the channel buffer...
		    ;; boy, how to put the focus back into the channel
		    ;; window now???
		    (set-buffer buffer))
		  (setq erc-last-channel-names nil)
		  (erc-refresh-channel-members chnl "")
		  (erc-update-mode-line)
		  (erc-make-notice
		   (erc-format-message 'JOIN-you ?c chnl)))
		 (t
		  (setq buffer (erc-get-buffer chnl proc))
		  (erc-make-notice
		   (erc-format-message 'JOIN ?n nick ?u login ?h host ?c chnl))))))
      (if buffer (set-buffer buffer))
      (erc-update-channel-member chnl nick nick t nil nil host login)
      (erc-update-channel-info-buffer chnl)
      ;; on join, we want to stay in the new channel buffer
      ;;(set-buffer ob)
      (erc-display-line str buffer))))

(defun erc-server-KICK (proc parsed)
  (let* ((ch (aref parsed 2))
	 (tgt (aref parsed 3))
	 (reason (erc-trim-string (aref parsed 4)))
	 (sndr (erc-parse-user (aref parsed 1)))
	 (nick (nth 0 sndr))
	 (login (nth 1 sndr))
	 (host (nth 2 sndr))
	 (buffer (erc-get-buffer ch proc)))
    (erc-remove-channel-member buffer tgt)
    (erc-update-channel-info-buffer ch)
    (erc-display-line
     (erc-make-notice
      (cond
       ((string= tgt (erc-current-nick))
	(erc-delete-default-channel ch buffer)
	(erc-update-mode-line buffer)
	(format "You have been kicked by %s (%s@%s) off channel %s: %s"
		nick login host ch reason))
       ((string= nick (erc-current-nick))
	(format "You have kicked %s off channel %s: %s" tgt ch reason))
       (t
	(format "%s (%s@%s) has kicked %s off channel %s: %s"
		nick login host tgt ch reason))))
     buffer)))

(defvar erc-server-KILL-hook nil)

;;; MODE

(defun erc-server-MODE (proc parsed)
  (let* ((tgt (aref parsed 2))
	 (mode (mapconcat 'identity (delete* nil (subseq parsed 3)) " "))
	 (sndr (erc-parse-user (aref parsed 1)))
	 (nick (nth 0 sndr))
	 (login (nth 1 sndr))
	 (host (nth 2 sndr)))
    (erc-log (format "MODE: %s -> %s: %s" nick tgt mode))
    ;; dirty hack
    (let ((ob (current-buffer))
	  (buf (cond ((erc-channel-p tgt)
		      (erc-get-buffer tgt proc))
		     (erc-active-buffer erc-active-buffer)
		     (t (erc-get-buffer tgt))))
	  (res (if (or (string= login "") (string= host ""))
		   (erc-make-notice
		    (format "%s has changed mode for %s to %s"
			    nick tgt mode))
		 (erc-make-notice
		  (erc-format-message 'MODE ?n nick ?u login ?h host ?t tgt ?m mode)))))
      (if buf (set-buffer buf))
      (erc-update-modes tgt mode nick host login)
      (set-buffer ob)
      (erc-display-line res buf))))

;;; NICK

(defun erc-server-NICK (proc parsed)
  (let* ((nn (aref parsed 2))
	 (sndr (erc-parse-user (aref parsed 1)))
	 (nick (nth 0 sndr))
	 (login (nth 1 sndr))
	 (host (nth 2 sndr))
	 (bufs (erc-buffer-list-with-nick nick proc)))
    (erc-log (format "NICK: %s -> %s" nick nn))
    (erc-update-member-all-channels nick nn nil nil nil host login)
    (erc-update-channel-info-buffers bufs)
    (cond
     ((string= nick (erc-current-nick))
      (erc-push-nick nn)
      (erc-update-mode-line)
      (erc-display-message
       parsed 'notice bufs (erc-format-message 'NICK-you ?n nick ?N nn)))

     (t
      (erc-handle-user-status-change 'nick (list nick login host) (list nn))
      (erc-display-message
       parsed 'notice bufs 'NICK ?n nick ?u login ?h host ?N nn)))))

;;; PART

(defun erc-server-PART (proc parsed)
  (let* ((chnl (aref parsed 2))
	 (reason (erc-trim-string (aref parsed 3)))
	 (sndr (erc-parse-user (aref parsed 1)))
	 (nick (nth 0 sndr))
	 (login (nth 1 sndr))
	 (host (nth 2 sndr))
	 (buffer (erc-get-buffer chnl proc)))
    (erc-remove-channel-member buffer nick)
    (erc-update-channel-info-buffer chnl)
    (erc-display-message
     parsed 'notice buffer 'PART ?n nick ?u login ?h host ?c chnl ?r reason)
    (when (string= nick (erc-current-nick))
      (when erc-save-buffer-on-part
	(erc-save-buffer-in-logs buffer))
      (erc-delete-default-channel chnl buffer)
      (erc-update-mode-line buffer)
      (when erc-kill-buffer-on-part
	(kill-buffer buffer)))))

;;; PING

(defun erc-server-PING (proc parsed)
  (let ((pinger (aref parsed 2)))
    (erc-log (format "PING: %s" pinger))
    ;; ping response to the server MUST be forced, or you can lose big
    (erc-send-command (format "PONG :%s" pinger) t)
    (when erc-paranoid
      (erc-display-message
       parsed nil nil
       (format "ERC: server ping (last: %s sec. ago)"
	       (erc-time-diff last-ping-time (erc-current-time)))
       (process-buffer proc)))
    (setq last-ping-time (erc-current-time))))

(defvar erc-server-PONG-hook nil)

;;; PRIVMSG

(defun erc-auto-query (proc parsed)
  "Put this on `erc-server-PRIVMSG-hook'."
  (let* ((nick (car (erc-parse-user (aref parsed 1))))
	 (target (aref parsed 2))
	 (msg (aref parsed 3))
	 (query  (if (not erc-query-on-unjoined-chan-privmsg)
		     nick
		   (if (string= (erc-downcase target)
				(erc-downcase (erc-current-nick)))
		       nick
		     target))))
    (and erc-auto-query
	 (or erc-query-on-unjoined-chan-privmsg
	     (string= target (erc-current-nick)))
	 (not (erc-get-buffer query proc))
	 (not (erc-is-message-ctcp-and-not-action-p msg))
	 (erc-cmd-QUERY query)
	 nil)))

;;; PRIVMSG and NOTICE

(defun erc-is-message-ctcp-p (message)
  "Check if MESSAGE is a CTCP message or not."
  (string-match "^\C-a\\([^\C-a]*\\)\C-a?$" message))

(defun erc-is-message-ctcp-and-not-action-p (message)
  "Check if MESSAGE is a CTCP message or not."
  (and (erc-is-message-ctcp-p message)
       (not (string-match "^\C-a\\ACTION.*\C-a$" message))))

(defun erc-format-privmessage (nick msg privp msgp)
  "Format a PRIVMSG in an insertible fashion."
  (let* ((mark-s (concat (erc-format-timestamp)
			 (if msgp (if privp "*" "<") "-")))
	 (mark-e (if msgp (if privp "*" ">") "-"))
	 (str	 (if away
		     (format "%s%s%s %s %s"
			     mark-s nick mark-e
			     msg
			     (format-time-string erc-away-timestamp-format
						 (current-time)))
		   (format "%s%s%s %s" mark-s nick mark-e msg))))
    (erc-put-text-property (1- (length mark-s)) (length str) 'face
			   (if privp
			       'erc-direct-msg-face
			     'erc-default-face) str) str))

(defcustom erc-format-nick-function 'erc-format-nick
  "*Function to format a nickname for message display."
  :group 'erc
  :type 'function)

(defun erc-format-nick (&optional nick op voice host user full-name info)
  "Standard nickname formatting function. Only returns the value of NICK."
  nick)

(defun erc-format-@nick (&optional nick op voice host user full-name info)
  "Format a nickname such that @ or + are prefix for the NICK
if OP or VOICE are t respectively."
  (when nick
    (concat (if voice "+" "")
	    (if op "@" "")
	    nick)))

(defun erc-server-PRIVMSG-or-NOTICE (proc parsed)
  (let ((sspec (aref parsed 1))
	(cmd (aref parsed 0))
	(tgt (aref parsed 2))
	(msg (aref parsed 3)))
    (if (erc-ignored-user-p sspec)
	(message "ignored %s from %s to %s" cmd sspec tgt)
      (let* ((sndr (erc-parse-user sspec))
	     (nick (nth 0 sndr))
	     (login (nth 1 sndr))
	     (host (nth 2 sndr))
	     (msgp (string= cmd "PRIVMSG"))
	     (noticep (string= cmd "NOTICE"))
	     ;; S.B. downcase *both* tgt and current nick
	     (privp (string= (erc-downcase tgt) (erc-downcase (erc-current-nick))))
	     s buffer
	     fnick)
	(setq buffer (erc-get-buffer (if privp nick tgt) proc))
	(when buffer
	  (with-current-buffer buffer
	    ;; update the chat partner info.  Add to the list if private
	    ;; message.  We will accumulate private identities indefinitely
	    ;; at this point.
	    (if (erc-update-channel-member (if privp nick tgt) nick nick
					   privp nil nil host login)
		(erc-update-channel-info-buffer (if privp nick tgt)))
	    (setq fnick
		  (apply erc-format-nick-function
			 (assoc nick channel-members)))))
	(cond
	 ((erc-is-message-ctcp-p msg)
	  (setq s (if msgp
		      (erc-process-ctcp-query proc parsed nick login host)
		    (erc-process-ctcp-reply proc parsed nick login host
					    (match-string 1 msg)))))
	 (t
	  (setcar last-peers nick) ; is last-peers useful in _any_ way?
	  (setq s (erc-format-privmessage (or fnick nick) msg privp msgp))))
	(when s
	  (erc-display-message parsed nil buffer s))))))

;;; QUIT

(defun erc-server-QUIT (proc parsed)
  (let* ((reason (aref parsed 2))
	 (sndr (erc-parse-user (aref parsed 1)))
	 (nick (nth 0 sndr))
	 (login (nth 1 sndr))
	 (host (nth 2 sndr))
	 (bufs (erc-buffer-list-with-nick nick proc)))
    (erc-remove-member-all-channels nick bufs)
    (erc-update-channel-info-buffers bufs)
    (setq reason (erc-wash-quit-reason reason nick login host))
    (erc-display-message
     parsed 'notice bufs
     'QUIT ?n nick ?u login ?h host ?r reason)))

(defun erc-server-TOPIC (proc parsed)
  (let* ((sspec (aref parsed 1))
	 (ch (aref parsed 2))
	 (topic (erc-trim-string (aref parsed 3)))
	 (sndr (erc-parse-user sspec))
	 (nick (nth 0 sndr))
	 (login (nth 1 sndr))
	 (host (nth 2 sndr))
	 (time (format-time-string "%T %m/%d/%y" (current-time))))
    (erc-update-channel-member ch nick nick nil nil nil host login)
    (erc-update-channel-topic ch (format "%s\C-c (%s, %s)" topic nick time))
    (erc-update-channel-info-buffer ch)
    (erc-display-message
     parsed 'notice (erc-get-buffer ch proc)
     'TOPIC ?n nick ?u login ?h host ?c ch ?T topic)))

(defvar erc-server-WALLOPS-hook nil)
(defvar erc-server-001-hook '(erc-server-MOTD))
(defvar erc-server-002-hook '(erc-server-MOTD))
(defvar erc-server-003-hook '(erc-server-MOTD))

(defun erc-server-004 (proc parsed)
  (let ((server-name (aref parsed 3))
	(server-version (aref parsed 4)))
    (set (make-local-variable 'erc-server-version) server-version)
    (set (make-local-variable 'erc-announced-server-name) server-name)
    (erc-update-mode-line-buffer (process-buffer proc))
    (erc-display-message
     parsed 'notice nil
     's004 ?s server-name ?v server-version ?U (aref parsed 5) ?C (aref parsed 6))
    (process-buffer proc)))

(defvar erc-server-200-hook nil)
(defvar erc-server-201-hook nil)
(defvar erc-server-202-hook nil)
(defvar erc-server-203-hook nil)
(defvar erc-server-204-hook nil)
(defvar erc-server-205-hook nil)
(defvar erc-server-206-hook nil)
(defvar erc-server-208-hook nil)
(defvar erc-server-209-hook nil)
(defvar erc-server-211-hook nil)
(defvar erc-server-212-hook nil)
(defvar erc-server-213-hook nil)
(defvar erc-server-214-hook nil)
(defvar erc-server-215-hook nil)
(defvar erc-server-216-hook nil)
(defvar erc-server-217-hook nil)
(defvar erc-server-218-hook nil)
(defvar erc-server-219-hook nil)

(defun erc-server-221 (proc parsed)
  (let ((modes (mapconcat 'identity (delete* nil (subseq parsed 3)) " "))
	(nick (aref parsed 2)))
    (erc-set-modes nick modes)
    (erc-display-message
     parsed 'notice 'active
     's221 ?n nick ?m modes)))

(defvar erc-server-241-hook nil)
(defvar erc-server-242-hook nil)
(defvar erc-server-243-hook nil)
(defvar erc-server-244-hook nil)
(defvar erc-server-249-hook nil)
(defvar erc-server-251-hook '(erc-server-MOTD))
(defvar erc-server-252-hook nil)
(defvar erc-server-253-hook nil)
(defvar erc-server-254-hook nil)
(defvar erc-server-255-hook '(erc-server-MOTD))
(defvar erc-server-256-hook nil)
(defvar erc-server-257-hook nil)
(defvar erc-server-258-hook nil)
(defvar erc-server-259-hook nil)
(defvar erc-server-261-hook nil)
(defvar erc-server-262-hook nil)

(defun erc-server-301 (proc parsed)
  "AWAY notice"
  (let ((nick (aref parsed 3))
	(msg (aref parsed 4)))
    (erc-display-message
     parsed 'notice 'active
      's301 ?n nick ?r msg)))

(defvar erc-server-302-hook nil)

(defun erc-server-303 (proc parsed)
  (let ((str (aref parsed 3)))
    (erc-display-message
     parsed 'notice (process-buffer proc)
     (concat "Is online: " str))))


;;; AWAY message

(defun erc-server-305-or-306 (proc parsed)
  (erc-process-away proc (string= (aref parsed 0) "306"))
  (erc-display-message
   parsed 'notice 'active
   (aref parsed 3)))

(defun erc-server-311-or-314 (proc parsed)
  "WHOIS/WAS notices"
  (let ((nick (aref parsed 3))
	(user (aref parsed 4))
	(host (aref parsed 5))
	(fname (aref parsed 7)))
    (erc-update-member-all-channels nick nick nil nil nil host user fname)
    (erc-update-channel-info-buffers)
    (erc-display-message
     parsed 'notice 'active
      (format "%s %s %s (%s@%s)"
	      nick
	      (if (string= (aref parsed 0) "311")
		  "is"
		"was")
	      fname user host))))

(defun erc-server-312 (proc parsed)
  (let ((nick (aref parsed 3))
	(saddr (aref parsed 4))
	(server (aref parsed 5)))
    (erc-display-message
     parsed 'notice 'active
      (format "%s is/was on server %s (%s)"
	      nick saddr server))))

(defvar erc-server-313-hook nil)
(defvar erc-server-315-hook nil)

(defun erc-server-317 (proc parsed)
  ;; IDLE notice
  (let* ((nick (aref parsed 3))
	 (nsec (string-to-number (aref parsed 4)))
	 (time-str (aref parsed 5))
	 (time (when time-str
		 (format-time-string "%T %m/%d/%y"
				     (erc-string-to-emacs-time time-str)))))
    (erc-update-member-all-channels nick nick nil nil nil nil nil nil
				      (if time
					  (format "on since %s" time) nil))
    (erc-update-channel-info-buffers)
    (erc-display-message
     parsed 'notice 'active
      (format "%s is idle for %s%s"
	      nick
	      (erc-sec-to-time nsec)
	      (if time (format ", on since %s" time) "")))))

(defvar erc-server-318-hook nil)

(defun erc-server-319 (proc parsed)
  (let ((nick (aref parsed 3))
	(chnls (aref parsed 4)))
    (erc-display-message
     parsed 'notice 'active
      (format "%s is on channel(s): %s" nick chnls))))

(defun erc-server-320 (proc parsed)
  (let ((nick (aref parsed 3))
	(text (aref parsed 4)))
    (erc-display-message
     parsed 'notice 'active
      (format "%s %s" nick text))))

(defun erc-server-321 (proc parsed)
  ;; LIST header
  (setq channel-list nil) ; FIXME. rename-me to erc-channel-list later
  (erc-display-message
   parsed 'notice 'active
    (concat (aref parsed 3) "  " (aref parsed 4))))

(defun erc-server-322 (proc parsed)
  ;; LIST notice
  (let ((chnl (aref parsed 3))
	(nv (aref parsed 4))
	(topic (aref parsed 5)))
    (add-to-list 'channel-list (list chnl))
    (erc-update-channel-topic chnl topic)
    (erc-update-channel-info-buffer chnl)
    (erc-display-message
     parsed 'notice 'active
      (if (string= topic "")
	  (format "%s [%s]" chnl nv)
	(format "%s [%s]: %s" chnl nv topic)))))

(defvar erc-server-323-hook nil)

(defun erc-server-324 (proc parsed)
  ;; channel or nick modes
  (let ((chnl (aref parsed 3))
	(modes (mapconcat 'identity (delete* nil (subseq parsed 4)) " ")))
    (erc-set-modes chnl modes)
    (erc-display-message
     parsed 'notice (erc-get-buffer chnl proc) (format "%s modes: %s" chnl modes))))

(defun erc-server-329 (proc parsed)
  ;; Channel living time
  (let ((chnl (aref parsed 3))
	(time (erc-string-to-emacs-time (aref parsed 4))))
    (erc-display-message
     parsed 'notice 'active
      (format "%s was created on %s"
	      chnl
	      (format-time-string "%A %Y-%m-%d %X" time)))))

(defun erc-server-331 (proc parsed)
  ;; Channel topic
  (let ((chnl (aref parsed 3))
	(topic (aref parsed 4)))
    (erc-display-message
     parsed 'notice (erc-get-buffer chnl proc)
      (format "Topic on %s: %s" chnl topic))))

(defun erc-server-332 (proc parsed)
  ;; TOPIC notice
  (let ((chnl (aref parsed 3))
	(topic (aref parsed 4)))
    (erc-update-channel-topic chnl topic)
    (erc-update-channel-info-buffer chnl)
    (erc-display-message
     parsed 'notice 'active
      (format "%s topic: %s" chnl topic))))

(defun erc-server-333 (proc parsed)
  ;; who set the topic and when
  (let ((chnl (aref parsed 3))
	(nick (aref parsed 4))
	(time (format-time-string "%T %m/%d/%y"
				  (erc-string-to-emacs-time (aref parsed 5)))))
    (erc-update-channel-topic chnl (format "\C-c (%s, %s)" nick time)
			      'append)
    (erc-update-channel-info-buffer chnl)
    (erc-display-message
     parsed 'notice 'active
      (format "%s: topic set by %s, %s"
	      chnl nick time))))

(defvar erc-server-341-hook nil)
(defvar erc-server-342-hook nil)
(defvar erc-server-351-hook nil)

(defun erc-server-352 (proc parsed)
  ;; WHO notice
  (let ((chnl (aref parsed 3))
	(user (aref parsed 4))
	(host (aref parsed 5))
	(server (aref parsed 6))
	(nick (aref parsed 7))
	(flag (aref parsed 8))
	(name (aref parsed 9))      ; This is not quite right, but ok
	hopcount)
    (when (string-match "\\(^[0-9]+ \\)\\(.*\\)$" name)
      (setq hopcount (match-string 1 name))
      (setq name (match-string 2 name)))
    (erc-update-channel-member chnl nick nick nil nil nil host user name nil)
    (erc-display-message
     parsed 'notice 'active
      (format "%-11s %-10s %-4s %s@%s (%s)" chnl nick flag user host name))))

(defun erc-server-353 (proc parsed)
  (let ((chnl (aref parsed 4))
	(users (aref parsed 5)))
    (erc-refresh-channel-members chnl users t)
    (erc-update-channel-info-buffer chnl)
    (erc-display-message
     parsed 'notice 'active
      (format "Users on %s: %s" chnl users))))

(defvar erc-server-364-hook nil)
(defvar erc-server-365-hook nil)
(defvar erc-server-366-hook nil)
(defvar erc-server-367-hook nil)
(defvar erc-server-368-hook nil)
(defvar erc-server-369-hook nil)

;;; MOTD numreplies

(defun erc-handle-login ()
  (when (not erc-logged-in)
    (setq erc-logged-in t)
    (message "Logging in as \'%s\'... done" (erc-current-nick))
    ;; execute a startup script
    (let ((f (erc-select-startup-file)))
      (when f
	(erc-load-script f)))))

(defun erc-server-MOTD (proc parsed)
  (erc-handle-login)
  (erc-display-message
   parsed 'notice 'active (aref parsed 3)))

(defvar erc-server-371-hook '(erc-server-MOTD))
(defvar erc-server-372-hook '(erc-server-MOTD))
(defvar erc-server-374-hook '(erc-server-MOTD))
(defvar erc-server-375-hook '(erc-server-MOTD))
(defvar erc-server-376-hook '(erc-server-MOTD))

(defun erc-server-379 (proc parsed)
  "Forwarding to another channel"
  (let ((from (aref parsed 3))
	(to (aref parsed 4)))
    (erc-display-message
     parsed 'notice 'active
      's379 ?c from ?f to)))

(defvar erc-server-381-hook nil)
(defvar erc-server-382-hook nil)
(defvar erc-server-391-hook nil)
(defvar erc-server-392-hook nil)
(defvar erc-server-393-hook nil)
(defvar erc-server-394-hook nil)
(defvar erc-server-395-hook nil)

(defun erc-display-error-notice (parsed string)
  (erc-display-message
   parsed '(notice error) 'active string))

(defun erc-server-401 (proc parsed)
  "No suck nick/channel"
  (let ((target (aref parsed 3))
	(text (aref parsed 4)))
    (erc-display-error-notice
     parsed
     (format "%s: %s" target text))))

(defvar erc-server-402-hook nil)

(defun erc-server-403 (proc parsed)
  "That channel doesn't exist"
  (let ((target (aref parsed 3))
	(text (aref parsed 4)))
    (erc-display-error-notice
     parsed
     (format "%s: %s" target text))))

(defvar erc-server-404-hook nil)

(defun erc-server-405 (proc parsed)
  "You can't join that many channels"
  (let ((target (aref parsed 3))
	(text (aref parsed 4)))
    (erc-display-error-notice
     parsed
     (format "%s: %s" target text))))

(defvar erc-server-406-hook nil)
(defvar erc-server-407-hook nil)
(defvar erc-server-409-hook nil)
(defvar erc-server-411-hook nil)
(defvar erc-server-412-hook nil)
(defvar erc-server-413-hook nil)
(defvar erc-server-414-hook nil)
(defvar erc-server-415-hook nil)

(defun erc-server-421 (proc parsed)
  ;; Unknown command
  (let ((command (aref parsed 3)))
    (erc-display-error-notice
     parsed
     (format "Unknown command \'%s\'" command))))

(defvar erc-server-422-hook nil)
(defvar erc-server-423-hook nil)
(defvar erc-server-424-hook nil)
(defvar erc-server-431-hook nil)
(defvar erc-server-432-hook nil)

(defun erc-server-433 (proc parsed)
  ;; login-time 'nickname in use' message
  (let* ((nick (aref parsed 3))
	 (newnick (nth 1 erc-default-nicks)))
    (setq bad-nick t)
    ;; try to use a different nick
    (if erc-default-nicks (setq erc-default-nicks (cdr erc-default-nicks)))
    (if (not newnick)
	(setq newnick (concat nick erc-nick-uniquifier)))
    (erc-cmd-NICK newnick t)
    (erc-display-error-notice
     parsed
     (format "Nickname \'%s\' is already in use, trying %s"
	       nick newnick))))

(defvar erc-server-436-hook nil)
(defvar erc-server-437-hook nil)
(defvar erc-server-441-hook nil)
(defvar erc-server-442-hook nil)
(defvar erc-server-443-hook nil)
(defvar erc-server-444-hook nil)
(defvar erc-server-445-hook nil)
(defvar erc-server-446-hook nil)
(defvar erc-server-451-hook nil)

(defun erc-server-461 (proc parsed)
  (let ((cmd (aref parsed 3))
	(msg (aref parsed 4)))
    (erc-display-error-notice
     parsed
     (format "%s: %s" cmd msg))))

(defvar erc-server-462-hook nil)
(defvar erc-server-463-hook nil)
(defvar erc-server-464-hook nil)
(defvar erc-server-465-hook nil)
(defvar erc-server-467-hook nil)
(defvar erc-server-471-hook nil)
(defvar erc-server-472-hook nil)
(defvar erc-server-473-hook nil)

(defun erc-server-474 (proc parsed)
  (let ((channel (aref parsed 3))
	(msg (aref parsed 4)))
    (erc-display-error-notice
     parsed
     (format "%s: %s" channel msg))))

(defvar erc-server-475-hook nil)
(defvar erc-server-477-hook nil)

(defun erc-server-481 (proc parsed)
  (let ((msg (aref parsed 3)))
    (erc-display-error-notice
     parsed
     msg)))

(defvar erc-server-482-hook nil)
(defvar erc-server-483-hook nil)
(defvar erc-server-491-hook nil)

(defun erc-server-501 (proc parsed)
  (erc-display-error-notice
   parsed
   (aref parsed 3)))

(defvar erc-server-502-hook nil)

(defun erc-process-ctcp-query (proc parsed nick login host)
  (let ((queries (delete "" (split-string (aref parsed 3) "\C-a"))))
    (if (> (length queries) 4)
	(erc-display-message
	 parsed (list 'notice 'error) (process-buffer proc)
	 "Too many CTCP queries in single message. Ignoring")
      (if (= 0 (length queries))
	  (erc-display-message
	   parsed (list 'notice 'error) (process-buffer proc)
	   (format "Illegal empty CTCP query received from %s. Ignoring." nick))
	(while queries
	  (let* ((type (car (split-string (car queries))))
		 (hook (intern-soft (concat "erc-ctcp-query-" type "-hook"))))
	    (if (and hook (boundp hook)
		     erc-paranoid
		     (not (member type '("ACTION" "PAGE" "SOUND"))))
		(erc-display-message
		 parsed 'error nil
		 (format "==> CTCP request from %s: %s\n"
			 (aref parsed 1) (car queries))))
	    (if (boundp hook)
		(run-hook-with-args-until-success hook proc nick login host
						  (aref parsed 2) (car queries))
	      (erc-display-message
	       parsed (list 'notice 'error) (process-buffer proc)
	       "Undefined CTCP query received. Silently ignored")))
	  (setq queries (cdr queries)))))))

(defvar erc-ctcp-query-ACTION-hook '(erc-ctcp-query-ACTION))

(defun erc-ctcp-query-ACTION (proc nick login host to msg)
  (when (string-match "^ACTION\\s-\\(.*\\)\\s-*$" msg)
    (let ((s (erc-propertize (match-string 1 msg) 'face 'erc-action-face))
	  (buf (or (erc-get-buffer to proc)
		   (erc-get-buffer (erc-downcase nick) proc)
		   (process-buffer proc))))
      (erc-display-message
       nil nil buf
       (concat (erc-format-timestamp)
	       (erc-format-message
		'ACTION
		?n nick ?u login ?h host ?a s)))))
  nil)

(defvar erc-ctcp-query-CLIENTINFO-hook '(erc-ctcp-query-CLIENTINFO))

(defun erc-ctcp-query-CLIENTINFO (proc nick login host to msg)
  (when (string-match "^CLIENTINFO\\(\\s-*\\|\\s-+.*\\)$" msg)
    (let ((s (erc-client-info (erc-trim-string (match-string 1 msg)))))
      (if (not erc-disable-ctcp-replies)
	  (erc-send-ctcp-notice nick (format "CLIENTINFO %s" s)))))
  nil)

(defvar erc-ctcp-query-ECHO-hook '(erc-ctcp-query-ECHO))
(defun erc-ctcp-query-ECHO (proc nick login host to msg)
  (when (string-match "^ECHO\\s-+\\(.*\\)\\s-*$" msg)
    (let ((s (match-string 1 msg)))
      (when (not erc-disable-ctcp-replies)
	(erc-send-ctcp-notice nick (format "ECHO %s" s))
	(erc-put-text-property 0 (length s) 'face 'erc-action-face s)
	(erc-display-line
	 (concat nick " [ECHO]> " s)))))
  nil)

(defvar erc-ctcp-query-FINGER-hook '(erc-ctcp-query-FINGER))
(defun erc-ctcp-query-FINGER (proc nick login host to msg)
  (if (not erc-disable-ctcp-replies)
      (let ((s (if erc-anonymous-login
		   (format "FINGER I'm %s." (erc-current-nick))
		 (format "FINGER %s (%s@%s)."
			 (user-full-name)
			 (user-login-name)
			 (system-name))))
	    (ns (erc-time-diff last-sent-time (erc-current-time))))
	(if (> ns 0)
	    (setq s (concat s " Idle for " (erc-sec-to-time ns))))
	(erc-send-ctcp-notice nick s)))
  nil)

(defvar erc-ctcp-query-PING-hook '(erc-ctcp-query-PING))
(defun erc-ctcp-query-PING (proc nick login host to msg)
  (when (string-match "^PING\\s-+\\([0-9]+\\)" msg)
    (if (not erc-disable-ctcp-replies)
	(let ((num (match-string 1 msg)))
	  (erc-send-ctcp-notice nick (format "PING %s" num)))))
  nil)

(defvar erc-ctcp-query-TIME-hook '(erc-ctcp-query-TIME))
(defun erc-ctcp-query-TIME (proc nick login host to msg)
  (if (not erc-disable-ctcp-replies)
      (erc-send-ctcp-notice nick (format "TIME %s" (current-time-string))))
  nil)

(defvar erc-ctcp-query-USERINFO-hook '(erc-ctcp-query-USERINFO))
(defun erc-ctcp-query-USERINFO (proc nick login host to msg)
  (if (not erc-disable-ctcp-replies)
      (erc-send-ctcp-notice nick (format "USERINFO %s" erc-user-information)))
  nil)

(defvar erc-ctcp-query-VERSION-hook '(erc-ctcp-query-VERSION))
(defun erc-ctcp-query-VERSION (proc nick login host to msg)
  (if (not erc-disable-ctcp-replies)
      (erc-send-ctcp-notice
	 nick (format "VERSION \C-bERC\C-b v%s - an IRC client for emacs (\C-b%s\C-b)"
		      erc-version-string
		      erc-official-location)))
  nil)

(defvar erc-ctcp-query-SOUND-hook '(erc-ctcp-query-SOUND))
(defun erc-ctcp-query-SOUND (proc nick login host to msg)
  (when (string-match "^SOUND\\s-+\\(\\S-+\\)\\(\\(\\s-+.*\\)\\|\\(\\s-*\\)\\)$" msg)
    (let ((sound (match-string 1 msg))
	  (comment (match-string 2 msg)))
      (if erc-play-sound (erc-play-sound sound))
      (erc-display-message
       nil 'notice nil
       (format "%s plays %s:%s" nick sound comment))))
  nil)

(defvar erc-ctcp-query-PAGE-hook '(erc-ctcp-query-PAGE))
(defun erc-ctcp-query-PAGE (proc nick login host to msg)
  (when (string-match "PAGE\\(\\s-+.*\\)?$" msg)
    (let* ((m (match-string 1 msg))
	   (page-msg (if m (erc-interpret-controls (substring m 1))
		       "[no message]")))
      (if m (setq m (substring m 1)))
      (erc-display-message
       nil 'notice nil
       (if erc-page-function
	   (progn
	     (funcall erc-page-function nick page-msg)
	     (concat nick " pages you: " m))
	 (message (concat "PAGE from " nick " (" login "@" host "): " page-msg))
	 (beep)
	 (concat nick " pages you: " m)))))
  nil)


(defun erc-process-ctcp-request (sndr msg &optional host login)
  "Process incoming CTCP request.
SNDR is sender's nickname. MSG is the message contents. The function
returns a string to be displayed or NIL"
  (erc-log (format "process-ctcp-request: [%s] %s" sndr msg))
;;; Don't display everything, only those that usually go silent
;  (if erc-paranoid
;      (erc-display-line
;       (erc-highlight-error
;	(format "==> CTCP request from %s: %s\n" sndr msg))))
  (cond
   ;; ACTION
   ((string-match "^ACTION\\s-\\(.*\\)\\s-*$" msg)
    (let ((s (match-string 1 msg)))
      (erc-put-text-property 0 (length s) 'face 'erc-action-face s)
      (concat (erc-format-timestamp)
	      (erc-format-message 'ACTION
				  ?n sndr ?u login ?h host ?a s))))
   ;; CLIENTINFO
   ((string-match "^CLIENTINFO\\(\\s-*\\|\\s-+.*\\)$" msg)
    (if erc-paranoid
	(erc-display-line
	 (erc-highlight-error
	  (format "==> CTCP request from %s (%s@%s): %s\n"
		  sndr login host msg))))
    (if (not erc-disable-ctcp-replies)
	(let ((s (erc-client-info (erc-trim-string (match-string 1 msg)))))
	  (erc-send-ctcp-notice sndr (format "CLIENTINFO %s" s))))
    nil)
   ;; ECHO
   ((string-match "^ECHO\\s-+\\(.*\\)\\s-*$" msg)
    (if erc-paranoid
	(erc-display-line
	 (erc-highlight-error
	  (format "==> CTCP request from %s (%s@%s): %s\n" sndr login host msg))))
    (if (not erc-disable-ctcp-replies)
	(let ((s (match-string 1 msg)))
	  (erc-send-ctcp-notice sndr (format "ECHO %s" s))
	  (erc-put-text-property 0 (length s) 'face 'erc-action-face s)
	  (concat sndr " [ECHO]> " s))))
   ;; FINGER
   ((string= "FINGER" msg)
    (if erc-paranoid
	(erc-display-line
	 (erc-highlight-error
	  (format "==> CTCP request from %s (%s@%s): %s\n" sndr login host msg))))
    (if (not erc-disable-ctcp-replies)
	(let ((s (if erc-anonymous-login
		 (format "FINGER I'm %s." (erc-current-nick))
	       (format "FINGER %s (%s@%s)."
		       (user-full-name)
		       (user-login-name)
		       (system-name))))
	  (ns (erc-time-diff last-sent-time (erc-current-time))))
      (if (> ns 0)
	  (setq s (concat s " Idle for " (erc-sec-to-time ns))))
      (erc-send-ctcp-notice sndr s)))
    nil)
   ;; PING
   ((string-match "^PING\\s-+\\([0-9]+\\)" msg)
    (if erc-paranoid
	(erc-display-line
	 (erc-highlight-error
	  (format "==> CTCP request from %s (%s@%s): %s\n" sndr login host msg))))
    (if (not erc-disable-ctcp-replies)
	(erc-send-ctcp-notice sndr (format "PING %s" (match-string 1 msg))))
    nil)
   ;; TIME
   ((string= msg "TIME")
    (if erc-paranoid
	(erc-display-line
	 (erc-highlight-error
	  (format "==> CTCP request from %s (%s@%s): %s\n" sndr login host msg))))
    (if (not erc-disable-ctcp-replies)
	(erc-send-ctcp-notice sndr (format "TIME %s" (current-time-string))))
    nil)
   ;; USERINFO
   ((string= msg "USERINFO")
    (if erc-paranoid
	(erc-display-line
	 (erc-highlight-error
	  (format "==> CTCP request from %s (%s@%s): %s\n" sndr login host msg))))
    (if (not erc-disable-ctcp-replies)
	(erc-send-ctcp-notice sndr (format "USERINFO %s"
					   erc-user-information)))
    nil)
   ;; VERSION request
   ((string= "VERSION" msg)
    (if erc-paranoid
	(erc-display-line
	 (erc-highlight-error
	  (format "==> CTCP request from %s (%s@%s): %s\n" sndr login host msg))))
    (if (not erc-disable-ctcp-replies)
	(erc-send-ctcp-notice
	 sndr
	 (format "VERSION \C-bERC\C-b v%s - an IRC client for emacs (\C-b%s\C-b)"
		 erc-version-string
		 erc-official-location)))
    nil)
   ;; SOUND request
   ((string-match "SOUND\\s-+\\(\\S-+\\)\\(\\(\\s-+.*\\)\\|\\(\\s-*\\)\\)$" msg)
    (let ((sound (match-string 1 msg))
	  (comment (match-string 2 msg)))
      (if erc-play-sound (erc-play-sound sound))
      (erc-make-notice
       (format "%s plays %s:%s" sndr sound comment))))
   ;; PAGE request
   ((string-match "PAGE\\(\\s-+.*\\)?$" msg)
    (let* ((m (match-string 1 msg))
	   (page-msg (if m (erc-interpret-controls (substring m 1))
		       "[no message]")))
      (if m (setq m (substring m 1)))
      (erc-make-notice
       (if erc-page-function
	   (progn
	     (apply erc-page-function (list sndr page-msg))
	     (concat sndr " pages you: " m))
	 (message (concat "PAGE from " sndr "(" login "@"
			  host "): " page-msg))
	 (beep)
	 (concat sndr " pages you: " m)))))
   ;; all other requests
   (t
    (erc-make-notice
     (format "Unknown CTCP request from %s (%s@%s): %s"
	     sndr login host msg)))))

(defun erc-process-ctcp-reply (proc parsed nick login host msg)
  (let* ((type (car (split-string msg)))
	 (hook (intern (concat "erc-ctcp-reply-" type "-hook"))))
    (if (boundp hook)
	(run-hook-with-args-until-success hook proc nick login host (aref parsed 2) msg)
      (erc-display-message
       parsed 'notice nil
       (format "Unknown CTCP message from %s: %s"
	       nick msg)))))

(defvar erc-ctcp-reply-ECHO-hook '(erc-ctcp-reply-ECHO))
(defun erc-ctcp-reply-ECHO (proc nick login host to msg)
  (when (string-match "^ECHO\\s-+\\(.*\\)\\s-*$" msg)
    (let ((s (format "%s [ECHO]> %s" nick (match-string 1 msg))))
      (erc-put-text-property 0 (length s) 'face 'erc-action-face s)
      (erc-display-message
       nil nil nil
       s)))
  nil)

(defvar erc-ctcp-reply-CLIENTINFO-hook '(erc-ctcp-reply-CLIENTINFO))
(defun erc-ctcp-reply-CLIENTINFO (proc nick login host to msg)
  (when (string-match "^CLIENTINFO\\s-+\\(.*\\)\\s-*$" msg)
    (let ((s (format "%s [CLIENTINFO]> %s" nick (match-string 1 msg))))
      (erc-put-text-property 0 (length s) 'face 'erc-action-face s)
      (erc-display-message
       nil nil nil
       s)))
  nil)

(defvar erc-ctcp-reply-FINGER-hook '(erc-ctcp-reply-FINGER))
(defun erc-ctcp-reply-FINGER (proc nick login host to msg)
  (when (string-match "^FINGER\\s-+\\(.*\\)\\s-*$" msg)
    (let ((s (format "%s [FINGER]> %s" nick (match-string 1 msg))))
      (erc-put-text-property 0 (length s) 'face 'erc-action-face s)
      (erc-display-message
       nil nil nil
       s)))
  nil)

(defvar erc-ctcp-reply-PING-hook '(erc-ctcp-reply-PING))
(defun erc-ctcp-reply-PING (proc nick login host to msg)
  (when (string-match "^PING\\s-+\\([0-9]+\\)" msg)
    (let* ((pt (string-to-number (concat (match-string 1 msg) ".0")))
	   (ct (erc-current-time))
	   (ns (erc-time-diff pt ct))
	   (pair (assoc pt pings)))
      (cond
       (pair
	;; do not delete our pings - we may have several replies if a
	;; ping was sent to a channel
	;(setq pings (delete pair pings))
	(erc-display-message
	 nil 'notice nil
	 (format "Ping time to %s%s is %s"
		 nick
		 (if (string= nick (cdr pair)) ""
		   (concat " (" (cdr pair) ")"))
		 (erc-sec-to-time ns))))
       (t
	(erc-display-message
	 nil 'error nil
	 (format "Unexpected PING response from %s (time %s)" nick pt))))))
  nil)
(defvar erc-ctcp-reply-TIME-hook '(erc-ctcp-reply-TIME))
(defun erc-ctcp-reply-TIME (proc nick login host to msg)
  (when (string-match "^TIME\\s-+\\(.*\\)\\s-*$" msg)
    (let ((s (format "%s [TIME]> %s" nick (match-string 1 msg))))
      (erc-put-text-property 0 (length s) 'face 'erc-action-face s)
      (erc-display-message
       nil nil nil
       s)))
  nil)

(defvar erc-ctcp-reply-VERSION-hook '(erc-ctcp-reply-VERSION))
(defun erc-ctcp-reply-VERSION (proc nick login host to msg)
  (when (string-match "^VERSION\\s-+\\(.*\\)\\s-*$" msg)
    (let ((s (format "%s [VERSION]> %s" nick (match-string 1 msg))))
      (erc-put-text-property 0 (length s) 'face 'erc-action-face s)
      (erc-display-message
       nil nil nil
       s)))
  nil)


(defun erc-process-ctcp-response (sndr msg)
  "Process incoming CTCP response.
SNDR is sender's nickname.
MSG is the message contents."
  (erc-log (format "process-ctcp-response: [%s] %s" sndr msg))
  (cond
   ;; ECHO
   ((string-match "^ECHO\\s-+\\(.*\\)\\s-*$" msg)
    (let ((s (format "%s [ECHO]> %s" nick (match-string 1 msg))))
      (erc-put-text-property 0 (length s) 'face 'erc-action-face s)
      s))
   ;; CLIENTINFO
   ((string-match "^CLIENTINFO\\s-+\\(.*\\)\\s-*$" msg)
    (let ((s (format "%s [CLIENTINFO]> %s" nick (match-string 1 msg))))
      (erc-put-text-property 0 (length s) 'face 'erc-action-face s)
      s))
   ;; FINGER
   ((string-match "^FINGER\\s-+\\(.*\\)\\s-*$" msg)
    (let ((s (format "%s [FINGER]> %s" nick (match-string 1 msg))))
      (erc-put-text-property 0 (length s) 'face 'erc-action-face s)
      s))
   ;; PING
   ((string-match "^PING\\s-+\\([0-9]+\\)" msg)
    (let* ((pt (string-to-number (concat (match-string 1 msg) ".0")))
	   (ct (erc-current-time))
	   (ns (erc-time-diff pt ct))
	   (pair (assoc pt pings)))
      (cond
       (pair
	;; do not delete our pings - we may have several replies if a
	;; ping was sent to a channel
	;(setq pings (delete pair pings))
	(erc-make-notice
	 (format "Ping time to %s%s is %s"
		 sndr
		 (if (string= sndr (cdr pair)) ""
		   (concat " (" (cdr pair) ")"))
		 (erc-sec-to-time ns))))
       (t
	(erc-highlight-error
	 (format "Unexpected PING response from %s (time %s)" nick pt))))))
   ;; TIME
   ((string-match "^TIME\\s-+\\(.*\\)\\s-*$" msg)
    (let ((s (format "%s [TIME]> %s" nick (match-string 1 msg))))
      (erc-put-text-property 0 (length s) 'face 'erc-action-face s)
      s))
   ;; VERSION response
   ((string-match "^VERSION\\s-+\\(.*\\)\\s-*$" msg)
    (let ((s (format "%s [VERSION]> %s" nick (match-string 1 msg))))
      (erc-put-text-property 0 (length s) 'face 'erc-action-face s)
      s))
   (t
    (erc-make-notice
     (format "Unknown CTCP message from %s: %s"
	     nick msg)))))

(defun erc-process-away (proc away-p)
  "Do additional processing of user going away (if AWAY-P is non-nil),
or coming back"
  (let ((sessionbuf (process-buffer proc)))
    (when sessionbuf
      (with-current-buffer sessionbuf
	(when erc-away-nickname
	  (erc-log (format "erc-process-away: away-nick: %s, away-p: %s" erc-away-nickname away-p))
	  (erc-cmd-NICK (if away-p
			    erc-away-nickname
			  erc-nick)))
	(cond
	 (away-p
	  (erc-with-all-buffers-of-server proc nil
					  (setq away (current-time))))
	 (t
	  (let ((away-time away))
	    ;; away must be set to NIL BEFORE sending anything to prevent
	    ;; an infinite recursion
	    (erc-with-all-buffers-of-server proc nil
					    (setq away nil))
	    (save-excursion
	      (set-buffer erc-active-buffer)
	      (when erc-public-away-p
	      (erc-send-action
	       (erc-default-target)
	       (if away-time
		   (format "is back (gone for %s)"
			   (erc-sec-to-time (erc-time-diff
					     (erc-emacs-time-to-erc-time away-time)
					     (erc-current-time))))
		   "is back")))))))))
    (erc-update-mode-line)))

;;;; List of channel members handling

(defun erc-refresh-channel-members (chnl names-string &optional add)
  "If there is a buffer for CHNL, updates the `channel-members'
variable for that buffer according to NAMES-STRING - the string
listing all the names on the channel.  If optional ADD is non-nil, do
not remove existing names from the list."
  (let ((buf (and (erc-channel-p chnl)
		  (boundp 'erc-process) erc-process
		  (erc-get-buffer chnl erc-process)))
	(ob (current-buffer))
	names name op voice res)
    (when buf
      (set-buffer buf)
;;       (while (string-match "^\\s-*\\(\\S-+\\)\\(\\s-+.*$\\|$\\)"
;;			   names-string)
;;	(setq names (cons (match-string 1 names-string) names))
;;	(setq names-string (match-string 2 names-string)))
      (setq names (split-string names-string))
      (if (not add) (setq channel-members nil))
      (let ((erc-channel-members-changed-hook nil))
	(dolist (item names)
	  (cond ((string-match "^@\\(.*\\)$" item)
		 (setq name (match-string 1 item)
		       op 'on
		       voice 'off))
		((string-match "^+\\(.*\\)$" item)
		 (setq name (match-string 1 item)
		       op 'off
		       voice 'on))
		(t (setq name item
			 op 'off
			 voice 'off)))
	  (erc-update-channel-member chnl name name add op voice)))
      (run-hooks 'erc-channel-members-changed-hook)
      (set-buffer ob))))

(defcustom erc-channel-members-changed-hook nil
  "*This hook is called everytime the variable `channel-members' changes.
The buffer where the change happened is current while this hook is called."
  :group 'erc-hooks
  :type 'hook)

(defun erc-update-channel-member (chnl nick new-nick
				       &optional add op voice host email
				       full-name info)
  "Updates the user info in the channel CHNL.  The user's NICK will be
changed to NEW-NICK (provide the same nick if not changed).  If ADD is
non-nil, add the user if it is not in the list.  The other optional
arguments OP, HOST, EMAIL and FULL-NAME change the appropriate fields.
INFO is the additional info (sign-on time, comments, etc.), VOICE is
the voice mode in the channel.  Note, that OP = NIL means the status
does not change, so use 'ON or 'OFF to set the OP status instead of T
and NIL.  The same goes for VOICE.

If the info is actually updated, Returns non-nil and calls
`erc-channel-members-updated-hook'."

  (let ((buf (and ;;(string-match "^[&#+!]" chnl) update for private chat too
		  (boundp 'erc-process) erc-process
		  (erc-get-buffer chnl erc-process)))
	(ob (current-buffer))
	res names entry ently-local new-entry changed)
    (when buf
      (set-buffer buf)
      (setq names (and (boundp 'channel-members)
		       channel-members))
      (setq entry-local (assoc nick names))
      (setq entry (or entry-local
		      (assoc nick erc-last-channel-names)))
      (if entry
	  (progn
	    (erc-log (format "update-member: entry = %S" entry))
	    ;;(message "update-member: entry = %S, add=%s" entry add)
	    (let* ((nick0 (nth 0 entry))
		   (op0 (nth 1 entry))
		   (voice0 (nth 2 entry))
		   (host0 (nth 3 entry))
		   (email0 (nth 4 entry))
		   (full0 (nth 5 entry))
		   (info0 (nth 6 entry)))
	      (setq new-entry (list new-nick
				    (cond ((eq op 'on) t)
					  ((eq op 'off) nil)
					  (entry-local op0)
					  (t nil))
				    (cond ((eq voice 'on) t)
					  ((eq voice 'off) nil)
					  (entry-local voice0)
					  (t nil))
				    (if host host host0)
				    (if email email email0)
				    (if full-name full-name full0)
				    (if info info info0))))
	    (cond ((and entry-local (not (equal entry-local new-entry)))
		   (erc-log (format "update-member: new-entry = %S"
				    new-entry))
		   (while names
		     (if (string= (erc-downcase nick)
				  (erc-downcase (caar names)))
			 (setcar names new-entry))
;;		     (let* ((entry0 (car names))
;;			    (nick0 (nth 0 entry0)))
;;		       (setq res (cons (if (string= (erc-downcase nick)
;;						    (erc-downcase nick0))
;;					   new-entry entry0)
;;				       res)))
		     (setq names (cdr names)))
;;		   (setq channel-members (nreverse res))
		   (setq changed t))
		  ;; no nick in the list yet, add it if needed
		  ((and (not entry-local) add)
		   (setq channel-members
			 (cons new-entry channel-members))
		   (setq changed t))))
	(when add
	  (setq channel-members (cons (list new-nick
					    (cond ((eq op 'on) t)
						  ((eq op 'off) nil)
						  (t op))
					    (cond ((eq voice 'on) t)
						  ((eq voice 'off) nil)
						  (t voice))
					    host email full-name info)
				      names))
	  (setq changed t)))
      (when (and changed (or add (not (string-equal nick new-nick))))
	(run-hooks 'erc-channel-members-changed-hook))
      (set-buffer ob))
    changed))

(defun erc-get-channel-members (chnl)
  "Returns the value of `channel-members' in the channel buffer assigned
to CHNL."
  (let ((buf (and (erc-channel-p chnl)
		  (boundp 'erc-process) erc-process
		  (erc-get-buffer chnl erc-process))))
    (when buf
      (with-current-buffer buf
	(and (boundp 'channel-members) channel-members)))))

(defun erc-remove-channel-member (chnl nick)
  "Takes CHNL and NICK, removes the NICK from the channel's CHNL
membership list."
  (setq nick (erc-downcase nick))
  (let ((buf (or (and (bufferp chnl) chnl)
		 (and (boundp 'erc-process) erc-process
		      (erc-get-buffer chnl erc-process))))
	changed)
    (when buf
      (with-current-buffer buf
	(let ((lst channel-members))
	  (while lst
	    (if (string-equal (erc-downcase (caar lst)) nick)
		(setq channel-members (delq (car lst) channel-members)
		      changed t
		      lst nil))
	    (setq lst (cdr lst)))))
      (when changed
	(run-hooks 'erc-channel-members-changed-hook)))))


(defun erc-update-channel-topic (chnl topic &optional modify)
  "Finds a buffer for the channel CHNL and sets the TOPIC for it.  If
optional MODIFY is 'append or 'prepend, then append or prepend the
TOPIC string to the current topic."
  (let ((buf (and (erc-channel-p chnl)
		  (boundp 'erc-process) erc-process
		  (erc-get-buffer chnl erc-process))))
    (when buf
      (with-current-buffer buf
	(when (boundp 'channel-topic)
	  (cond ((eq modify 'append)
		 (setq channel-topic (concat channel-topic topic)))
		((eq modify 'prepend)
		 (setq channel-topic (concat topic channel-topic)))
		(t (setq channel-topic topic))))
	(erc-update-mode-line-buffer buf)))))

(defun erc-set-modes (tgt mode-string)
  "Set the modes for the TGT provided as MODE-STRING."
  (let* ((modes (erc-parse-modes mode-string))
	 (add-modes (nth 0 modes))
	 (remove-modes (nth 1 modes))
	 ;; list of triples: (mode-char 'on/'off argument)
	 (arg-modes (nth 2 modes)))
    (cond ((erc-channel-p tgt); channel modes
	   (let ((buf (and (boundp 'erc-process) erc-process
			   (erc-get-buffer tgt erc-process)))
		 (ob (current-buffer)))
	     (when buf
	       (set-buffer buf)
	       (setq channel-modes add-modes)
	       (setq channel-user-limit nil)
	       (while arg-modes
		 (let ((mode (nth 0 (car arg-modes)))
		       (onoff (nth 1 (car arg-modes)))
		       (arg (nth 2 (car arg-modes))))
		   (cond ((string-match "^[Ll]" mode)
			  (erc-update-channel-limit tgt onoff arg))
			 (t nil)))
		 (setq arg-modes (cdr arg-modes)))
	       (erc-update-mode-line-buffer buf)
	       (erc-update-channel-info-buffer tgt)
	       (set-buffer ob))))
	  (t (setq erc-user-modes add-modes))) ; we do not keep our nick's modes yet
    ))

(defun erc-sort-strings (strings)
  "Sorts the list of STRINGS in the lexicographic order.  Does not
have a side effect."
  (sort (copy-sequence strings) 'string<))

(defun erc-parse-modes (mode-string)
  "Return a list of 3 elements: '(add-modes remove-modes arg-modes).
The add-modes and remove-modes are lists of single-character strings
for modes without parameters to add and remove respectively.  The
arg-modes is a list of triples of the form '(mode-char on/off
argument)."
  (if (string-match "^\\s-*\\(\\S-+\\)\\(\\s-.*$\\|$\\)" mode-string)
      (let ((chars (mapcar 'char-to-string (match-string 1 mode-string)))
	    (args-str (match-string 2 mode-string));; arguments in channel modes
	    (args nil)
	    (add-modes nil)
	    (remove-modes nil)
	    (arg-modes nil); list of triples: (mode-char 'on/'off argument)
	    (add-p t))
	;; make the argument list
	(while (string-match "^\\s-*\\(\\S-+\\)\\(\\s-+.*$\\|$\\)" args-str)
	  (setq args (cons (match-string 1 args-str) args))
	  (setq args-str (match-string 2 args-str)))
	(setq args (nreverse args))
	;; collect what modes changed, and match them with arguments
	(while chars
	  (cond ((string= (car chars) "+") (setq add-p t))
		((string= (car chars) "-") (setq add-p nil))
		((string-match "^[ovbOVB]" (car chars))
		 (setq arg-modes (cons (list (car chars)
					     (if add-p 'on 'off)
					     (if args (car args) nil))
				       arg-modes))
		 (if args (setq args (cdr args))))
		((string-match "^[Ll]" (car chars))
		 (setq arg-modes (cons (list (car chars)
					     (if add-p 'on 'off)
					     (if (and add-p args)
						 (car args) nil))
				       arg-modes))
		 (if (and add-p args) (setq args (cdr args))))
		(add-p (setq add-modes (cons (car chars) add-modes)))
		(t (setq remove-modes (cons (car chars) remove-modes))))
	  (setq chars (cdr chars)))
	(setq add-modes (nreverse add-modes))
	(setq remove-modes (nreverse remove-modes))
	(setq arg-modes (nreverse arg-modes))
	(list add-modes remove-modes arg-modes))
    nil))

(defun erc-update-modes (tgt mode-string &optional nick host login)
  "Updates the mode information for TGT, provided as MODE-STRING.
Optional arguments: NICK, HOST and LOGIN - the attributes of the
person who changed the modes."
  (let* ((modes (erc-parse-modes mode-string))
	 (add-modes (nth 0 modes))
	 (remove-modes (nth 1 modes))
	 ;; list of triples: (mode-char 'on/'off argument)
	 (arg-modes (nth 2 modes)))
    ;; now parse the modes changes and do the updates
    (cond ((erc-channel-p tgt); channel modes
	   (let ((buf (and (boundp 'erc-process) erc-process
			   (erc-get-buffer tgt erc-process)))
		 (ob (current-buffer)))
	     (when buf
	       (set-buffer buf)
	       (if (not (boundp 'channel-modes))
		   (set (make-local-variable 'channel-modes) nil))
	       (while remove-modes
		 (setq channel-modes (delete (car remove-modes) channel-modes)
		       remove-modes (cdr remove-modes)))
	       (while add-modes
		 (setq channel-modes (cons (car add-modes) channel-modes)
		       add-modes (cdr add-modes)))
	       (setq channel-modes (erc-sort-strings channel-modes))
	       (while arg-modes
		 (let ((mode (nth 0 (car arg-modes)))
		       (onoff (nth 1 (car arg-modes)))
		       (arg (nth 2 (car arg-modes))))
		   (cond ((string-match "^[oO]" mode)
			  (erc-update-channel-member tgt arg arg nil onoff))
			 ((string-match "^[Vv]" mode)
			  (erc-update-channel-member tgt arg arg nil nil onoff))
			 ((string-match "^[Ll]" mode)
			  (erc-update-channel-limit tgt onoff arg))
			 (t nil)); only ops are tracked now
		   (setq arg-modes (cdr arg-modes))))
	       (erc-update-mode-line buf)
	       (erc-update-channel-info-buffer tgt))))
	  ;; nick modes - ignored at this point
	  (t nil))))

(defun erc-update-channel-limit (chnl onoff n)
  "Updates the limit on the number of users."
  (let ((chnl-buf (and ;;(string-match "^[&#+!]" chnl); allow private chat too
		       (boundp 'erc-process) erc-process
		       (erc-get-buffer chnl erc-process)))
	(ob (current-buffer)))
    (when (and chnl-buf
	       (or (not (eq onoff 'on))
		   (and (stringp n) (string-match "^[0-9]+$" n))))
      (set-buffer chnl-buf)
      (cond ((eq onoff 'on) (setq channel-user-limit (string-to-number n)))
	    (t (setq channel-user-limit nil))))))

(defun erc-find-channel-info-buffer (chnl)
  "Returns the channel info buffer.  If it doesn't exist, creates it.
The channel must be a string starting with `#' character.  If it is
not, nil is returned and no buffer is created."
  (let* ((chnl-buf (and ;;(string-match "^[&#+!]" chnl); allow private chat too
			(boundp 'erc-process) erc-process
			(erc-get-buffer chnl erc-process)))
	 (ob (current-buffer))
	 name res)
    (when chnl-buf
      (set-buffer chnl-buf)
      ;; if chnl is a real channel, the target must match
      (when (and chnl
		 (or (not (erc-channel-p chnl))
		     (and (erc-default-target)
			  (string= (downcase (erc-default-target))
				   (downcase chnl)))))
	;;(message "test is true, chnl = %S" chnl)
	(setq name (concat (buffer-name chnl-buf) ":INFO"))
	(setq res (get-buffer name))
	;;(message "res (after 1st setq) = %S" res)
	(when (not res)
	  (setq res (get-buffer-create name))
	  ;;(message "res (after 2nd setq) = %S" res)
	  (set-buffer res)
	  (erc-info-mode)))
      (set-buffer ob))
    res))

(defun erc-update-channel-info-buffer (chnl)
  "This function updates the channel info buffer with current info,
like topic, modes, users, etc."
  (let* ((chnl-buf (or (and (bufferp chnl) chnl)
		       (and ;;(string-match "^[&#+!]" chnl)
			(boundp 'erc-process) erc-process
			(erc-get-buffer chnl erc-process))))
	 (ob (current-buffer))
	 (info-buf (erc-find-channel-info-buffer chnl))
	 limit names topic modes info-point)
    ;;(message "chnl-buf=%s" chnl-buf)
    ;;(message "info-buf=%s" info-buf)
    ;; if no target, do not update anything
    (when (and chnl chnl-buf info-buf)
      (set-buffer chnl-buf)
      (setq names (and (boundp 'channel-members) channel-members)
	    topic (and (boundp 'channel-topic) channel-topic)
	    modes (and (boundp 'channel-modes) channel-modes)
	    limit (and (boundp 'channel-user-limit) channel-user-limit))
      ;;(message "info-buf=%s" info-buf)
      (set-buffer info-buf)
      (setq info-point (point))
      ;;(message "info-point = %s" info-point)
      (toggle-read-only -1)
      (erase-buffer)
      (insert (erc-interpret-controls
	       (cond ((erc-channel-p chnl)
		      (format "\C-c3Channel \C-b%s\C-b[%s]:\C-c %s"
			      chnl (length names)
			      (if (and topic (not (string= "" topic)))
				  topic
				"<no topic>")))
		     (t (format "\C-c5Private from \C-b%s\C-b[%s]\C-c"
				chnl (length names))))))
      (if modes
	  (insert (concat "\nmodes: +"
			  (mapconcat 'identity modes ""))))
      (if limit (insert (format "   user limit = %s" limit)))
      (insert "\n\n")
      (if names
	  (while names
	    (let* ((entry (car names))
		   (nick (nth 0 entry))
		   (op (nth 1 entry))
		   (voice (nth 2 entry))
		   (host (nth 3 entry))
		   (email (nth 4 entry))
		   (full (nth 5 entry))
		   (info (nth 6 entry)))
	      (insert
	       (concat (if op "@" " ")
			(if voice "+" " ") nick " "
			(if email email "")
			(if host (concat "@" host) "")
			(if full
			    (concat " "
				    (if (> (length full) 25)
					(concat (substring full 0 22) "...")
				      full))
			  "")
			(if info (concat " " info) "") "\n")))
	    (setq names (cdr names))))
      (goto-char info-point)
      (toggle-read-only 1)
      (set-buffer ob))))

(defun erc-remove-member-all-channels (nick &optional buffer-list)
  "Does what it says in all the buffers for the current session."
  (let ((ob (current-buffer))
	tgt)
    (if (not buffer-list)
	(setq buffer-list (and (boundp 'erc-process)
			       (erc-buffer-list nil erc-process))))
    (mapc
     (lambda (buffer)
       (when (buffer-live-p buffer)
	 (set-buffer buffer)
	 (setq tgt (erc-default-target))
	 (if (and tgt (erc-channel-p tgt))
	     (erc-remove-channel-member buffer nick))))
     buffer-list)
    (set-buffer ob)))

(defun erc-update-member-all-channels (nick new-nick &optional add op voice
				       host email full-name info buffer-list)
  "Does what it says in all the buffers for the current session."
  (let ((ob (current-buffer))
	tgt)
    (if (not buffer-list)
	(setq buffer-list (and (boundp 'erc-process)
			       (erc-buffer-list nil erc-process))))
    (dolist (buffer buffer-list)
      (when (buffer-live-p buffer)
	(set-buffer buffer)
	(setq tgt (erc-default-target))
	(if (and tgt (erc-channel-p tgt))
	    (erc-update-channel-member tgt nick new-nick add op voice
				       host email full-name info))))
    (set-buffer ob)))

(defun erc-update-channel-info-buffers (&optional buffer-list)
  "Does what it says in all the buffers for the current session."
  (let ((ob (current-buffer))
	tgt)
    (if (not buffer-list)
	(setq buffer-list (and (boundp 'erc-process)
			       (erc-buffer-list nil erc-process))))
    (dolist (buffer buffer-list)
      (when (buffer-live-p buffer)
	(set-buffer buffer)
	(setq tgt (erc-default-target))
	(if (and tgt (erc-channel-p tgt))
	    (erc-update-channel-info-buffer tgt))))
    (set-buffer ob)))

(defun erc-handle-user-status-change (typ nlh &optional l)
  "Handle changes in any user's status. So far, only nick change is handled.

Generally, the TYP argument is a symbol describing the change type, NLH is
a list containing the original nickname, login name and hostname for the user,
and L is a list containing additional TYP-specific arguments.

So far the following TYP/L pairs are supported:

       event                    TYP                    L

    nickname change            'nick                (NEW-NICK)"
  (erc-log (format "user-change: type: %S  nlh: %S  l: %S" typ nlh l))
  (cond
   ;; nickname change
   ((equal typ 'nick)
    t)

   (t
    nil)))

(defun erc-highlight-notice (s)
  "Highlight notice message S and return it.
See also variable `erc-notice-highlight-type'"
  (cond
   ((eq erc-notice-highlight-type 'prefix)
    (erc-put-text-property 0 (length erc-notice-prefix) 'face 'erc-notice-face s)
    s)
   ((eq erc-notice-highlight-type 'all)
    (erc-put-text-property 0 (length s) 'face 'erc-notice-face s)
    s)
   (t s)))

(defun erc-make-notice (message)
  "Notify the user of MESSAGE."
  (when erc-minibuffer-notice
    (message "%s" message))
  (erc-highlight-notice (concat (erc-format-timestamp)
				erc-notice-prefix
				message)))

(defun erc-keywords (s)
  "Highlight strings in S and return it.
See also variable `erc-keywords'."
  (cond
   ((eq erc-keyword-highlight-type 'keyword)
    (let ((strs erc-keywords)
	  str pos)
      (while strs
	(setq str (car strs)
	      strs (cdr strs)
	      pos (string-match str s))
	(while pos
	  (erc-put-text-property (match-beginning 0)
			     (match-end 0)
			     'face 'erc-keyword-face s)
	  (setq pos (string-match str s (match-end 0)))))
      s))
   ((eq erc-keyword-highlight-type 'all)
    (when (erc-list-match erc-keywords s)
      (erc-put-text-property 0 (length s)
			 'face 'erc-keyword-face s))
    (erc-put-text-properties 0 (length s) erc-keyword-highlight-props s))
   (t
    s)))

(defun erc-highlight-error (s)
  "Highlight error message S and return it"
  (erc-put-text-property 0 (length s) 'face 'erc-error-face s)
  s)

(defun erc-put-text-property (start end property value &optional object)
  "Set text-property for an object (usually a string).
START and END define the characters covered.
PROPERTY is the text-property set, usually the symbol `face'.
VALUE is the value for the text-property, usually a face symbol such as
the face `bold' or `erc-pal-face'.
OBJECT is a string which will be modified and returned.
The is modified without being copied first.

You can redefine or `defavice' this function in order add
EmacsSpeak support."
  (put-text-property start end property value object))

(defun erc-parse-user (string)
  "Parse IRC-type user specification (nick!login@host) to three separate
tokens and return them as a list."
  (if (string-match "^\\([^!]*\\)!\\([^@]*\\)@\\(.*\\)$" string)
      (list (match-string 1 string)
	    (match-string 2 string)
	    (match-string 3 string))
    (list string "" "")))

(defun erc-extract-nick (string)
  "Return the nick of a nick!user@host IRC type spec."
  (car (erc-parse-user string)))
(defun erc-put-text-properties (start end properties &optional object value-list)
  "Set text-properties for an object (usually a string).
If VALUE-LIST is empty, set each property in PROPERTIES to t, else set
each property to the corresponding value in VALUE-LIST."
  (unless value-list
    (setq value-list (mapcar (lambda (x)
			       t)
			     properties)))
  (mapcar* (lambda (prop value)
	     (erc-put-text-property start end prop value object))
	   properties value-list))

;;; Input area handling:

(defun erc-beg-of-input-line ()
  (save-excursion
    (if (and (boundp 'erc-insert-marker)
	     (markerp erc-insert-marker))
	(goto-char erc-insert-marker)
      (goto-char (beginning-of-line)))
    (point)))

(defun erc-end-of-input-line ()
  (point-max))

(defun erc-parse-current-line ()
  "Parse current input line.
Returns a pair (PART1 . PART2), where PART1 is the input part before the point
and PART2 is the part after the point."
  (save-excursion
    (let* ((erc-prompt-regexp (concat "^" (regexp-quote (erc-prompt)) " ")); "[ \t]*"))
	   (p1 (point))
	   (p0 (erc-beg-of-input-line))
	   (p2 (erc-end-of-input-line))
	   (l0 (buffer-substring p0 p2))
	   (l1 (buffer-substring p0 p1))
	   (l2 (buffer-substring p1 p2)))
;	   (l0 (buffer-substring-no-properties p0 p2))
;	   (l1 (buffer-substring-no-properties p0 p1))
;	   (l2 (buffer-substring-no-properties p1 p2)))
;;      (erc-log (format "parse-line: l0: %S  l1: %S  l2: %S\n" l0 l1 l2))
      (cond ((string-match erc-prompt-regexp l0)
	     (let ((i1 (match-end 0)))
	       (if (>= i1 (length l1))
		   (cons nil (substring l0 i1))
		 (cons (substring l1 i1) l2))))
	    (t (cons l1 l2))))))

(defun erc-send-distinguish-noncommands (str)
  "If str is an ERC non-command, set erc-insert-this to nil"
  (let* ((command (erc-extract-command-from-line str))
	 (cmd-fun (and command
		       (intern (concat "erc-cmd-" (car command))))))
    (when (and cmd-fun
	       (not (string-match "\n.+$" str))
	       (memq cmd-fun erc-noncommands-list))
      (setq erc-insert-this nil))))

(defun erc-send-current-line ()
  "Parse current line and send it to IRC."
  (interactive)
  (setq erc-active-buffer (current-buffer))
  (let* ((inhibit-read-only t)
	 (line (progn
		 (goto-char (point-max))
		 (while (and (> (point) (+ 1 (length (erc-prompt)) erc-insert-marker))
			     (or (memq (char-before) '(?\r ?\n))
				 (eq ?  (char-syntax (char-before)))))
		   (delete-backward-char 1))
		 (erc-parse-current-line)))
	 (str  (concat (car line) (cdr line)))
	 (old-buf (current-buffer)))
    (cond
     ((string-match "^\\s-*\n*$" str)
      (if erc-warn-about-blank-lines
	  (progn
	    (message "Blank line - ignoring...")
	    (ding))))
     ((<= (point) erc-insert-marker)
      (message "Point is not in the input area")
      (ding))
     (t
      (setq erc-send-this t)
      (setq erc-insert-this t)
      (run-hook-with-args 'erc-send-pre-hook str)
      (delete-region (erc-beg-of-input-line) (erc-end-of-input-line))
      (when erc-send-this
	(let* ((lines (split-string str "\n"))
	       (multiline-p (< 1 (length lines))))
	  (mapc
	   (lambda (line)
	     (when erc-insert-this
	       (let ((insert-position (point)))
		 (if (and (not multiline-p)
			  (eq (elt line 0) ?/))	; is it a non-pasted command?
		     (erc-display-prompt)
		   (insert (erc-format-timestamp) ; ok, it's a privmsg.
			   (concat "<" (erc-current-nick) "> ")))
		 (when (string-match "[\n]+$" line) ; remove the \ns at the end.
		   (setq line (substring s 0 (match-beginning 0))))
		 (erc-put-text-property 0 (length line) 'face 'erc-input-face line)
		 (insert (if erc-uncontrol-input-line
			     (erc-interpret-controls line)
			   line))
		 (goto-char (point-max))
		 (open-line 1)
		 (goto-char (point-max))
		 (save-excursion
		   (save-match-data
		     (save-restriction
		       (narrow-to-region insert-position (point-max))
		       (run-hook-with-args 'erc-send-modify-hook)
		       (run-hook-with-args 'erc-send-post-hook))))))
	     (set-marker (process-mark erc-process) (point))
	     (set-marker erc-insert-marker (point))
	     (erc-process-input-line (concat line "\n") t multiline-p))
	   lines)))
      (goto-char (point-max))
      (when (buffer-live-p old-buf)
	(set-buffer old-buf)
	(erc-display-prompt)
	(when (featurep 'emacspeak)
	  (emacspeak-auditory-icon 'select-object)))))))


(defun erc-send-paragraph (&optional force)
  "Send multiple lines starting from the first `erc-prompt' before the
point till the end of buffer.  If a non-nil argument is provided,
force sending even under excess flood condition. Note that this function
is obsolete, \\[erc-send-current-line] handles multiline input."
  (interactive "P")
  (setq erc-active-buffer (current-buffer))
  (let* ((pnt (point))
	 (start (let ((re (concat "^" (erc-prompt)))
		      (doit t))
		  (while (and doit (/= (point) (point-min)))
		    (backward-char)
		    (if (looking-at re) (setq doit nil)))
		  (+ (point) (length (erc-prompt)) 1)))
	 (end (point-max))
	 (str (buffer-substring start end)))
    (delete-region start end)
    (goto-char (point-max))
    (setq str (erc-merge-controls str))
    (erc-load-irc-script-lines (erc-split-multiline-safe str) force t)
    (goto-char (point-max));; do it again, in case loading moves the point
    (open-line 1)
    (goto-char (point-max))
    (set-marker (process-mark erc-process) (point))
    (set-marker erc-insert-marker (point))
    (erc-display-prompt)))

(defun erc-extract-command-from-line (line)
  "Extract command and args from the input LINE.
If no command was given, return nil.  If command matches, return a
list of the form: (command args) where both elements are strings."
  (when (string-match "^/\\([A-Za-z]+\\)\\(\\s-+.*\\|\\s-*\\)$" line)
    (list (format "%s" (upcase (match-string 1 line))) ; command
	  (format "%s" (match-string 2 line)))))       ; args



(defun erc-split-multiline-safe (string)
  "Split STRING, containing multiple lines and return them in a list.
Do it only for STRING as the complete input, do not carry unfinished
strings over to the next call."
  (let ((l ())
	(i0 0)
	(doit t))
    (while doit
      (let ((i (string-match "\r?\n" string i0))
	    (s (substring string i0)))
	(cond (i (setq l (cons (substring string i0 i) l))
		 (setq i0 (match-end 0)))
	      ((> (length s) 0)
		 (setq l (cons s l))(setq doit nil))
	      (t (setq doit nil)))))
    (nreverse l)))

;; nick handling

(defun erc-push-nick (nick)
  "Push new nickname to a nickname stack"
  (let ((ob (current-buffer))
	(sb (erc-server-buffer)))
    (if sb
	(set-buffer sb))
    (setq nick-stk (cons nick nick-stk))
    (set-buffer ob)))

(defun erc-pop-nick ()
  "Remove topmost nickname from a stack"
  (let ((ob (current-buffer))
	(sb (erc-server-buffer)))
    (if sb
	(set-buffer sb))
    (if (null nick-stk)
	(error "Nickname stack empty")
      (setq nick-stk (cdr nick-stk)))
    (set-buffer ob)))

(defun erc-current-nick ()
  "Return current nickname (top of the nickname stack)"
  (let ((ob (current-buffer))
	(sb (erc-server-buffer))
	res)
    (if sb
	(set-buffer sb))
    (if nick-stk (setq res (car nick-stk)))
    (set-buffer ob)
    res))

(defun erc-original-nick ()
  "Return the original nickname the buffer was created with
\(bottom of the nickname stack)"
  (let ((ob (current-buffer))
	(sb (erc-server-buffer))
	res)
    (if sb
	(set-buffer sb))
    (if nick-stk
	(setq res (nth (- (length nick-stk) 1) nick-stk)))
    (set-buffer ob)
    res))

;; default target handling

(defun erc-default-target ()
  "Returns current default target (as a character string) or NIL if none."
  (let ((tgt (car erc-default-recipients)))
    (cond
     ((not tgt) nil)
     ((listp tgt) (cdr tgt))
     (t tgt))))

(defun erc-add-default-channel (ch)
  "Add channel to the default channel list. "

;;; This is no longer true.  The channel is added to another window
;;; and we don't want to mess the target there.
;"If the current default
;recepient is of QUERY type, then push the new default channel *after*
;the head"

  (let ((d1 (car erc-default-recipients))
	(d2 (cdr erc-default-recipients))
	(chl (downcase ch)))
;    (if (and (listp d1)
;	     (eq (car d1) 'QUERY))
;	(setq erc-default-recipients
;	      (cons d1 (cons chl d2)))
      (setq erc-default-recipients
	    (cons chl erc-default-recipients))
;)
))

(defun erc-delete-default-channel (ch &optional buffer)
  "Delete channel from the default channel list."
  (let ((ob (current-buffer)))
    (if (and buffer (bufferp buffer))
	(set-buffer buffer))
    (setq erc-default-recipients (delete (downcase ch) erc-default-recipients))
    (set-buffer ob)))

(defun erc-add-query (nick)
  "Add QUERY'd nickname to the default channel list. The previous
default target of QUERY type gets removed"
  (let ((d1 (car erc-default-recipients))
	(d2 (cdr erc-default-recipients))
	(qt (cons 'QUERY (downcase nick))))
    (if (and (listp d1)
	     (eq (car d1) 'QUERY))
	(setq erc-default-recipients (cons qt d2))
      (setq erc-default-recipients (cons qt erc-default-recipients)))))

(defun erc-delete-query ()
  "Delete the topmost target if it is a QUERY"

  (let ((d1 (car erc-default-recipients))
	(d2 (cdr erc-default-recipients)))
    (if (and (listp d1)
	     (eq (car d1) 'QUERY))
	(setq erc-default-recipients d2)
      (error "Current target is not a QUERY"))))

(defun erc-ignored-user-p (spec)
  "Takes a full SPEC of a user in the form \"nick!login@host\", and
matches against all the regexp's in `erc-ignore-list'.  If anyone
match, returns that regexp, and nil otherwise."
  (let ((lst erc-ignore-list))
    (while (and lst (not (string-match (car lst) spec)))
      (setq lst (cdr lst)))
    (and lst (car lst))))

(defun erc-list-match (lst str)
  "Return non-nil if any regexp in LST matches STR."
  (memq nil (mapcar (lambda (regexp)
		      (not (string-match regexp str)))
		    lst)))

;; Sound stuff - S.B.

(defun erc-play-sound (file)
  "Plays a sound file located in one of the directories in `erc-sound-path'
with a command `erc-play-command'."
  (let ((filepath (erc-find-file file erc-sound-path)))
    (if (and (not filepath) erc-default-sound)
	(setq filepath erc-default-sound))
    (cond ((and filepath (file-exists-p filepath))
	   (if (and (fboundp 'device-sound-enabled-p)
		    (device-sound-enabled-p))
	       ; For XEmacs
	       (play-sound-file filepath)
;	     (start-process "erc-sound" nil erc-play-command filepath)
	     (start-process "erc-sound" nil "/bin/tcsh"  "-c"
			    (concat erc-play-command " " filepath))))
	  (t (beep)))
    (erc-log (format "Playing sound file %S" filepath))))

;(defun erc-play-sound (file)
;  "Plays a sound file located in one of the directories in `erc-sound-path'
;   with a command `erc-play-command'."
;  (let ((filepath nil)
;	(paths erc-sound-path))
;    (while (and paths
;		(progn (setq filepath (concat (car paths) "/" file))
;		       (not (file-exists-p filepath))))
;      (setq paths (cdr paths)))
;    (if (and (not (and filepath (file-exists-p filepath)))
;	     erc-default-sound)
;	(setq filepath erc-default-sound))
;    (cond ((and filepath (file-exists-p filepath))
;;	   (start-process "erc-sound" nil erc-play-command filepath)
;	   (start-process "erc-sound" nil "/bin/tcsh"  "-c"
;			  (concat erc-play-command " " filepath))
;	   )
;	  (t (beep)))
;    (erc-log (format "Playing sound file %S" filepath))))

(defun erc-toggle-sound (&optional arg)
  "Toggles playing sounds on and off.  With positive argument,
  turns them on.  With any other argument turns sounds off."
  (interactive "P")
  (cond ((and (numberp arg) (> arg 0))
	 (setq erc-play-sound t))
	(arg (setq erc-play-sound nil))
	(t (setq erc-play-sound (not erc-play-sound))))
  (message "ERC sound is %s" (if erc-play-sound "ON" "OFF")))

;; other "toggles"

(defun erc-toggle-ctcp-autoresponse (&optional arg)
  "Toggles automatic CTCP replies (like VERSION and PING) on and off.
 With positive argument, turns them on.  With any other argument turns
 them off."
  (interactive "P")
  (cond ((and (numberp arg) (> arg 0))
	 (setq erc-disable-ctcp-replies t))
	(arg (setq erc-disable-ctcp-replies nil))
	(t (setq erc-disable-ctcp-replies (not erc-disable-ctcp-replies))))
  (message "ERC CTCP replies are %s" (if erc-disable-ctcp-replies "OFF" "ON")))

(defun erc-toggle-flood-control (&optional arg)
  "Toggles between strict and normal and no flood control."
  (interactive "P")
  (cond ((and (numberp arg) (> arg 0))
	 (setq erc-flood-protect 'strict))
	(arg (setq erc-flood-protect 'normal))
	((eq erc-flood-protect 'strict)
	 (setq erc-flood-protect nil))
	(erc-flood-protect (setq erc-flood-protect 'strict))
	(t (setq erc-flood-protect 'normal)))
  (message "ERC flood control is %s"
	   (cond ((eq erc-flood-protect 'strict) "STRICT")
		 (erc-flood-protect "NORMAL")
		 (t "OFF"))))

(defun erc-toggle-interpret-controls (&optional arg)
  "Toggles interpretation of colors and other control sequences in
messages on and off.  With positive argument, turns it on.  With any
other argument turns it off."
  (interactive "P")
  (cond ((and (numberp arg) (> arg 0))
	 (setq erc-interpret-controls-p t))
	(arg (setq erc-interpret-controls-p nil))
	(t (setq erc-interpret-controls-p (not erc-interpret-controls-p))))
  (message "ERC color interpretation %s"
	   (if erc-interpret-controls-p "ON" "OFF")))

;; Some useful channel and nick commands for fast key bindings

(defun erc-invite-only-mode (&optional arg)
  "Turns on the invite only mode (+i) for the current channel.  If the
non-nil argument is provided, turn this mode off (-i).

This command is sent even if excess flood is detected."
  (interactive "P")
  (setq erc-active-buffer (current-buffer))
  (let ((tgt (erc-default-target))
	(erc-force-send t))
    (cond ((or (not tgt) (not (erc-channel-p tgt)))
	   (erc-display-line (erc-highlight-error "No target\n")
			     (current-buffer)))
	  (arg (erc-load-irc-script-lines (list (concat "/mode " tgt " -i"))
					  t))
	  (t (erc-load-irc-script-lines (list (concat "/mode " tgt " +i"))
					t)))))

(defun erc-insert-mode-command ()
  "Inserts the line \"/mode <current target> \" at the cursor."
  (interactive)
  (let ((tgt (erc-default-target)))
    (if tgt (insert (concat "/mode " tgt " "))
      (erc-display-line (erc-highlight-error "No target\n")
			(current-buffer)))))

(defun erc-channel-names ()
  "Runs \"/names #channel\" in the current channel"
  (interactive)
  (setq erc-active-buffer (current-buffer))
  (let ((tgt (erc-default-target)))
    (if tgt (erc-load-irc-script-lines (list (concat "/names " tgt)))
      (erc-display-line (erc-highlight-error "No target\n")
			(current-buffer)))))

(defun erc-remove-text-properties-region (start end)
  "Clears the region from all the colors, etc."
  (interactive "r")
  (save-excursion
    (let ((s (buffer-substring-no-properties start end)))
      (delete-region start end)
      (insert s))))

;; script execution and startup

(defun erc-find-file (file &optional path)
  "Searches for a FILE as it is (in a current directory) and then
directories in the PATH, if provided, and returns the first full name
found, or NIL if none."
  (let ((filepath file))
    (if (file-readable-p filepath) filepath
      (progn
	(while (and path
		    (progn (setq filepath (concat (car path) "/" file))
			   (not (file-readable-p filepath))))
	  (setq path (cdr path)))
	(if path filepath nil)))))

(defun erc-select-startup-file ()
  "Select startup file with a script to execute. See also
the variable `erc-startup-file-list'"
  (let ((l erc-startup-file-list)
	(f nil))
    (while (and (not f) l)
      (if (file-readable-p (car l))
	  (setq f (car l)))
      (setq l (cdr l)))
    f))

(defun erc-find-script-file (file)
  "Searches for the FILE in the current directory and those provided
in `erc-script-path'."
  (erc-find-file file erc-script-path))

(defun erc-load-script (file)
  "Load a script from FILE.  FILE must be the full name, it is not
 searched in the `erc-script-path'.  If the filename ends with `.el',
 then load it as a emacs-lisp program. Otherwise, trieat it as a
 regular IRC script"
  (erc-log (concat "erc-load-script: " file))
  (cond
   ((string-match "\\.el$" file)
    (load file))
   (t
    (erc-load-irc-script file))))

(defun erc-process-script-line (line &optional args)
  "Does script-specific substitutions (script arguments, current nick,
server, etc.)  in the line and returns it.

Substitutions are: %C and %c = current target (channel or nick),
%S %s = current server, %N %n = my current nick, and %x is x verbatim,
where x is any other character;
$* = the entire argument string, $1 = the first argument, $2 = the second,
end so on."
  (if (not args) (setq args ""))
  (let* ((arg-esc-regexp "\\(\\$\\(\\*\\|[1-9][0-9]*\\)\\)\\([^0-9]\\|$\\)")
	 (percent-regexp "\\(%.\\)")
	 (esc-regexp (concat arg-esc-regexp "\\|" percent-regexp))
	 (tgt (erc-default-target))
	 (server (and (boundp 'erc-session-server) erc-session-server))
	 (nick (erc-current-nick))
	 (res "")
	 (tmp nil)
	 (arg-list nil)
	 (arg-num 0))
    (if (not tgt) (setq tgt ""))
    (if (not server) (setq server ""))
    (if (not nick) (setq nick ""))
    ;; First, compute the argument list
    (setq tmp args)
    (while (string-match "^\\s-*\\(\\S-+\\)\\(\\s-+.*$\\|$\\)" tmp)
      (setq arg-list (cons (match-string 1 tmp) arg-list))
      (setq tmp (match-string 2 tmp)))
    (setq arg-list (nreverse arg-list))
    (setq arg-num (length arg-list))
    ;; now do the substitution
    (setq tmp (string-match esc-regexp line))
    (while tmp
      ;;(message "beginning of while: tmp=%S" tmp)
      (let* ((hd (substring line 0 tmp))
	     (esc "")
	     (subst "")
	     (tail (substring line tmp)))
	(cond ((string-match (concat "^" arg-esc-regexp) tail)
	       (setq esc (match-string 1 tail))
	       (setq tail (substring tail (match-end 1))))
	      ((string-match (concat "^" percent-regexp) tail)
	       (setq esc (match-string 1 tail))
	       (setq tail (substring tail (match-end 1)))))
	;;(message "hd=%S, esc=%S, tail=%S, arg-num=%S" hd esc tail arg-num)
	(setq res (concat res hd))
	(setq subst
	      (cond ((string= esc "") "")
		    ((string-match "^\\$\\*$" esc) args)
		    ((string-match "^\\$\\([0-9]+\\)$" esc)
		     (let ((n (string-to-number (match-string 1 esc))))
		       (message "n = %S, integerp(n)=%S" n (integerp n))
		       (if (<= n arg-num) (nth (1- n) arg-list) "")))
		    ((string-match "^%[Cc]$" esc) tgt)
		    ((string-match "^%[Ss]$" esc) server)
		    ((string-match "^%[Nn]$" esc) nick)
		    ((string-match "^%\\(.\\)$" esc) (match-string 1 esc))
		    (t (erc-log (format "BUG in erc-process-script-line: bad escape sequence: %S\n" esc))
		       (message "BUG IN ERC: esc=%S" esc)
		       "")))
	(setq line tail)
	(setq tmp (string-match esc-regexp line))
	(setq res (concat res subst))
	;;(message "end of while: line=%S, res=%S, tmp=%S" line res tmp)
	))
    (setq res (concat res line))
    res))

(defun erc-load-irc-script (file &optional force)
  "Load IRC script from FILE"
  (erc-log (concat "erc-load-script: " file))
  (let (str)
    (with-temp-buffer
      (insert-file file)
      (setq str (buffer-string)))
    (erc-load-irc-script-lines (erc-split-multiline-safe str) force)))

(defun erc-load-irc-script-lines (lines &optional force noexpand)
  "Load IRC script LINES (a list of strings).  If optional NOEXPAND
argument is non-nil, do not expand script-specific sequenced, process
the lines verbatim.  Use this for multiline user input."
  (let* ((cb (current-buffer))
	 (pnt (point))
	 (s "")
	 (sp (concat (erc-prompt) " "))
	 (args (and (boundp 'erc-script-args) erc-script-args)))
    (if (and args (string-match "^ " args))
	(setq args (substring args 1)))
    ;; prepare the prompt string for echo
    (erc-put-text-property 0 (length (erc-prompt)) 'face 'erc-prompt-face sp)
    (erc-put-text-property (length (erc-prompt)) (length sp)
		       'face 'erc-input-face sp)
    (while lines
      (setq s (car lines))
      (erc-log (concat "erc-load-script: CMD: " s))
      (when (not (string-match "^\\s-*$" s))
	(let ((line (if noexpand s (erc-process-script-line s args))))
	  (if (and (erc-process-input-line line force)
		   erc-script-echo)
	      (progn
	   ;		  (erc-display-prompt cb erc-insert-marker)
		(erc-put-text-property 0 (length line)
				       'face 'erc-input-face line)
		(erc-display-line (concat sp line) cb)))))
      (setq lines (cdr lines)))))

;; authentication

(defun erc-login ()
  "Perform user authentication at the IRC server"
  (erc-log (format "login: nick: %s, user: %s %s %s :%s"
		   (erc-current-nick)
		   (user-login-name)
		   (system-name)
		   erc-session-server
		   erc-session-user-full-name))
  (if password (erc-send-command (format "PASS %s" password))
    (message "Logging without password"))
  (erc-send-command (format "NICK %s" (erc-current-nick)))
  (erc-send-command
   (format "USER %s %s %s :%s"
	   ;; hacked - S.B.
	   (if erc-anonymous-login erc-email-userid (user-login-name))
	   (if erc-anonymous-login "128.129.130.131" (system-name))
	   erc-session-server
	   erc-session-user-full-name)))

;; connection properties' heuristics

(defun erc-determine-parameters (&optional server port nick name)
  "Determine the connection and authentication parameters and
sets the buffer local variables:

- erc-session-server
- erc-session-port
- nick-stk
- erc-session-full-name"
  (setq erc-session-server (erc-compute-server server)
	erc-session-port (or port erc-default-port)
	erc-session-user-full-name (erc-compute-full-name name))
  (erc-push-nick (erc-compute-nick nick)))

(defun erc-compute-server (server)
  "return the IRC server to use using the following order until a non-NIL
one is found:

- argument
- erc-server value
- value of IRCSERVER environment variable
- erc-default-server value"
  (or server
      erc-server
      (getenv "IRCSERVER")
      erc-default-server))

(defun erc-compute-nick (nick)
  "Return the user's nick using the following order until a non-NIL
one is found:

- argument
- erc-nick value
- value of IRCNICK environment variable
- user's login name"
  (or nick
      (if (consp erc-nick) (car erc-nick) erc-nick)
      (getenv "IRCNICK")
      (user-login-name)))


(defun erc-compute-full-name (name)
  "return the user's full name using the following order until a non-NIL
one is found:

- argument
- erc-user-full-name value
- value of IRCNAME environment variable
- user's full name from the system databases"
  (or name
      erc-user-full-name
      (getenv "IRCNAME")
      (if erc-anonymous-login "unknown" nil)
      (user-full-name)))

;; time routines

(defun erc-format-timestamp ()
  "Format the current timestamp into a string, or return the empty
string if erc-timestamp-format is NIL."
  (if erc-timestamp-format
      (let ((ts (format-time-string erc-timestamp-format
				    (current-time))))
	(erc-put-text-property 0 (length ts) 'face 'erc-timestamp-face ts)
	(erc-put-text-property 0 (length ts) 'invisible 'timestamp ts)
	;; N.B. Later use categories instead of this harmless, but
	;; inelegant, hack. -- BPT
	(erc-put-text-property 0 (length ts) 'intangible t ts)
	ts)
    ""))

;; This function is used to munge `buffer-invisibility-spec to an
;; appropriate value. Currently, it only handles timestamps, thus its
;; location. If you add other features which affect invisibility,
;; please modify this function and move it to a more appropriate
;; location.
(defun erc-munge-invisibility-spec ()
  (if erc-hide-timestamps
      (setq buffer-invisibility-spec
	    (if (listp buffer-invisibility-spec)
		(cons 'timestamp buffer-invisibility-spec)
		(list 't 'timestamp)))
      (setq buffer-invisibility-spec
	    (if (listp buffer-invisibility-spec)
		(remove 'timestamp buffer-invisibility-spec)
		(list 't)))))

;; FIXME: undocumented
(defun erc-hide-timestamps ()
  (interactive)
  (setq erc-hide-timestamps t)
  (erc-munge-invisibility-spec))

;; FIXME: undocumented
(defun erc-show-timestamps ()
  (interactive)
  (setq erc-hide-timestamps nil)
  (erc-munge-invisibility-spec))

(defun erc-string-to-emacs-time (string)
  "Convert long number represented by the STRING into the list of
'(high low), compatible with emacs time format."
  (let* ((n (string-to-number (concat string ".0"))))
    (list (truncate (/ n 65536))
	  (truncate (mod n 65536)))))

(defun erc-emacs-time-to-erc-time (tm)
  "Convert Emacs time to a number of seconds since the epoch"
  (+ (* (nth 0 tm) 65536.0) (nth 1 tm)))
;  (round (+ (* (nth 0 tm) 65536.0) (nth 1 tm))))

(defun erc-current-time ()
  "Return current time as a number of seconds since the epoch"
  (erc-emacs-time-to-erc-time (current-time)))

(defun erc-time-diff (t1 t2)
  "Return time difference in seconds between T1 and T2 (T2 >= T1)"
  (- t2 t1))

(defun erc-time-gt (t1 t2)
  "Check whether T1 > T2"
  (> t1 t2))

(defun erc-sec-to-time (ns)
  "Convert seconds to a time string HH:MM.SS"
  (setq ns (truncate ns))
  (format "%02d:%02d.%02d"
	  (/ ns 3600)
	  (/ (% ns 3600) 60)
	  (% ns 60)))

;; info

(defconst erc-clientinfo-alist
  '(("ACTION" . "is used to inform about one's current activity")
    ("CLIENTINFO" . "gives help on CTCP commands supported by client")
    ("ECHO" . "echoes its arguments back")
    ("FINGER" . "shows user's name, location, and idle time")
    ("PING" . "measures delay between peers")
    ("TIME" . "shows client-side time")
    ("USERINFO" . "shows information provided by a user")
    ("VERSION" . "shows client type and version"))
  "Alist of CTCP CLIENTINFO for ERC commands")

(defun erc-client-info (s)
  "Return CTCP CLIENTINFO on command S. Is S is NIL or an empty string
then return general CLIENTINFO"

  (if (or (not s) (string= s ""))
      (concat
       (apply #'concat
	      (mapcar (lambda (e)
			(concat (car e) " "))
		      erc-clientinfo-alist))
       ": use CLIENTINFO <COMMAND> to get more specific information")
    (let ((h (assoc s erc-clientinfo-alist)))
      (if h
	  (concat s " " (cdr h))
	(concat s ": unknown command")))))

;; Hook functions

(defun erc-directory-writable-p (dir)
  "Determines whether the DIR is a writable directory.
  At this point, only determines whether it exists and is a directory.
  The rest is to be implemented."
  (let ((attr (file-attributes dir)))
    (and attr ;; it exists
	 (nth 0 attr) ;; it is a dir. or a sym. link
	 )))

(defun erc-truncate-buffer-to-size (size &optional buffer)
  "Truncates the buffer to the size SIZE, leaving the bottom portion, if
it is bigger than SIZE+512 characters.  If BUFFER is not provided,
the current buffer is assumed.

If `erc-log-channels' is non-nil and `erc-log-channels-directory' is
a valid directory with write access, then append the cut portion of
the buffer in the appropriate log file."
  (let ((ob (current-buffer)))
    (if (and buffer (get-buffer buffer))
	(set-buffer (get-buffer buffer)))
    (when (and
	   erc-truncate-buffer-on-save
	   (> (point-max) (+ size 512)))
      (buffer-disable-undo)
      (if (and erc-log-channels
	       erc-log-channels-directory
	       (erc-directory-writable-p erc-log-channels-directory))
	  (append-to-file 1 (- (point-max) size)
			  (concat erc-log-channels-directory
				  "/" (buffer-name) ".txt")))
      (delete-region 1 (- (point-max) size))
      (buffer-enable-undo))
    (set-buffer ob)))

(defun erc-generate-log-file-name-short (buffer target nick server port)
  "This function computes a short log file name. In fact, it only
uses the buffer name of the BUFFER argument, so you can affect that using
`rename-buffer' and the-like."
  (concat erc-log-channels-directory "/" (buffer-name buffer) ".txt"))

(defun erc-generate-log-file-name-long (buffer target nick server port)
  "Generates a log-file name in the way ERC always did it.
#channel!nick@server:port.txt)"
  (concat erc-log-channels-directory "/" (if target (concat target "!"))
	  nick "@" server ":" (cond ((stringp port) port)
				    ((numberp port)
				     (number-to-string port))) ".txt"))

(defun erc-save-buffer-in-logs (&optional buffer)
  "When the logs are enabled, that is `erc-log-channels' is non-nil
and `erc-log-channels-directory' is a valid directory, appends the
entire BUFFER contents to the log file.  If BUFFER is not provided,
current buffer is used.

This is normally done on exit, to save the unsaved portion of the
buffer, since only the text that runs off the buffer limit is logged
automatically."
  (interactive)
  (if (not buffer) (setq buffer (current-buffer)))
  (let ((file (funcall erc-generate-log-file-name-function
		       buffer (erc-default-target) (erc-current-nick)
		       erc-session-server erc-session-port)))
    (when buffer
      (with-current-buffer buffer
	(setq buffer-file-name nil)
	(let* ((prompt-length      (length (erc-prompt)))
	       (adjusted-point-max (- (point-max)
				      (if (= 0 prompt-length)
					  0
					(1+ prompt-length)))))
	  (when (and erc-log-channels
		     erc-log-channels-directory
		     (erc-directory-writable-p erc-log-channels-directory))
	    (append-to-file erc-last-saved-position adjusted-point-max
			    file)
	    (if erc-truncate-buffer-on-save
		(progn
		  (erase-buffer)
		  (setq erc-last-saved-position (point-max))
		  (erc-display-prompt))
	      (setq erc-last-saved-position adjusted-point-max))
	    (goto-char (point-max)))
	  (set-buffer-modified-p nil)
	  (setq buffer-file-name file))))))

(defun erc-truncate-buffer ()
  "Truncates the current buffer to `erc-max-buffer-size'.
Meant to be used in hooks, like `erc-insert-post-hook'."
  (interactive)
  (erc-truncate-buffer-to-size erc-max-buffer-size))

(defun erc-save-query-buffers (process)
  "Save all buffers process."
  (erc-with-all-buffers-of-server process
				  nil
				  (erc-save-buffer-in-logs)))

(defun erc-kill-query-buffers (process)
  "Kill all buffers of process"
  ;; here, we only want to match the channel buffers, to avoid
  ;; "selecting killed buffers" b0rkage.
  (erc-with-all-buffers-of-server process
				  (lambda ()
				    (not (erc-server-buffer-p)))
				  (kill-buffer (current-buffer))))

;; Join hook function - use it to run an autogreeting script
; rewrite me, I am wrong. I should be compatible witz erc-server-JOIN-hook instead.
(defun erc-join-autogreet (chnl nick buffer &optional host login spec)
  "Runs an autogreet script provided by `erc-autogreet-script' with
arguments CHNL, NICK, and BUFFER name (all strings).  It is meant to
be used in `erc-join-hook'.

Keep in mind, that autogreets sometimes are considered poor style on
IRC.  Use it when you really need it."
  (save-excursion
    (erc-cmd-LOAD (concat erc-autogreet-script " " chnl
			  " " nick " " buffer))))

(defun erc-nick-at-point ()
  "Give information about nickname at point. If called interactively,
give a human readable message in the minibuffer. If called programatically,
return the corresponding assoc entry of `channel-members'."
  (interactive)
  (require 'thingatpt)
  (let ((nickinfo (assoc (word-at-point) channel-members)))
    (when nickinfo
      (if (interactive-p)
	  (message (format "%s is %s@%s%s%s"
			   (car nickinfo)
			   (nth 4 nickinfo)
			   (nth 3 nickinfo)
			   (if (nth 5 nickinfo)
			       (format " (%s)" (car (nthcdr 5 nickinfo)))
			     "")
			   (if (or (cadr nickinfo) (car (nthcdr 2 nickinfo)))
			       (format " and is +%s%s on %s"
				       (if (cadr nickinfo) "o" "")
				       (if (nth 2 nickinfo) "v" "")
				       (erc-default-target))
			     "")))
	nickinfo))))

(defun erc-operator-p (nick &optional channel)
  "Report if nickname is a channel operator."
  ;; Unfinished yet. This is intended to be a saveguard to prevent
  ;; doing things which require op rights client-side.
  ;; Interactively it could prevent looking in the info buffer or doing
  ;; C-c C-n and looking for the nick.
  (interactive (list (let ((nickinfo (erc-nick-at-point)))
		       (if nickinfo
			   nickinfo
			 (read-from-minibuffer "Nick: ")))
		     (erc-default-target)))
  (let ((result))
    (cond ((and (string= channel (erc-default-target)) (stringp nick))
	   (setq result (car (cdr (assoc nick channel-members)))))
	  ((listp nick)
	   (progn (setq result (car (cdr nick)))
		  (setq nick (car nick))))
	  (t (error "Not implemented")))
    (if (interactive-p)
	(message (format "%s %s operator on %s"
			 nick
			 (if result "is" "is not")
			 channel))
      result)))

;; Mode line handling

(defcustom erc-mode-line-format
  (list (if (boundp 'mode-line-mule-info)
	    mode-line-mule-info
	  "")
	'mode-line-modified
	" " 'target-and/or-server
	" " 'status 'away
	'global-mode-string
	"%[(" 'mode-name 'mode-line-process 'minor-mode-alist "%n" ")%]--"
	'(line-number-mode "L%l--") '(-3 . "%p") "-%-")
  "mode-line-format in erc-mode. This variable is processed through
`erc-prepare-mode-line-format' first."
  :group 'erc
  :type 'sexp)

(defcustom erc-header-line-format
  '("[IRC] " nick " on " target
    " " channel-modes " " topic)
  "Format of the header-line in erc-mode.
Only used in Emacs 21. This variable is processed using
`erc-prepare-mode-line-format'."
  :group 'erc
  :type 'sexp)

(defcustom erc-common-server-suffixes
  '(("openprojects.net$" . "OPN"))
  "Alist of common server name suffixes.
This variable is used in mode-line display to save screen
real estate. Set it to nil if you want to avoid changing
displayed hostnames."
  :group 'erc
  :type 'alist)

(defun erc-shorten-server-name (string)
  (if (stringp string)
      (with-temp-buffer
	(insert string)
	(let ((alist erc-common-server-suffixes))
	  (while alist
	    (goto-char (point-min))
	(if (re-search-forward (caar alist) nil t)
	    (replace-match (cdar alist)))
	(setq alist (cdr alist))))
	(buffer-string))))

(defun erc-prepare-mode-line-format (line)
  "Replace certain symbols in LINE by data acquired from the current
erc-mode buffer. The following symbols are recognized:
'away: String indicating away status or \"\" if you are not away
'channel-modes: The modes of the channel
'nick: The current nick name
'port: The session port
'status: \" (CLOSED) \" in case the process is no longer open/run
'target: The name of the target (channel or nickname or servername:port)
'target-and/or-server: In the server-buffer, this gets filled with the
		       value of erc-announced-server-name,
		       in a channel, the value of (erc-default-target) also
		       get concatenated.
'topic: The topic of the channel"
  (let ((away (when (and (boundp 'erc-process)
			 (processp erc-process))
		(with-current-buffer (process-buffer erc-process)
		  away))))
    (mapcar
     (lambda (sym)
       (cond ((eq sym 'nick)
	      (erc-current-nick))
	     ((eq sym 'target)
	      (let ((target (erc-default-target)))
		(if target
		    target
		  (concat (erc-shorten-server-name
			   (if (boundp 'erc-announced-server-name)
			       erc-announced-server-name
			     erc-session-server))
			  ":" (erc-port-to-string erc-session-port)))))
	     ((eq sym 'port)
	      (erc-port-to-string erc-session-port))
	     ((eq sym 'status)
	      (if (and (boundp 'erc-process) (processp erc-process)
		       (member (process-status erc-process) '(run open)))
		  ""
		" (CLOSED) "))
	     ((eq sym 'channel-modes)
	      (concat (apply 'concat
			     "(+" channel-modes)
		      (if channel-user-limit
			  (format "l %.0f" channel-user-limit) ; Emacs has no BIGNUMs
			"")
		      ")"))
	     ((eq sym 'away)
	      (if away
		  (concat " (AWAY since "
			  (format-time-string "%a %b %d %H:%M" away)
			  ") ")
		""))
	     ((eq sym 'topic)
	      (erc-interpret-controls channel-topic))
	     ((eq sym 'target-and/or-server)
	      (let ((server-name (erc-shorten-server-name
				  (if (boundp 'erc-announced-server-name)
				      erc-announced-server-name
				    erc-session-server))))
		(if (erc-default-target)
		    (concat (erc-default-target) "@" server-name)
		  server-name)))
	     (t
	      sym)))
     line)))

(defun erc-update-mode-line-buffer (buffer)
  "Update the mode line in a single ERC buffer BUFFER"
  (with-current-buffer buffer
    (setq mode-line-format
	  (erc-prepare-mode-line-format erc-mode-line-format))
    (when (boundp 'header-line-format)
      (if erc-header-line-format
	  (setq header-line-format
		(erc-prepare-mode-line-format erc-header-line-format))
	(setq header-line-format nil)))))

(defun erc-update-mode-line (&optional buffer)
  "Update the mode line in one buffer BUFFER, if it is supplied, or
all ERC buffers otherwise."
  (if (and buffer (bufferp buffer))
      (erc-update-mode-line-buffer buffer)
    (dolist (buf (erc-buffer-list))
      (when (buffer-live-p buf)
	(erc-update-mode-line-buffer buf)))))

;; Miscellaneous

(defun erc-port-to-string (p)
  "Convert port P to string. P may be an integer or a service name"
  (if (integerp p)
      (int-to-string p)
    p))

(defun erc-string-to-port (s)
  "Convert string S to either integer port number or a service name"
  (let ((n (string-to-number s)))
    (if (= n 0)
	s
      n)))

(defun erc-version (&optional here)
  "Show the version number of ERC in the minibuffer.
If optional argument HERE is non-nil, insert version number at point."
  (interactive "P")
  (let ((version-string
	 (format "ERC version %s" erc-version-string)))
    (if here
	(insert version-string)
      (if (interactive-p)
	  (message "%s" version-string)
	version-string))))

(defun erc-latest-version ()
  "Retrieves the latest erc.el version from CVS."
  (interactive)
  (if (and (require 'url) (require 'w3))
      (progn
	(switch-to-buffer (get-buffer-create "*erc.el latest version*"))
	(delete-region (point-min) (point-max))
	(kill-all-local-variables)
	(url-insert-file-contents "http://cvs.sf.net/cgi-bin/viewcvs.cgi/~checkout~/erc/erc/erc.el?rev=HEAD&content-type=text/plain")
	(emacs-lisp-mode)
	(current-buffer))
    (error "W3 needs to be installed")))

(defun erc-ediff-latest-version ()
  "Brings up a ediff of your currently installed erc.el version compared
to the latest version in CVS. See `erc-latest-version'."
  (interactive)
  (ediff-buffers (progn
		   (find-file (locate-library "erc"))
		   (current-buffer))
		 (erc-latest-version)))

(defun erc-trim-string (s)
  "Trim leading and trailing spaces off the string"
  (cond
   ((not (stringp s)) nil)
   ((string-match "^\\s-*$" s)
    "")
   ((string-match "^\\s-*\\(.*\\S-\\)\\s-*$" s)
    (match-string 1 s))
   (t
    s)))

(defun erc-arrange-session-in-multiple-windows ()
  "Opens a separate window for every channel/query buffer related to
to `erc-session-server' in the current frame."
  (interactive)
  (when (not (boundp 'erc-process))
    (error "No erc-process found in current buffer"))
  (let ((bufs (erc-buffer-list nil erc-process)))
    (when bufs
      (delete-other-windows)
      (switch-to-buffer (car bufs))
      (setq bufs (cdr bufs))
      (while bufs
	(split-window)
	(switch-to-buffer-other-window (car bufs))
	(setq bufs (cdr bufs))
	(balance-windows)))))

(defun erc-popup-input-buffer ()
  "Provide a input buffer."
   (interactive)
   (let ((buffer-name (generate-new-buffer-name "*ERC input*"))
	 (mode (intern
		(completing-read
		 "Mode: "
		 (mapcar (lambda (e)
			   (list (symbol-name e)))
			 (apropos-internal "-mode$" 'commandp))
		 nil t))))
     (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
     (funcall mode)
     (narrow-to-region (point) (point))
     (shrink-window-if-larger-than-buffer)))

;;; Message catalog

(defun erc-make-message-variable-name (catalog entry)
  (intern (concat "erc-message-"
		  (symbol-name catalog) "-" (symbol-name entry))))

(defun erc-define-catalog-entry (catalog entry format-spec)
  (set (erc-make-message-variable-name catalog entry)
       format-spec))

(defun erc-define-catalog (catalog entries)
  (dolist (entry entries)
    (erc-define-catalog-entry catalog (car entry) (cdr entry))))

(erc-define-catalog
 'english
 '((connect . "Connecting to %S:%p... ")
   (nick-in-use . "%n is in use. Choose new nickname: ")
   (login . "Logging in as \'%n\'...")
   (ACTION . "* %n %a")
   (INVITE . "%n (%u@%h) invites you to channel %c")
   (JOIN   . "%n (%u@%h) has joined channel %c")
 (JOIN-you . "You have joined channel %c")
   (MODE   . "%n (%u@%h) has changed mode for %t to %m")
   (NICK   . "%n (%u@%h) is now known as %N")
 (NICK-you . "Your new nickname is %N")
   (PART   . erc-message-english-PART)
   (QUIT   . "%n (%u@%h) has quit: %r")
   (TOPIC  . "%n (%u@%h) has set the topic for %c: \"%T\"")
   (s004   . "%s %v %U %C")
   (s221   . "User modes for %n: %m")
   (s301   . "%n is AWAY: %r")
   (s379   . "%c: Forwarded to %f")))

(defun erc-message-english-PART (&rest args)
  "Format a proper PART message. This function is an example on
what could be done with formatting functions."
  (let ((nick (cadr (member ?n args)))
	(user (cadr (member ?u args)))
	(host (cadr (member ?h args)))
	(channel (cadr (member ?c args)))
	(reason (cadr (member ?r args))))
    (if (string= nick (erc-current-nick))
	(format "You have left channel %s" channel)
      (format "%s (%s@%s) has left channel %s%s"
	      nick user host channel
	      (if (not (string= reason ""))
		  (format ": %s" reason)
		"")))))


(defvar erc-current-message-catalog 'english)
(make-variable-buffer-local 'erc-current-message-catalog)
(defun erc-retrieve-catalog-entry (entry &optional catalog)
  (unless catalog (setq catalog erc-current-message-catalog))
  (let ((var (erc-make-message-variable-name catalog entry)))
    (if (boundp var)
	(symbol-value var)
      (when (boundp (erc-make-message-variable-name 'english entry))
	(symbol-value (erc-make-message-variable-name 'english entry))))))

(defun erc-format-message (msg &rest args)
  (when (oddp (length args))
    (error "Obscure usage of this function appeared"))
  (let ((entry (erc-retrieve-catalog-entry msg)))
    (when (functionp entry)
      (setq entry (apply entry args)))
    (format-spec entry (apply 'format-spec-make args))))

;;; Various hook functions

(defun erc-make-read-only ()
  "Makes freshly inserted text read-only to avoid accidental editing.
Put this function on `erc-insert-post-hook' and/or `erc-send-post-hook'."
  (put-text-property (point-min) (point-max) 'read-only t)
  (put-text-property (point-min) (point-max) 'front-nonsticky t))



;; Imenu Autoload

(add-hook 'erc-mode-hook
	  (lambda ()
	    (setq imenu-create-index-function 'erc-create-imenu-index)))
(autoload 'erc-create-imenu-index "erc-imenu" "Imenu index creation function")

(provide 'erc)

;;; erc.el ends here
;;
;; Local Variables:
;; mode: outline-minor
;; outline-regexp: ";;+"
;; indent-tabs-mode: t
;; End:
