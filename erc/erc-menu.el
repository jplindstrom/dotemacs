;; erc-menu.el -- Menu-bar definitions for the Emacs IRC Client

;; Author: Mario Lang <mlang@delysid.org>

;; This file is not part of GNU Emacs, but the same license applies.

(require 'erc)
(require 'easymenu)

(defvar erc-menu-definition
  (list "IRC"
	["Connect to server" erc-select t]
	"-"
	["Join channel" erc-join-channel t]
	["Leave channel" erc-part-from-channel t]
	["Set topic" erc-set-topic t]
	["List users" erc-channel-names t]
	["Change mode" erc-insert-mode-command t]
	["Invite only mode" erc-invite-only-mode
	 :style toggle :selected (member "i" channel-modes)]
	"-"
	["Add pal" erc-add-pal]
	["Delete pal" erc-delete-pal]
	["Add fool" erc-add-fool]
	["Delete fool" erc-delete-fool]
	["Add keyword" erc-add-keyword]
	["Delete keyword" erc-delete-keyword]
	["Add dangerous host" erc-add-dangerous-host]
	["Delete dangerous host" erc-delete-dangerous-host]
	"-"
	["Input action" erc-input-action (erc-default-target)]
	(list "IRC services..."
	      ["Identify to NickServ" erc-nickserv-identify])
	"-"
	["Track hidden channel buffers" erc-track-modified-channels-mode
	 :style toggle :selected erc-track-modified-channels-mode]
	)
  "ERC menu definition.")

(easy-menu-define erc-menu erc-mode-map "ERC menu" erc-menu-definition)
(easy-menu-add erc-menu erc-mode-map)

(provide 'erc-menu)
