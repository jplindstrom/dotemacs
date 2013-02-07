;;; Commentary:
;;
;; This file contains code related to Ibuffer and ERC.
;; (draft only, dont use seriously!)
;;
;;; Usage:
;;
;; Type / C-e C-h when in Ibuffer-mode to see limiting commands related to
;; erc-ibuffer.el.
;;
;;; Code:

(require 'ibuffer)

(defgroup erc-ibuffer nil
  "The Ibuffer group for ERC."
  :group 'erc)

(defcustom erc-ibuffer-keyword-char ?k
  "Char used to indicate a channel which had keyword traffic lately (hidden)."
  :group 'erc-ibuffer
  :type 'character)
(defcustom erc-ibuffer-pal-char ?p
  "Char used to indicate a channel which had pal traffic lately (hidden)."
  :group 'erc-ibuffer
  :type 'character)
(defcustom erc-ibuffer-fool-char ?f
  "Char used to indicate a channel which had fool traffic lately (hidden)."
  :group 'erc-ibuffer
  :type 'character)
(defcustom erc-ibuffer-dangerous-host-char ?d
  "Char used to indicate a channel which had dangerous-host traffic lately (hidden)."
  :group 'erc-ibuffer
  :type 'character)

(ibuffer-define-limiter erc-server
  (:documentation
   "Toggle current view to buffers which are related to ERC servers."
   :description "erc servers"
   :reader
   (let ((regexp (read-from-minibuffer "Limit by server (regexp) (RET for all): ")))
     (if (string= regexp "")
	 ".*"
       regexp)))
  (with-current-buffer buf
    (and (eq major-mode 'erc-mode)
	 (boundp 'erc-announced-server-name)
	 (string-match qualifier erc-announced-server-name))))

(ibuffer-define-column erc-modified (:name "M")
  (if (and (boundp 'erc-track-modified-channels-mode)
	   erc-track-modified-channels-mode)
      (let ((entry (assq (current-buffer) erc-modified-channels-alist)))
	(if entry
	    (if (> (length entry) 1)
		(cond ((eq 'pal (nth 1 entry))
		       (string erc-ibuffer-pal-char))
		      ((eq 'fool (nth 1 entry))
		       (string erc-ibuffer-fool-char))
		      ((eq 'keyword (nth 1 entry))
		       (string erc-ibuffer-keyword-char))
		      ((eq 'dangerous-host (nth 1 entry))
		       (string erc-ibuffer-dangerous-host-char))
		      (t "$"))
	      (string ibuffer-modified-char))
	  " "))
    " "))

(ibuffer-define-column erc-server-name (:name "Server")
  (if (and (boundp 'erc-process) (processp erc-process))
      (with-current-buffer (process-buffer erc-process)
	erc-announced-server-name)
    ""))

(ibuffer-define-column erc-target (:name "Target")
  (if (or (eq major-mode 'erc-mode) (eq major-mode 'erc-info-mode))
      (cond ((and (boundp 'erc-process) (processp erc-process)
		  (eq (current-buffer) (process-buffer erc-process)))
	     (concat "Server " erc-session-server ":"
		     (erc-port-to-string erc-session-port)))
	    ((erc-channel-p (erc-default-target))
	     (concat (erc-default-target)))
	    ((eq major-mode 'erc-info-mode)
	     "INFO")
	    ((erc-default-target)
	     (concat "Query: " (erc-default-target)))
	    (t "(parted)"))
    (buffer-name)))

(ibuffer-define-column erc-topic (:name "Topic")
  (if (and (eq major-mode 'erc-mode)
	   (boundp 'channel-topic))
      (erc-interpret-controls channel-topic)
    ""))

(ibuffer-define-column erc-members (:name "Users")
  (if (and (eq major-mode 'erc-mode)
	   (boundp 'channel-members)
	   (> (length channel-members) 0))
      (number-to-string (length channel-members))
    ""))

(ibuffer-define-column erc-away (:name "A")
  (if (and (boundp 'erc-process)
	   (processp erc-process)
	   (with-current-buffer (process-buffer erc-process)
	     away))
      "A"
    " "))

(ibuffer-define-column erc-op (:name "O")
  (if (and (eq major-mode 'erc-mode)
	   (boundp 'channel-members)
	   (nth 1 (assoc (erc-current-nick) channel-members)))
      "@"
    " "))

(ibuffer-define-column erc-voice (:name "V")
  (if (and (eq major-mode 'erc-mode)
	   (boundp 'channel-members)
	   (nth 2 (assoc (erc-current-nick) channel-members)))
      "+"
    " "))

(ibuffer-define-column erc-channel-modes (:name "Mode")
  (if (and (eq major-mode 'erc-mode)
	   (or (> (length channel-modes) 0)
	       channel-user-limit))
      (concat (apply 'concat
		     "(+" channel-modes)
	      (if channel-user-limit
		  (format "l %d" channel-user-limit)
		"")
	      ")")
    (if (not (eq major-mode 'erc-mode))
	mode-name
      "")))

(ibuffer-define-column erc-nick (:name "Nick")
  (if (eq major-mode 'erc-mode)
      (erc-current-nick)
    ""))

(defvar erc-ibuffer-formats '((mark erc-modified erc-away erc-op erc-voice " " (erc-nick 8 8) " " (erc-target 18 40) (erc-members 5 5 :center) (erc-channel-modes 6 16 :center) " " (erc-server-name 20 30) " " (erc-topic 10 -1))
			      (mark erc-modified erc-away erc-op erc-voice " " (erc-target 18 40) (erc-members 5 5 :center) (erc-channel-modes 9 20 :center) " " (erc-topic 10 -1))))
(setq ibuffer-formats (append ibuffer-formats erc-ibuffer-formats))

(defvar erc-ibuffer-limit-map nil
  "Prefix keymap to use for ERC related limiting.")
(define-prefix-command 'erc-ibuffer-limit-map)
(define-key 'erc-ibuffer-limit-map (kbd "s") 'ibuffer-limit-by-erc-server)
(define-key ibuffer-mode-map (kbd "/ \C-e") 'erc-ibuffer-limit-map)

(provide 'erc-ibuffer)
