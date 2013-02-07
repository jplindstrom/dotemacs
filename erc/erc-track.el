;;; erc-track.el --- Track modified channel buffers

;;; Commentary:
;;

;; Todo:
;; * Add extensibility so that custom functions can track
;;   custom modification types.

(require 'erc)

;;; Code:

(defcustom erc-track-exclude nil
  "A list targets (channel names or query targets) which should not be tracked."
  :group 'erc
  :type '(repeat string))

(defcustom erc-track-shorten-start 1
  "This number specifies the minimum number of characters a channel name in
the mode-line should be reduced to."
  :group 'erc
  :type 'number)

(defcustom erc-track-shorten-cutoff 4
  "All channel names longer than this value will be shortened."
  :group 'erc
  :type 'number)

(defcustom erc-track-shorten-aggressively nil
  "*If non-nil, channel names will be shortened more aggresively."
  :group 'erc
  :type 'boolean)

(defcustom erc-track-shorten-function 'erc-track-shorten-names
  "*This function will be used to reduce the channel names before display.
It takes one arguemtn, CHANNEL-NAMES which is a list of strings.
It should return a list of strings of the same number of elements."
  :group 'erc
  :type 'function)

(defcustom erc-track-use-faces t
  "*If non-nil, use faces to indicate pal/fool/keyword/dangerou host activities in the mode-line.
The faces used are the same as used for text in the buffers.
\(e.g. `erc-pal-face' is used if a pal sent a message to that channel.)"
  :group 'erc
  :type 'boolean)

(defvar erc-modified-channels-string ""
  "Internal string used for displaying modified channels in the mode line.")

(defvar erc-modified-channels-alist nil
  "An ALIST used for tracking channel modification activity. Each element
looks like (BUFFER HIGHLIGHT-FLAG...) where
BUFFER is a buffer object of the channel the entry corresponds to and
HIGHLIGHT-FLAG is one of
  'pal - Text contained `erc-pal-face'
  'fool - Text contained `erc-fool-face'
  'keyword - Text contained `erc-keyword-face'
  'dangerous-host - Text contained `erc-dangerous-hosts-face'

Entries in this list should only happen for buffers where activity occured
while the buffer was not visible.")

;;; Utility functions

(defun unique-substrings (strings &optional predicate start)
  "Return a list of unique substrings of STRINGS."
  (if (or (not (numberp start))
	  (< start 0))
      (setq start 0))
  (mapcar
   (lambda (str)
     (let* ((others (delete str (copy-sequence strings)))
	    (maxlen (length str))
	    (i start)
	    candidate
	    done)
       (if (and (functionp predicate) (not (funcall predicate str)))
	   str
	 ;; Start with smallest substring candidate, ie. length 1.
	 ;; Then check all the others and see wether any of them starts
	 ;; with the same substring.  While there is such another
	 ;; element in the list, increase the length of the candidate.
	 (while (not done)
	   (setq i (1+ i))
	   (if (> i maxlen)
	       (setq done t)
	     (setq candidate (substring str 0 i)
		   done (not (unique-substring-1 candidate others)))))
	 (if (and (= (length candidate) (1- maxlen))
		  (not erc-track-shorten-aggressively))
	     str
	   candidate))))
   strings))

(defun unique-substring-1 (candidate others)
  "Return non-nil when any string in OTHERS starts with CANDIDATE."
  (let (result other (maxlen (length candidate)))
    (while (and others
		(not result))
      (setq other (car others)
	    others (cdr others))
      (when (and (>= (length other) maxlen)
		 (string= candidate (substring other 0 maxlen)))
	(setq result other)))
    result))

(let ((erc-track-shorten-aggressively t))
  (assert
   (and (equal (unique-substring-1 "abc" '("ab" "abcd")) "abcd")
	(not (unique-substring-1 "a" '("xyz" "xab")))
	(equal (unique-substrings '("abc" "xyz" "xab")) '("a" "xy" "xa"))
	(equal (unique-substrings '("abc" "abcdefg")) '("abc" "abcd")))))


(defun erc-make-mode-line-buffer-name (string buffer &optional actions)
  (let ((map (make-sparse-keymap))
	(name string))
    (define-key map (vector 'mode-line 'mouse-2)
      `(lambda (e)
	 (interactive "e")
	 (save-selected-window
	   (select-window
	    (posn-window (event-start e)))
	   (switch-to-buffer ,buffer))))
    (put-text-property 0 (length name) 'local-map map name)
    (when (and (car actions) erc-track-use-faces)
      (let ((facesym (intern (concat "erc-" (symbol-name (car actions)) "-face"))))
	(if (facep facesym)
	    (put-text-property 0 (length name) 'face facesym name))))
    name))

(if (fboundp 'define-minor-mode)
    (define-minor-mode erc-track-modified-channels-mode
      "Global minor mode for tracking erc channel buffers with activity.
Use (erc-track-modified-channels-mode t) to activate it."
  nil nil nil
  :global t
  :group 'erc
  (if erc-track-modified-channels-mode
      (progn
	(or global-mode-string
	    (setq global-mode-string '("")))
	(or (memq 'erc-modified-channels-string global-mode-string)
	    (setq global-mode-string
		  (append global-mode-string '(erc-modified-channels-string))))
	(setq erc-modified-channels-string "")
	(erc-update-mode-line)
	(add-hook 'erc-insert-post-hook 'erc-track-modified-channels)
	(add-hook 'window-configuration-change-hook 'erc-modified-channels-update))
    (setq global-mode-string
	  (delq 'erc-modified-channels-string global-mode-string))
    (remove-hook 'erc-insert-post-hook 'erc-track-modified-channels)
    (remove-hook 'window-configuration-change-hook 'erc-modified-channels-update)))
  (when (fboundp 'add-minor-mode) ;; Emacs20 doesnt have channel tracking atm
				  ;; Anyone care to fix that?
    ;; Compatibility code for XEmacs, generated using macroexpand on the above.
    ;; FIXME, regenerate me, I am out of date!
    (defcustom erc-track-modified-channels-mode nil
      "Non-nil if Erc-Track-Modified-Channels minor mode is enabled.
See the command `erc-track-modified-channels-mode' for a description of this minor-mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `erc-track-modified-channels-mode'."
      :set (lambda (symbol value)
	     (funcall symbol (or value 0)))
      :initialize 'custom-initialize-default
      :group 'erc
      :type 'boolean)
    (defun erc-track-modified-channels-mode (&optional arg)
      "Global minor mode for tracking erc channel buffers with activity.
Use (erc-track-modified-channels-mode t) to activate it."
      (interactive
       (list (or current-prefix-arg
		 (if erc-track-modified-channels-mode 0 1))))
      (setq erc-track-modified-channels-mode
	    (if arg
		(> (prefix-numeric-value arg) 0)
	      (not erc-track-modified-channels-mode)))
      (if erc-track-modified-channels-mode
	  (progn
	    (or global-mode-string
		(setq global-mode-string '("")))
	    (or (memq 'erc-modified-channels-string global-mode-string)
		(setq global-mode-string
		      (append global-mode-string
			      '(erc-modified-channels-string))))
	    (setq erc-modified-channels-string "")
	    (add-hook 'erc-insert-post-hook 'erc-track-modified-channels)
	    (erc-update-mode-line)
	    (ad-enable-advice 'switch-to-buffer 'after 'erc-remove-channel-from-modded-list)
	    (ad-activate 'switch-to-buffer))
	(setq global-mode-string
	      (delq 'erc-modified-channels-string global-mode-string))
	(remove-hook 'erc-insert-post-hook 'erc-track-modified-channels)
	(ad-disable-advice 'switch-to-buffer 'after 'erc-remove-channel-from-modded-list)
	(ad-activate 'switch-to-buffer))
      (run-hooks 'erc-track-modified-channels-mode-hook
		 (if erc-track-modified-channels-mode
		     'erc-track-modified-channels-mode-on-hook
		   'erc-track-modified-channels-mode-off-hook))
      (if
	  (interactive-p)
	  (message "Erc-Track-Modified-Channels minor mode %sabled"
		   (if erc-track-modified-channels-mode "en" "dis")))
      (force-mode-line-update)
      erc-track-modified-channels-mode)
    :autoload-end
    (defcustom erc-track-modified-channels-mode-hook nil
      "Hook run at the end of function `erc-track-modified-channels-mode'."
      :group 'erc
      :type 'hook)

    (add-minor-mode 'erc-track-modified-channels-mode 'nil
		    (if (boundp 'erc-track-modified-channels-mode-map)
			(symbol-value 'erc-track-modified-channels-mode-map)))
    (if (and load-file-name erc-track-modified-channels-mode)
	(eval-after-load load-file-name
	  '(erc-track-modified-channels-mode 1)))))

(defun erc-modified-channels-update ()
  "This function updates the information in `erc-modified-channels-alist'
according to buffer visibility. It calls `erc-modified-channels-display' at the
end. This should usually be called via `window-configuration-change-hook'."
  (interactive)
  (mapcar (lambda (elt)
	    (let ((buffer (car elt)))
	      (when (or (not (bufferp buffer))
			(not (buffer-live-p buffer))
			(get-buffer-window buffer t))
		(erc-modified-channels-remove-buffer buffer))))
	  erc-modified-channels-alist)
  (erc-modified-channels-display))

(defun erc-track-shorten-names (channel-names)
  (unique-substrings
   channel-names
   (lambda (s)
     (and (erc-channel-p s)
	  (> (length s) erc-track-shorten-cutoff)))
   erc-track-shorten-start))

(defun erc-modified-channels-display ()
  (if (null erc-modified-channels-alist)
      (setq erc-modified-channels-string "")
    (let* ((bufnames (mapcar (lambda (x) (buffer-name (car x)))
			    erc-modified-channels-alist))
	   (tmplist (if (functionp erc-track-shorten-function)
			(funcall erc-track-shorten-function bufnames)
		      bufnames))
	   (alist erc-modified-channels-alist)
	   new-list)
      (while alist
	(setq new-list (cons (append (list (car tmplist))
				     (car alist))
			     new-list))
	(setq alist (cdr alist) tmplist (cdr tmplist)))
      (setq new-list (nreverse new-list))
      (setq erc-modified-channels-string
	    (concat "["
		    (mapconcat (lambda (elt)
				 (erc-make-mode-line-buffer-name
				  (car elt) (nth 1 elt) (nthcdr 2 elt)))
			       new-list ",")
		    "] ")))))

(defun erc-modified-channels-remove-buffer (buffer)
  "Remove BUFFER from `erc-modified-channels-alist'."
  (interactive "bBuffer: ")
  (setq erc-modified-channels-alist
	(delete (assq buffer erc-modified-channels-alist)
		erc-modified-channels-alist))
  (when (interactive-p)
    (erc-modified-channels-display)))

(defun erc-track-modified-channels ()
  "Hook function for `erc-insert-post-hook' to check if the current
buffer should be added to the modeline as a hidden, modified
channel.  Assumes it will only be called when current-buffer
is in `erc-mode'."
  (let ((this-channel (erc-default-target)))
    (if (and (not (get-buffer-window (current-buffer) t))
	     this-channel
	     (not (member this-channel erc-track-exclude)))
	(let (props)
	  (when (text-property-any (point-min) (point-max) 'face 'erc-pal-face)
	    (push 'pal props))
	  (when (text-property-any (point-min) (point-max) 'face 'erc-keyword-face)
	    (push 'keyword props))
	  (when (text-property-any (point-min) (point-max) 'face 'erc-fool-face)
	    (push 'fool props))
	  (when (text-property-any (point-min) (point-max) 'face 'erc-dangerous-host-face)
	    (push 'dangerous-host props))

	  (if (not (assq (current-buffer) erc-modified-channels-alist))
	      ;; only do this work if the buffer isn't visible and
	      ;; isn't already in our list
	      (progn
		(add-to-list 'erc-modified-channels-alist
			     (append (list (current-buffer))
				     props))
		(erc-modified-channels-display))
	    ;; See if we need to add a highlight type
	    (when props
	      (let* ((cell (assq (current-buffer) erc-modified-channels-alist))
		     (value (cdr cell)))
		(dolist (elt props)
		  (add-to-list 'value elt))
		(setcdr cell value)
		(erc-modified-channels-display)))))
      (when (or (get-buffer-window (current-buffer) t)
		(and this-channel
		     (assq (current-buffer) erc-modified-channels-alist)
		     (member this-channel erc-track-exclude)))
	;; Remove it from mode-line if buffer is visible or
	;; channel was added to erc-track-exclude recently.
	(erc-modified-channels-remove-buffer (current-buffer))
	(erc-modified-channels-display)))))

(provide 'erc-track)

;;; erc-track.el ends here
