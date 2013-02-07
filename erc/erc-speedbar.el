;; TODO / ideas:
;;
;; * Write intelligent update function:
;;   update-channel, update-nick, remove-nick-from-channel, ...
;; * Use indicator-strings for op/voice
;; * Extract/convert face notes field from bbdb if available and show it using sb-image.el
;;

(defvar erc-speedbar-key-map nil
  "Keymap used when in the erc display mode.")

(defun erc-install-speedbar-variables ()
  "Install those variables used by speedbar to enhance ERC."
  (if erc-speedbar-key-map
      nil
    (setq erc-speedbar-key-map (speedbar-make-specialized-keymap))

    ;; Basic tree features
    (define-key erc-speedbar-key-map "e" 'speedbar-edit-line)
    (define-key erc-speedbar-key-map "\C-m" 'speedbar-edit-line)
    (define-key erc-speedbar-key-map "+" 'speedbar-expand-line)
    (define-key erc-speedbar-key-map "=" 'speedbar-expand-line)
    (define-key erc-speedbar-key-map "-" 'speedbar-contract-line)
    )

  (speedbar-add-expansion-list '("ERC" erc-speedbar-menu-items
				 erc-speedbar-key-map
				 erc-speedbar-server-buttons)))

(defvar erc-speedbar-menu-items
  '(["Goto buffer" speedbar-edit-line t]
    ["Expand Node" speedbar-expand-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.\\+. "))]
    ["Contract Node" speedbar-contract-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.-. "))]
    )
  "Additional menu-items to add to speedbar frame.")

;; Make sure our special speedbar major mode is loaded
(if (featurep 'speedbar)
    (erc-install-speedbar-variables)
  (add-hook 'speedbar-load-hook 'erc-install-speedbar-variables))

;;; ERC hierarchy display method
;;;###autoload
(defun erc-speedbar-browser ()
  "Initialize speedbar to display an ERC browser.
This will add a speedbar major display mode."
  (interactive)
  (require 'speedbar)
  ;; Make sure that speedbar is active
  (speedbar-frame-mode 1)
  ;; Now, throw us into Info mode on speedbar.
  (speedbar-change-initial-expansion-list "ERC")
  (speedbar-get-focus))

(defun erc-speedbar-buttons (buffer)
  (let (serverp chanp)
    (with-current-buffer buffer
      (setq serverp (eq buffer (process-buffer erc-process)))
      (setq chanp (erc-channel-p (erc-default-target))))
    (cond (serverp
	   (erc-speedbar-channel-buttons nil 0 buffer))
	  (chanp
	   (erc-speedbar-insert-target buffer 0)
	   (forward-line -1)
	   (erc-speedbar-expand-channel "+" buffer 0))
	  (t (error "Not sure, this should never happen.")))))


(defun erc-speedbar-server-buttons (directory depth)
  "Insert the initial list of servers you are connected to."
  (let ((servers (erc-buffer-list (lambda ()
				    (eq (current-buffer)
					(process-buffer erc-process))))))
    (when servers
      (speedbar-with-writable
	(dolist (server servers)
	  (speedbar-make-tag-line
	   'bracket ?+ 'erc-speedbar-expand-server server
	   (buffer-name server) 'erc-speedbar-goto-buffer server nil
	   depth))
	t))))

(defun erc-speedbar-expand-server (text server indent)
  (cond ((string-match "+" text)
	 (speedbar-change-expand-button-char ?-)
	 (if (speedbar-with-writable
	       (save-excursion
		 (end-of-line) (forward-char 1)
		 (erc-speedbar-channel-buttons nil (1+ indent) server)))
	     (speedbar-change-expand-button-char ?-)
	   (speedbar-change-expand-button-char ??)))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops... not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun erc-speedbar-channel-buttons (directory depth server-buffer)
  (when (get-buffer server-buffer)
    (let* ((proc (with-current-buffer server-buffer erc-process))
	   (targets (erc-buffer-list (lambda ()
				       (not (eq (process-buffer erc-process)
						(current-buffer))))
				     proc)))
      (when targets
	(speedbar-with-writable
	  (dolist (target targets)
	    (erc-speedbar-insert-target target depth))
	  t)))))

(defun erc-speedbar-insert-target (buffer depth)
  (if (with-current-buffer buffer
	(erc-channel-p (erc-default-target)))
      (speedbar-make-tag-line
       'bracket ?+ 'erc-speedbar-expand-channel buffer
       (buffer-name buffer) 'erc-speedbar-goto-buffer buffer nil
       depth)
    ;; Query target
    (speedbar-make-tag-line
     nil nil nil nil
     (buffer-name buffer) 'erc-speedbar-goto-buffer buffer nil
     depth)))

(defun erc-speedbar-expand-channel (text channel indent)
  (cond ((string-match "+" text)
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	  (save-excursion
	    (end-of-line) (forward-char 1)
	    (let ((modes (with-current-buffer channel
			   (mapconcat 'identity channel-modes "")))
		  (topic (erc-interpret-controls
			  (with-current-buffer channel channel-topic))))
	      (speedbar-make-tag-line
		 nil nil nil nil
		 (concat "Modes: +" modes) nil nil nil
		 (1+ indent))
	      (when (not (string= topic ""))
		(speedbar-make-tag-line
		 nil nil nil nil
		 (concat "Topic: " topic) nil nil nil
		 (1+ indent)))
	      (let ((names (with-current-buffer channel channel-members)))
		(when names
		  (speedbar-with-writable
		   (dolist (entry names)
		     (erc-speedbar-insert-user entry ?+ (1+ indent)))))))))
	 )
	((string-match "-" text)
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops... not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun erc-speedbar-insert-user (entry exp-char indent)
  (let* ((op (nth 1 entry))
	 (voice (nth 2 entry))
	 (nick (nth 0 entry))
	 (nick-str (concat (if op "@" "") (if voice "+" "") nick))
	 (host (nth 3 entry))
	 (user (nth 4 entry))
	 (finger (concat user (when (or user host) "@") host))
	 (name (nth 5 entry))
	 (info (nth 6 entry)))
    (if (or user host name info) ; we want to be expandable
	(progn
	  (speedbar-make-tag-line
	   'bracket ?+ 'erc-speedbar-expand-user (list finger name info)
	   nick-str nil nil nil
	   indent)
	  (when (equal exp-char ?-)
	    (forward-line -1)
	    (erc-speedbar-expand-user "+" (list finger name info) indent)))
      (speedbar-make-tag-line
       nil nil nil nil
       nick-str nil nil nil
       indent))))

(defun erc-speedbar-update-channel (buffer)
  "Update the speedbar information about a ERC buffer. The update
is only done when the channel is acutally expanded already."
  ;; This is only a rude hack and doesnt care about multiserver usage
  ;; yet, consider this a brain storming, better ideas?
  (with-current-buffer speedbar-buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (concat "^1: *.+. *"
				       (regexp-quote (buffer-name buffer)))
			       nil t)
	(beginning-of-line)
	(speedbar-delete-subblock 1)
	(erc-speedbar-expand-channel "+" buffer 1)))))

(defun erc-speedbar-expand-user (text token indent)
  (cond ((string-match "+" text)
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	  (save-excursion
	    (end-of-line) (forward-char 1)
	    (let ((finger (nth 0 token))
		  (name (nth 1 token))
		  (info (nth 2 token)))
	      (when finger
		(speedbar-make-tag-line
		 nil nil nil nil
		 finger nil nil nil
		 (1+ indent)))
	      (when name
		(speedbar-make-tag-line
		 nil nil nil nil
		 name nil nil nil
		 (1+ indent)))
	      (when info
		(speedbar-make-tag-line
		 nil nil nil nil
		 info nil nil nil
		 (1+ indent)))))))
	((string-match "-" text)
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops... not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun erc-speedbar-goto-buffer (text buffer indent)
  "When user clicks on TEXT, goto an ERC buffer.
The INDENT level is ignored."
  (require 'dframe)
  (dframe-select-attached-frame speedbar-frame)
  (let ((bwin (get-buffer-window buffer 0)))
    (if bwin
	(progn
	  (select-window bwin)
	  (raise-frame (window-frame bwin)))
      (if dframe-power-click
	  (let ((pop-up-frames t)) (select-window (display-buffer buffer)))
	(dframe-select-attached-frame speedbar-frame)
	(switch-to-buffer buffer)))))


(provide 'erc-speedbar)
