;;; property-function-mode-el -- Major mode for editing .properties files

;;; Code:
(defvar property-function-mode-hook nil)
(defvar property-function-mode-map
  (let ((property-function-mode-map (make-keymap)))
    (define-key property-function-mode-map "\C-j" 'newline-and-indent)
    property-function-mode-map)
  "Keymap for Property Function major mode")

(add-to-list 'auto-mode-alist '("\\.properties\\'" . property-function-mode))

(defconst property-function-font-lock-keywords-1
  (list
   ; These define the beginning and end of each Property Function entity definition
   ; "PARTICIPANT" "END_PARTICIPANT" "MODEL" "END_MODEL" "WORKFLOW"
   ; "END_WORKFLOW" "ACTIVITY" "END_ACTIVITY" "TRANSITION"
   ; "END_TRANSITION" "APPLICATION" "END_APPLICATION" "DATA" "END_DATA"
   ; "TOOL_LIST" "END_TOOL_LIST"
   '("messages" . font-lock-builtin-face)
   '("message" . font-lock-variable-name-face))
  "Minimal highlighting expressions for Property Function mode.")

(defconst property-function-font-lock-keywords-2
  (append property-function-font-lock-keywords-1
		  (list
				 ; These are some possible attributes of Property Function entities
			  ; "WPDL_VERSION" "VENDOR" "CREATED" "NAME" "DESCRIPTION"
			; "AUTHOR" "STATUS" "EXTENDED_ATTRIBUTE" "TYPE" "TOOLNAME"
					; "IN_PARAMETERS" "OUT_PARAMETERS" "DEFAULT_VALUE"
			; "IMPLEMENTATION" "PERFORMER" "SPLIT" "CONDITION" "ROUTE"
									  ; "JOIN" "OTHERWISE" "TO" "FROM"
		   '("\\<\\(AUTHOR\\|C\\(ONDITION\\|REATED\\)\\|DE\\(FAULT_VALUE\\|SCRIPTION\\)\\|EXTENDED_ATTRIBUTE\\|FROM\\|I\\(MPLEMENTATION\\|N_PARAMETERS\\)\\|JOIN\\|NAME\\|O\\(THERWISE\\|UT_PARAMETERS\\)\\|PERFORMER\\|ROUTE\\|S\\(PLIT\\|TATUS\\)\\|T\\(O\\(OLNAME\\)?\\|YPE\\)\\|VENDOR\\|WPDL_VERSION\\)\\>" . font-lock-keyword-face)
		   '("\\<\\(TRUE\\|FALSE\\)\\>" . font-lock-constant-face)))
  "Additional Keywords to highlight in Property Function mode.")

(defconst property-function-font-lock-keywords-3
  (append property-function-font-lock-keywords-2
		  (list
		 ; These are some possible built-in values for Property Function attributes
			 ; "ROLE" "ORGANISATIONAL_UNIT" "STRING" "REFERENCE" "AND"
			 ; "XOR" "WORKFLOW" "SYNCHR" "NO" "APPLICATIONS" "BOOLEAN"
							 ; "INTEGER" "HUMAN" "UNDER_REVISION" "OR"
		   '("\\<\\(A\\(ND\\|PPLICATIONS\\)\\|BOOLEAN\\|HUMAN\\|INTEGER\\|NO\\|OR\\(GANISATIONAL_UNIT\\)?\\|R\\(EFERENCE\\|OLE\\)\\|S\\(TRING\\|YNCHR\\)\\|UNDER_REVISION\\|WORKFLOW\\|XOR\\)\\>" . font-lock-constant-face)))
  "Balls-out highlighting in WPDL mode.")

(defvar property-function-font-lock-keywords property-function-font-lock-keywords-3
  "Default highlighting expressions for Property Function mode.")

(defun property-function-indent-line ()
  "Indent current line as Property Function code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
	  (indent-line-to 0)		   ; First line is always non-indented
	(let ((not-indented t) cur-indent)
	  (if (looking-at "^[ \t]*END_") ; If the line we are looking at is the end of a block, then decrease the indentation
		  (progn
			(save-excursion
			  (forward-line -1)
			  (setq cur-indent (- (current-indentation) default-tab-width)))
			(if (< cur-indent 0) ; We can't indent past the left margin
				(setq cur-indent 0)))
		(save-excursion
		  (while not-indented ; Iterate backwards until we find an indentation hint
			(forward-line -1)
			(if (looking-at "^[ \t]*END_") ; This hint indicates that we need to indent at the level of the END_ token
				(progn
				  (setq cur-indent (current-indentation))
				  (setq not-indented nil))
			  (if (looking-at "^[ \t]*\\(PARTICIPANT\\|MODEL\\|APPLICATION\\|WORKFLOW\\|ACTIVITY\\|DATA\\|TOOL_LIST\\|TRANSITION\\)") ; This hint indicates that we need to indent an extra level
				  (progn
					(setq cur-indent (+ (current-indentation) default-tab-width)) ; Do the actual indenting
					(setq not-indented nil))
				(if (bobp)
					(setq not-indented nil)))))))
	  (if cur-indent
		  (indent-line-to cur-indent)
		(indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation

(defvar property-function-mode-syntax-table
  (let ((property-function-mode-syntax-table (make-syntax-table)))
	
    ; This is added so entity names with underscores can be more easily parsed
	(modify-syntax-entry ?_ "w" property-function-mode-syntax-table)
	
	; Comment styles are same as C++
	(modify-syntax-entry ?/ ". 124b" property-function-mode-syntax-table)
	(modify-syntax-entry ?* ". 23" property-function-mode-syntax-table)
	(modify-syntax-entry ?\n "> b" property-function-mode-syntax-table)
	property-function-mode-syntax-table)
  "Syntax table for wpdl-mode")
  
(defun property-function-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map property-function-mode-map)
  (set-syntax-table property-function-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(property-function-font-lock-keywords))
  ;; Register our indentation function
  (set (make-local-variable 'indent-line-function) 'property-function-indent-line)  
  (setq major-mode 'property-function-mode)
  (setq mode-name "Property Function")
  (run-hooks 'property-function-mode-hook))

(provide 'property-function-mode)

;;; property-function-mode.el ends here



