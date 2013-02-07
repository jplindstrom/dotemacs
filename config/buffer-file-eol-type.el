
(defun set-buffer-file-eol-type (eol-type)
   "Set the file end-of-line conversion type of the current buffer to
 EOL-TYPE.
 This means that when you save the buffer, line endings will be converted
 according to EOL-TYPE.

 EOL-TYPE is one of three symbols:

   unix (LF)
   dos (CRLF)
   mac (CR)

 This function marks the buffer modified so that the succeeding
 \\[save-buffer]
 surely saves the buffer with EOL-TYPE.  From a program, if you don't want
 to mark the buffer modified, use coding-system-change-eol-conversion
 directly [weikart]."
   (interactive "SEOL type for visited file (unix, dos, or mac): ")
   (setq buffer-file-coding-system (coding-system-change-eol-conversion
                      buffer-file-coding-system eol-type))
   (set-buffer-modified-p t)
   (force-mode-line-update))

 (global-set-key "\^Cu" (lambda () (interactive) (set-buffer-file-eol-type 'unix)))
 (global-set-key "\^Cd" (lambda () (interactive) (set-buffer-file-eol-type 'dos)))
 (global-set-key "\^Cm" (lambda () (interactive) (set-buffer-file-eol-type 'mac)))

 ;; Make the mode-line display the standard EOL-TYPE symbols (used above)...
(setq eol-mnemonic-undecided "(?)"  ;; unknown EOL type
       eol-mnemonic-unix  "(unix)" ;; LF
       eol-mnemonic-dos  "(dos)"  ;; CRLF
       eol-mnemonic-mac  "(mac)") ;; CR




