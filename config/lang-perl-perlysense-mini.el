;; Mini Devel::PerlySense which re-implements the only real thing I
;; use PerlySense now for, outside of Perl dev, which is finding
;; things in a project. Use projectile instead.
;;
;; This is because Devel::PerlySense is excrutiatingely slow on the
;; macOS (could be the work Mac crapware)


;; ** PerlySense **
;; The PerlySense prefix key (unset only if needed)

(define-key grep-mode-map "\C-o" nil)
(define-key compilation-mode-map "\C-o" nil)
(define-key dired-mode-map "\C-o" nil)
;; (global-unset-key "\C-o")  ;; Already done erlier

(setq ps/key-prefix "\C-o")



(defun ps/active-region-string ()
  "Return the string making up the active region, or nil if no
region is active"
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    nil))

(defun ps/project-dir ()
  "Return the project dir of the current buffer, or nil of no
project was found"
  (let ((dir (file-name-directory (or buffer-file-name "./dummy"))))
    ;; (projectile-acquire-root dir)
    (projectile-project-root dir)
    )
  )


;; Probably reinventing the wheel here
(defmacro ps/with-default-directory (dir &rest body)
  "Execute the forms in BODY with the current
directory (default-directory) temporarily set to 'dir'.

The value returned is the value of the last form in BODY."
  (let ((original-dir default-directory)
        (original-buffer (current-buffer)))
    `(prog2
         (cd ,dir)
         ,@body
       (with-current-buffer ,original-buffer
         (cd ,original-dir)))))


(defmacro ps/with-project-dir (&rest body)
  "Execute the forms in BODY with the current directory
temporarily set to the project dir of the current buffer.

The value returned is the value of the last form in BODY."
  (let ((dir
         (or
          `(ps/project-dir)
          `(progn
            (message "Could not identify a Project Directory, using current directory instead.")
            ,default-directory
            ))))
    `(progn
       (ps/with-default-directory
        ,dir
        ,@body))))

(defun ps/find-project-ack-thing-at-point ()
  "Run ack from the project dir. Default to a sensible ack command line.

If there is an active region, search for that.

if there is a word at point, search for that (with -w word boundary).

If not, search for an empty string.
"
  (interactive)
  (ps/with-project-dir
   (let* ((word-only-flag "")
          (search-term (or
                        (ps/active-region-string)
                        (let ((word-at-point (find-tag-default)))
                          (if (not word-at-point)
                              nil
                            (setq word-only-flag "-w ")
                            word-at-point))
                        ""))
          (escaped-search-term (shell-quote-argument search-term))

          ;; If the string is quoted, put the cursor just inside the
          ;; quote, else at the start of the string
          (quote-offset (if (string-match "^[\"']" escaped-search-term) 1 0))

          (ack-base-command (format "ack --nopager --nogroup --nocolor %s-Q -- " word-only-flag))
          (ack-command (format "%s%s" ack-base-command escaped-search-term))
          (grep-find-command   ;; For Emacs <= 22
           (cons               ;; Second item sets the initial position
            ack-command (+ 1 quote-offset (length ack-base-command))))
          (grep-host-defaults-alist  ;; For Emacs > 22, also set this
           `((localhost (grep-find-command ,grep-find-command))))
          )
   (call-interactively 'grep-find))))



(global-set-key (format "%sfa" ps/key-prefix) #'ps/find-project-ack-thing-at-point)


(defun ps/minibuffer-ack-option-filetype (new-type)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "\\(--nocolor \\)--\\([a-z-]+\\) " nil t) ; Replace option
        (let ((existing-type (match-string 2)))
          (if (string= new-type existing-type)
              ;; Same, so toggle by removing filetype
              (replace-match "\\1" nil nil)
            ;; Not same, so replace
            (replace-match (format "\\1--%s " new-type) nil nil)
            )
          )
      (if (re-search-forward "\\(--nocolor \\)" nil t) ; Add option
          (replace-match (format "\\1--%s " new-type) nil nil)
        (message "nope"))
      )
    )
  )
(defun ps/minibuffer-ack-option-all         () (interactive) (ps/minibuffer-ack-option-filetype "all"))
(defun ps/minibuffer-ack-option-known-types () (interactive) (ps/minibuffer-ack-option-filetype "known-types"))
(defun ps/minibuffer-ack-option-perl        () (interactive) (ps/minibuffer-ack-option-filetype "perl"))
(defun ps/minibuffer-ack-option-sql         () (interactive) (ps/minibuffer-ack-option-filetype "sql"))

(defun ps/minibuffer-ack-option-toggle (option)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward (format " %s " option) nil t) ;; Found one, remove it
        (replace-match " " nil nil)
      ;; Didn't find one, add it
      (beginning-of-line)
      (if (re-search-forward (format " -- " option) nil t)
          (replace-match (format " %s -- " option) nil nil)
        )
      )
    )
  )
(defun ps/minibuffer-ack-option-toggle-caseinsensitive () (interactive) (ps/minibuffer-ack-option-toggle "-i"))
(defun ps/minibuffer-ack-option-toggle-word            () (interactive) (ps/minibuffer-ack-option-toggle "-w"))
(defun ps/minibuffer-ack-option-toggle-quote           () (interactive) (ps/minibuffer-ack-option-toggle "-Q"))

;; This key map is used inside grep-find
(define-key minibuffer-local-shell-command-map (format "%sa" ps/key-prefix) 'ps/minibuffer-ack-option-all)
(define-key minibuffer-local-shell-command-map (format "%sk" ps/key-prefix) 'ps/minibuffer-ack-option-known-types)
(define-key minibuffer-local-shell-command-map (format "%sp" ps/key-prefix) 'ps/minibuffer-ack-option-perl)
(define-key minibuffer-local-shell-command-map (format "%ss" ps/key-prefix) 'ps/minibuffer-ack-option-sql)

(define-key minibuffer-local-shell-command-map (format "%si" ps/key-prefix) 'ps/minibuffer-ack-option-toggle-caseinsensitive)
(define-key minibuffer-local-shell-command-map (format "%sw" ps/key-prefix) 'ps/minibuffer-ack-option-toggle-word)
(define-key minibuffer-local-shell-command-map (format "%sq" ps/key-prefix) 'ps/minibuffer-ack-option-toggle-quote)



(global-set-key (format "%sgv" ps/key-prefix) 'magit-status)
(global-set-key (format "%sgV" ps/key-prefix) 'magit-status)


(global-set-key (kbd "\C-o \C-t") 'treemacs-select-window)



;; Snippets

(defun ps/current-package-name ()
  "Return the name of the current package statement, or nil if
  there isn't one."
  (save-excursion
    (end-of-line)
    (if (search-backward-regexp "^ *\\bpackage +\\([a-zA-Z0-9:_]+\\)" nil t)
        (let (( package-name (match-string 1) ))
          package-name)
      nil)))



(defun ps/package-name-from-file ()
  "Return the name of the current file as if it was a package
name, or return nil if not found."
  (interactive)
  (let* ((file-name (buffer-file-name)))
    (if (string-match "\\blib/\\(.+?\\)\\.pm$" file-name)
        (let* ((name-part (match-string 1 file-name)))
          (replace-regexp-in-string "/" "::" name-part)
          )
      )))



(defun ps/edit-copy-package-name ()
  "Copy (put in the kill-ring) the name of the current package
  statement, and display it in the echo area. Or, if not found,
  use the file package name."
  (interactive)
  (let ((package-name
         (or
          (ps/current-package-name)
          (ps/package-name-from-file))))
    (if package-name
        (progn
          (kill-new package-name)
          (message "Copied package name '%s'" package-name))
      (error "No package found either in the source or the file name"))))

