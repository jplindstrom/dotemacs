;; Author : Phil Jackson <emacs@shellarchive.co.uk>
;; Summary: anything.el related config

(defcustom anything-c-adaptive-history-file "~/elisp/anything-c-adaptive-history"
  "Path of file where history information is stored."
  :type 'string
  :group 'anything-config)

(defun anything-compatible-occur (name buffer-list pattern)
  (let ((anything-occur-buffer (get-buffer-create name)))
    (with-current-buffer anything-occur-buffer
      (occur-mode)
      (erase-buffer)
      (let ((count (occur-engine
                    pattern
                    buffer-list
                    anything-occur-buffer
                    list-matching-lines-default-context-lines
                    case-fold-search
                    list-matching-lines-buffer-name-face
                    nil list-matching-lines-face
                    (not (eq occur-excluded-properties t)))))
        (when (> count 0)
          (setq next-error-last-buffer anything-occur-buffer)
          (cdr (split-string (buffer-string) "\n" t)))))))

(defvar anything-c-source-occur
  '((name . "Occur")
    (init . (lambda ()
              (setq anything-occur-current-buffer
                    (current-buffer))))
    (candidates . (lambda ()
                    (anything-compatible-occur
                     "*Anything Occur*"
                     (list anything-occur-current-buffer)
                     anything-pattern)))
    (action .
     (("Goto line" . (lambda (candidate)
                       (with-current-buffer "*Anything Occur*"
                         (search-forward candidate))
                       (goto-line (string-to-number candidate)
                                  anything-occur-current-buffer)))))
    (requires-pattern . 3)
    (volatile)
    (delayed)))

(defun project-root-filename-history (project-root)
  "File name history but with non-existant files remoced and
files that are in the current project highlighted."
  (let* ((files (remove-if-not 'file-exists-p file-name-history))
         (files (delete-dups files))
         (hits '()))
    (dolist (f files)
      (let ((f (abbreviate-file-name f)))
        (when (file-exists-p f)
          (setq hits
                (append
                 hits
                 (list (cond
                         ((and project-root
                               (project-root-file-in-project
                                f
                                (cons "p" project-root)))
                          (propertize f 'face 'font-lock-string-face))
                         ((not (file-writable-p f))
                          (propertize f 'face 'font-lock-warning-face))
                         (t
                          f))))))))
    hits))

(defvar anything-c-source-pr-file-name-history
  '((name . "File Name History")
    (init . (lambda ()
              (unless project-details
                (project-root-fetch))
              (setq anything-project-root project-details)))
    (candidates . (lambda ()
                    (project-root-filename-history
                     (cdr anything-project-root))))
    (type . file)
    (volatile)))





(defvar connect-to-db-candidates
  '(
    ("dynamite_int"  . ("dynamite_integration"
                        "nm-int-db-iplayer.bbc.redbeemedia.net"
                        "dynamite_pub" "matt4norm"))
    ("dynamite_csd"  . ("dynamite_csd"
                        "nm-dev-db-iplayer.bbc.redbeemedia.net"
                        "scratchadmin" "scratchadmin"))
    ("dynamite_trunk_publishing_test" . ("dynamite_publishing_lindsj05_trunk_test" "localhost" "root" "r00t"))
    ("dynamite_trunk_activity_test"   . ("dynamite_activity_lindsj05_trunk_test" "localhost" "root" "r00t"))
    
    ))

(defun connect-to-db (db)
  (let ((details (cdr (assoc db connect-to-db-candidates)))
        (buf (get-buffer "*SQL*")))
    (when buf
      (kill-buffer buf))
    (when details
      (destructuring-bind (sql-database
                           sql-server
                           sql-user
                           sql-password) details
        (when (setq buf (sql-connect-mysql))
          (switch-to-buffer buf))))))

(defvar anything-c-source-databases
  `((name . "Databases")
    (candidates . ,(mapcar 'car connect-to-db-candidates))
    (action . (("Connect" . connect-to-db)))))




(with-library 'anything
  ;; dunno why I have to do this manually
  (require 'anything-match-plugin)
  (set-face-attribute 'anything-header nil
                      :inherit 'header-line
                      :underline t
                      :height 1.3
                      :weight 'bold)
  (global-set-key (kbd "C-c a") 'anything)
  (global-set-key (kbd "C-c C-a") 'anything)
  (setq anything-c-use-standard-keys t)
  (with-library 'anything-config
    (setq anything-sources
          (list 
                project-root-anything-config-files
                anything-c-source-buffers+
;                anything-c-source-pr-file-name-history
                anything-c-source-occur
                anything-c-source-databases
;                anything-c-source-kill-ring
                ))))

    ;; Adds make file targets
    ;(with-library 'anything-make)))

(provide 'my-anything-config)
