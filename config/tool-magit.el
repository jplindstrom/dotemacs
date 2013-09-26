
;; Git

;; (setq load-path (cons (concat emacs-home-directory "elisp/magit") load-path))
;; (setq load-path (cons "/home/j.lindstrom/dev/personal/magit" load-path))
(require 'magit)
;; (require 'magit-svn)


;; change magit diff colors
(set-face-foreground 'magit-diff-add "blue3")
(set-face-foreground 'magit-diff-del "red3")
(set-face-background 'magit-item-highlight "DarkSeaGreen1")


;; Avoid re-flymaking all open buffers when checking out a new branch
(require 'flymake)
(defadvice magit-revert-buffers (around magit-disable-flymake-during-revert)
  (setq flymake-start-syntax-check-on-find-file nil)
  ad-do-it
  (setq flymake-start-syntax-check-on-find-file t))
(ad-activate 'magit-revert-buffers)


;; Expire Projectile cache when checking out a new branch
(require 'projectile)
(defun jpl-projectile-invalidate-project-cache ()
  (projectile-invalidate-cache nil)
  (message "")
  )
(defadvice magit-revert-buffers (after magit-invalidate-projectile-cache)
  (jpl-projectile-invalidate-project-cache))
(ad-activate 'magit-revert-buffers)





(defun magit-commit-message-ticket-number ()
  (interactive)
  (let ((branch (magit-get-current-branch)))
    (if (string-match "\\([a-zA-Z]+-[0-9]+\\)" branch)
        (let* ( (match (match-string 1 branch))
                (ticket-number (upcase match)) )
          ticket-number))))

(define-key magit-log-edit-mode-map (kbd "C-c n")
  (lambda ()
    (interactive)
    (goto-char (point-at-bol))
    (insert (concat (magit-commit-message-ticket-number) ": "))))

(define-key magit-log-edit-mode-map (kbd "C-c b")
  (lambda ()
    (interactive)
    (goto-char (point-at-bol))
    (insert (concat (magit-get-current-branch) ": "))))


;; Avoid using / in local tracking branch names
;; TODO: set the magit-default-tracking-name-function var instead!
;; (defun magit-get-tracking-name (remote branch)
;;   "Given a REMOTE and a BRANCH name, ask the user for a local
;; tracking brach name suggesting a sensible default."
;;   (when (yes-or-no-p
;;          (format "Create local tracking branch for %s? " branch))
;;     (let* ((default-name (concat (replace-regexp-in-string "[/]" "-" branch)))
;;            (chosen-name (read-string (format "Call local branch (%s): " default-name)
;;                                      nil
;;                                      nil
;;                                      default-name)))
;;       (when (magit-ref-exists-p (concat "refs/heads/" chosen-name))
;;         (error "'%s' already exists." chosen-name))
;;       chosen-name)))


;; (require 'vc-git)
;; (when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
;; (require 'git-emacs)
;; ;(require 'git)
;; (autoload 'git-blame-mode "git-blame"
;;   "Minor mode for incremental blame for Git." t)


