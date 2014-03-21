
;; Git

;; (setq load-path (cons (concat emacs-home-directory "elisp/magit") load-path))
;; (setq load-path (cons "/home/j.lindstrom/dev/personal/magit" load-path))
(require 'magit)
;; (require 'magit-svn)


;; change magit diff colors
;;;; Not used with flatui-theme
;; (set-face-foreground 'magit-diff-add "blue3")
;; (set-face-foreground 'magit-diff-del "red3")
;; (set-face-background 'magit-item-highlight "DarkSeaGreen1")


;; Avoid re-flymaking all open buffers when checking out a new branch
(require 'flymake)
(defadvice magit-refresh-wrapper (around magit-disable-flymake-during-revert)
  (setq flymake-start-syntax-check-on-find-file nil)
  ad-do-it
  (setq flymake-start-syntax-check-on-find-file t)
  (message "JPL: revert no flymake")
  )
(ad-activate 'magit-refresh-wrapper)


;; Expire Projectile cache when checking out a new branch
(require 'projectile)
(defun jpl-projectile-invalidate-project-cache ()
  (projectile-invalidate-cache nil)
  (message "")
  )
(defadvice magit-refresh-wrapper (after magit-invalidate-projectile-cache)
  (jpl-projectile-invalidate-project-cache))
(ad-activate 'magit-refresh-wrapper)





(defun magit-commit-message-ticket-number ()
  (interactive)
  (let ((branch (magit-get-current-branch)))
    (if (string-match "\\([a-zA-Z]+-[0-9]+\\)" branch)
        (let* ( (match (match-string 1 branch))
                (ticket-number (upcase match)) )
          ticket-number))))

(define-key git-commit-mode-map (kbd "C-c n")
  (lambda ()
    (interactive)
    (goto-char (point-at-bol))
    (insert (concat (magit-commit-message-ticket-number) ": "))))

(define-key git-commit-mode-map (kbd "C-c b")
  (lambda ()
    (interactive)
    (goto-char (point-at-bol))
    (insert (concat (magit-get-current-branch) ": "))))


(defun magit-tracking-name-branch-name-only (remote branch)
  "Use local escapedbranch name only for tracking branches."
  (magit-escape-branch-name branch))

(setq magit-default-tracking-name-function 'magit-tracking-name-branch-name-only)


