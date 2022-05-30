
;; Git

;; (setq load-path (cons (concat emacs-home-directory "elisp/magit") load-path))
;; (setq load-path (cons "/home/j.lindstrom/dev/personal/magit" load-path))
(require 'magit)
;; (require 'magit-svn)

(add-to-list 'transient-values '(magit-show-refs "--sort=-committerdate"))


;; change magit diff colors
;;;; Not used with flatui-theme
;; (set-face-foreground 'magit-diff-add "blue3")
;; (set-face-foreground 'magit-diff-del "red3")
;; (set-face-background 'magit-item-highlight "DarkSeaGreen1")


(add-hook
 'git-commit-mode-hook
 (lambda () (setq git-commit-summary-max-length 80))
 )




;;JPL: does this still work?
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


(defun magit-commit-message-prefix ()
  "Return the branch ticket number, or the branch name"
  (or
   (magit-commit-message-ticket-number)
   (magit-get-current-branch)))


(defun magit-insert-commit-message-prefix ()
  (interactive)
  (goto-char (point-at-bol))
  (insert (concat (magit-commit-message-prefix) ": "))
)

(defun evil-magit-insert-commit-message-prefix ()
  (interactive)
  (magit-insert-commit-message-prefix)
  (evil-insert 1)
)

(define-key git-commit-mode-map (kbd "C-c C-j")
  'evil-magit-insert-commit-message-prefix)


(define-key git-commit-mode-map (kbd "C-c b")
  (lambda ()
    (interactive)
    (goto-char (point-at-bol))
    (insert (concat (magit-get-current-branch) ": "))))


;; JPL: check if this is needed / wanted anymore
(defun magit-tracking-name-branch-name-only (remote branch)
  "Use local escapedbranch name only for tracking branches."
  (magit-escape-branch-name branch))

(setq magit-default-tracking-name-function 'magit-tracking-name-branch-name-only)


;; Show fine differences for all displayed diff hunks.
(setq magit-diff-refine-hunk 'all)



;; Override magit-mode.el magit-mode-map
; Restore C-tab to global setting, used to be cycle
(define-key magit-mode-map [C-tab] nil)


(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)


;;
;;; Gitlab frontend
;;
;; git config --global gitlab.user my.username
;; https://gitlab.com/-/profile/personal_access_tokens (api permissions)
;; ~/.authinfo
;; machine gitlab.com/api/v4 login my.username^forge password 123456
(require 'forge)

