
;; Evil - Emacs Vim Emulation Layer
(setq evil-shift-width 4)
(setq evil-move-cursor-back nil)
(setq evil-regexp-search nil) ;; Same as incremental-search, so they will share history
(setq evil-find-skip-newlines t)
(setq evil-cross-lines t)
(setq evil-respect-visual-line-mode t)
(setq evil-move-beyond-eol t)

;; undo-tree
(setq undo-tree-auto-save-history nil)
(setq evil-undo-system 'undo-tree)
(global-undo-tree-mode)
(add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)

(require 'evil)
(evil-mode 1)



(evil-define-text-object jpl-evil-outer-defun (count &optional beg end type)
     (save-excursion
       (mark-defun)
       (next-line)
       (evil-range (region-beginning) (region-end) type :expanded t)))

(evil-define-text-object jpl-evil-inner-defun (count &optional beg end type)
  "Note: won't work correctly for one-line defuns"
  (save-excursion
    (mark-defun)
    (next-line)
    (next-line)
    (let* ((object-begin (point)))
      (exchange-point-and-mark)
      (previous-line)
      (previous-line)
      (end-of-line)
      (evil-range (object-begin) (point) type :expanded t))))

(define-key evil-inner-text-objects-map "d" 'jpl-evil-inner-defun)
(define-key evil-outer-text-objects-map "d" 'jpl-evil-outer-defun)



(define-key evil-motion-state-map (kbd "C-o") nil)
(define-key evil-normal-state-map (kbd "C-o") nil)
;; (define-key evil-window-state-map (kbd "C-o") nil)
(define-key evil-insert-state-map (kbd "C-o") nil)

;; Always indent in Insert-mode
(define-key evil-insert-state-map (kbd "RET") 'evil-ret-and-indent)


;; (defun evil-undefine ()
;;  (interactive)

;;  (let* ((evil-mode-map-alist)
;;         (command-keys (this-command-keys))
;;         (dummy (prin1 command-keys))
;;         (command (key-binding command-keys))
;;         (dummy2 (prin1 command))
;;        )
;;    (this-command-keys)
;;    (call-interactively command)
;;    )
;;  )


(define-key evil-normal-state-map "\C-o" nil)



(require 'evil-surround)
(global-evil-surround-mode 1)
(define-key evil-visual-state-map (kbd "s") 'evil-surround-region)
(define-key evil-visual-state-map (kbd "S") 'evil-substitute) ;; Previously on s


(setq evil-leader/leader ",")
(setq evil-leader/in-all-states t)
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-key "," 'evil-repeat-find-char-reverse)


;; Never really use this one any more, in favour of just A; . Remove.
(defun jpl-evil-add-trailing-semi ()
  (interactive)
  (evil-end-of-line)
  (unless (looking-back ";")
    (insert ";")))

(defun jpl-evil-move-first-nonblank-or-bol ()
  "Move to the first nonblank char on the line, or if point is
already there, to the beginning of the line."
  (interactive)
  (when (not (bolp))
    (let ((current-point (point)))
      (evil-first-non-blank)
      (when (= (point) current-point)
        (beginning-of-line)
        ))))


(defun jpl-evil-find-conflict-marker ()
  "Search forward for the next merge conflict marker,
  e.g. <<<<<<<"
  (interactive)
  (let ((evil-regexp-search t))
    (push "\\(<<<<<<<\\)\\|\\(=======\\)\\|\\(>>>>>>>\\)" regexp-search-ring)
    (evil-search-next))
  )





(evil-leader/set-key ";" 'jpl-evil-add-trailing-semi)

(evil-leader/set-key "r" 'isearch-backward) ;; Unused
(evil-leader/set-key "e" 'evil-end-of-line) ;; Unused
(evil-leader/set-key "a" 'evil-beginning-of-line) ;; Unused
(evil-leader/set-key "c" 'jpl-evil-find-conflict-marker)
(evil-leader/set-key "R" 'jpl-revert-buffer)



(define-key evil-normal-state-map "\C-r" 'isearch-backward)
(define-key evil-normal-state-map "\C-a" 'evil-first-non-blank)
(define-key evil-motion-state-map "\C-e" 'evil-end-of-line)

(define-key evil-normal-state-map " " 'avy-goto-char-timer)

(define-key evil-normal-state-map (kbd "zd") 'lp/recenter-top-of-defun)


(add-hook 'cperl-mode-hook
          (lambda () (setq evil-word "[:word:]_$@%")))


(evil-set-initial-state 'compilation-mode 'emacs) ;; Or make tab work
(evil-set-initial-state 'help-mode 'emacs) ;; Or make tab work
(evil-set-initial-state 'grep-mode 'emacs) ;; Or make tab work
(evil-set-initial-state 'rcirc-mode 'emacs)
(evil-set-initial-state 'sql-interactive-mode 'emacs)
(evil-set-initial-state 'magit-branch-mode 'emacs)
(evil-set-initial-state 'special-mode 'emacs)
(evil-set-initial-state 'tabulated-list-mode 'emacs)
(evil-set-initial-state 'Buffer-menu-mode 'emacs)

;; Can't get this to work :/, using C-c C-j instead
; (evil-set-initial-state 'git-commit-mode 'insert)


(require 'org) ;; For the keymap binding below to work
;; http://stackoverflow.com/questions/8483182/emacs-evil-mode-best-practice
;; Remap org-mode meta keys for convenience
(mapcar (lambda (state)
    (evil-declare-key state org-mode-map
      (kbd "M-j") 'org-forward-heading-same-level
      (kbd "M-k") 'org-backward-heading-same-level
      (kbd "M-h") 'outline-up-heading
      (kbd "M-l") 'outline-next-visible-heading

      (kbd "M-J") 'org-shiftmetadown
      (kbd "M-K") 'org-shiftmetaup
      (kbd "M-H") 'org-metaleft
      (kbd "M-L") 'org-metaright

      (kbd "C-M-j") 'org-metadown
      (kbd "C-M-k") 'org-metaup
      (kbd "C-M-h") 'org-shiftmetaleft
      (kbd "C-M-l") 'org-shiftmetaright

      (kbd "C-a") 'org-beginning-of-line
      ))
  '(normal insert visual))


;; (require 'evil-matchit)
;; (global-evil-matchit-mode 1)



(require 'expand-region)
(evil-leader/set-key "a" 'er/expand-region)
(evil-leader/set-key "s" 'er/contract-region)




(require 'evil-args)

;; bind evil-args text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;; bind evil-args text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;; bind evil-forward/backward-args
(define-key evil-normal-state-map (kbd "M-l") 'evil-forward-arg)
(define-key evil-normal-state-map (kbd "M-h") 'evil-backward-arg)

;; bind evil-jump-out-args
(define-key evil-normal-state-map (kbd "M-k") 'evil-jump-out-args)


;;;JPL: reinstate? (require 'evil-briefcase)
;; zC -- evil-briefcase-camel-upper
;; zc -- evil-briefcase-camel-lower
;; zS -- evil-briefcase-snake-upper
;; zs -- evil-briefcase-snake-lower
;; zK -- evil-briefcase-kebab-upper
;; zk -- evil-briefcase-kebab-lower
