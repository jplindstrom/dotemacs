
;; Evil - Emacs Vim Emulation Layer
(setq evil-shift-width 4)
(setq evil-move-cursor-back nil)
(setq evil-regexp-search nil) ;; Same as incremental-search, so they will share history
(setq evil-find-skip-newlines t)
(setq evil-cross-lines t)
(add-to-list 'load-path "~/elisp/evil")
(require 'evil)
(evil-mode 1)


;; Define a "defun" Evil text object
(defun jpl-line-before-end-of-defun (&optional arg)
  (interactive)
  (end-of-defun arg)
  (previous-line))

(defun jpl-line-after-beginning-of-defun (&optional arg)
  (interactive)
  (beginning-of-defun arg)
  (next-line))

(evil-define-text-object evil-inner-defun (count &optional beg end type)
  "Select inner defun.

This is a gross simplification, assuming the inner part is one
line within the outer part. Maybe find the next sexp and go with
that? Maybe there's something in the syntax table alerady for
this."
  (evil-inner-object-range count beg end type
   #'jpl-line-before-end-of-defun
   #'jpl-line-after-beginning-of-defun
   ))
(define-key evil-inner-text-objects-map "d" 'evil-inner-defun)

(evil-define-text-object evil-defun (count &optional beg end type)
  "Select around defun."
  (evil-an-object-range count beg end type
   #'end-of-defun
   #'beginning-of-defun))
(define-key evil-outer-text-objects-map "d" 'evil-defun)




(define-key evil-motion-state-map (kbd "C-o") nil)
(define-key evil-normal-state-map (kbd "C-o") nil)
;; (define-key evil-window-state-map (kbd "C-o") nil)
(define-key evil-insert-state-map (kbd "C-o") nil)

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



(require 'surround)
(global-surround-mode 1)

(setq evil-leader/leader ",")
(setq evil-leader/in-all-states t)
(require 'evil-leader)
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

(define-key evil-normal-state-map " " 'ace-jump-mode)

(define-key evil-normal-state-map (kbd "zd") 'lp/recenter-top-of-defun)


(add-hook 'cperl-mode-hook
          (lambda () (setq evil-word "[:word:]_$@%")))


(evil-set-initial-state 'compilation-mode 'emacs) ;; Or make tab work
(evil-set-initial-state 'help-mode 'emacs) ;; Or make tab work
(evil-set-initial-state 'grep-mode 'emacs) ;; Or make tab work
(evil-set-initial-state 'rcirc-mode 'emacs)
(evil-set-initial-state 'sql-mode 'emacs)
(evil-set-initial-state 'sql-interactive-mode 'emacs)
(evil-set-initial-state 'magit-branch-mode 'emacs)


(require 'org) ;; For the keymap binding below to work
;; http://stackoverflow.com/questions/8483182/emacs-evil-mode-best-practice
;; Remap org-mode meta keys for convenience
(mapcar (lambda (state)
    (evil-declare-key state org-mode-map
      (kbd "M-j") 'org-forward-same-level
      (kbd "M-k") 'org-backward-same-level
      (kbd "M-h") 'outline-up-heading
      (kbd "M-l") 'outline-next-visible-heading

      (kbd "M-J") 'org-metadown
      (kbd "M-K") 'org-metaup
      (kbd "M-H") 'org-metaleft
      (kbd "M-L") 'org-metaright

      (kbd "C-M-j") 'org-shiftmetadown
      (kbd "C-M-k") 'org-shiftmetaup
      (kbd "C-M-h") 'org-shiftmetaleft
      (kbd "C-M-l") 'org-shiftmetaright
      ))
  '(normal insert visual))



