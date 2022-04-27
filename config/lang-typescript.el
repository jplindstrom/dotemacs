(require 'flycheck)


(defun setup-tide-keys ()
  ;; company completion trigger
  (local-set-key "\C-\M-SPC" 'company-tide)


  ;; Go to
  (local-set-key "\C-o\C-g" 'tide-jump-to-definition)
  (local-set-key "\C-ogi" 'tide-jump-to-implementation)
  (local-set-key "\C-ogb" 'xref-pop-marker-stack)

  ;; Docs
  (local-set-key "\C-o\C-d" 'tide-documentation-at-point)

  ;; Find
  (local-set-key "\C-ofr" 'tide-references)

  ;; Errors
  (local-set-key "\C-ose" 'tide-project-errors)
  (local-set-key "\C-oss" 'tide-error-at-point)

  ;; Edit
  (local-set-key "\C-oed" 'tide-jsdoc-template)
  (local-set-key "\C-oe\C-f" 'tide-format)

  ;; Edit Refactor
  (local-set-key "\C-o\C-d" 'tide-documentation-at-point)

  (local-set-key "\C-oe\C-r" 'tide-refactor)
  (local-set-key "\C-oer\C-r" 'tide-rename-file)
  (local-set-key "\C-oerr" 'tide-rename-symbol)
  (local-set-key "\C-oei" 'tide-organize-imports)
  (local-set-key "\C-oef" 'tide-fix)
  )

;; https://github.com/ananthakumaran/tide
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1)

  (setup-tide-keys))


;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'js2-mode-hook #'setup-tide-mode)
;; configure javascript-tide checker to run after your default javascript checker
;;; Doesn't work? (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)


;; Evil mode: don't use it in the tide buffers
(evil-set-initial-state 'tide-references-mode 'emacs)
(evil-set-initial-state 'tide-project-errors-mode 'emacs)

