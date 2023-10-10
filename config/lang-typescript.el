(require 'flycheck)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(defun setup-tide-keys ()
  ;; company Completion trigger
  (local-set-key "\C-o\C-c" 'company-tide) ;; Complete

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

  (setq lsp-javascript-display-enum-member-value-hints t)
  (setq lsp-javascript-display-inlay-hints t)
  (setq lsp-javascript-display-parameter-name-hints "all")
  (setq lsp-javascript-display-parameter-name-hints-when-argument-matches-name t)
  (setq lsp-javascript-display-parameter-type-hints t)
  (setq lsp-javascript-display-property-declaration-type-hints t)
  (setq lsp-javascript-display-return-type-hints t)
  (setq lsp-javascript-display-variable-type-hints t)
  (setq lsp-javascript-suggest-complete-function-calls t)
  (setq lsp-javascript-suggest-complete-js-docs nil)
  (setq lsp-javascript-suggestion-actions-enabled t)
  (setq lsp-typescript-suggest-complete-function-calls t)
  (setq lsp-typescript-surveys-enabled nil)

  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1)

  ;;; Formatting
  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'jpl/tide-mode-maybe-format-before-save)

  (setup-tide-keys)

  ;; Restore original functionality, don't call prettier
  (local-set-key "\M-q" 'fill-paragraph)
  )

;; Disable `jpl/tide-mode-format-before-save` in .dir-locals if needed
;;
;; If it doesn't format, ensure that `prettier` is installed into the
;; active nvm environment. Maybe install it using
;;
;;    npm install -g prettier
;;
(setq jpl/tide-mode-format-before-save t)
(defun jpl/tide-mode-maybe-format-before-save ()
  (when jpl/tide-mode-format-before-save
    (tide-format-before-save)))


;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'js2-mode-hook #'setup-tide-mode)
;; configure javascript-tide checker to run after your default javascript checker
;;; Doesn't work? (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)





(defun setup-jest-test ()
  (interactive)
  ;; TODO: check if jest is used in this project
  (jest-test-mode)
  (local-set-key "\C-o\C-r" 'jest-test-run)
  (local-set-key "\C-ort" 'jest-test-run-at-point)
  (local-set-key "\C-orr" 'jest-test-rerun-test)
  (local-set-key "\C-ora" 'jest-test-run-all-tests)

  (local-set-key "\C-ord" 'jest-test-debug)
  )

(add-hook 'typescript-mode-hook #'setup-jest-test)
(add-hook 'js2-mode-hook #'setup-jest-test)





(use-package prettier-js
  :commands (prettier-js-mode prettier)
  :init (add-hook 'typescript-mode-hook 'prettier-js-mode)
  :bind (:map typescript-mode-map ("M-q" . prettier))
  :config
  (setq prettier-target-mode "typescript-mode")
  )
;; (setq prettier-js-args '(
;;     "--trailing-comma" "all"
;;     "--bracket-spacing" "false"))


;; Evil mode: don't use it in the tide buffers
(evil-set-initial-state 'tide-references-mode 'emacs)
(evil-set-initial-state 'tide-project-errors-mode 'emacs)
