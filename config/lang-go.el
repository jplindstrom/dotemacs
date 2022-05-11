
(defun go-setup-go-lsp ()


  (setq read-process-output-max 8192)

  ;; Run this to install the LSP server
  ;;
  ;;     go install golang.org/x/tools/gopls@latest
  ;;
  (message "JPL: lang-go lsp")


  ;; (lsp)
  (lsp-deferred)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)

  (lsp-headerline-breadcrumb-mode)
  (lsp-ui-doc-frame-mode)
  (yas-minor-mode)


  ;; Full LSP prefix
  (define-key lsp-mode-map (kbd "C-o C-l") lsp-command-map)

  ;; Company completion trigger
  (local-set-key (kbd "C-o C-c") 'company-complete)

  ;; Go to
  (local-set-key "\C-o\C-g" 'lsp-ui-peek-find-definitions)
  (local-set-key "\C-ogb" 'xref-pop-marker-stack)
  (local-set-key "\C-ogn" 'lsp-ui-find-next-reference)
  (local-set-key "\C-ogp" 'lsp-ui-find-prev-reference)

  ;; Peek
  (local-set-key "\C-o\C-p" 'lsp-ui-peek-find-references)
  (local-set-key "\C-opn" 'lsp-ui-peek--select-next)
  (local-set-key "\C-opp" 'lsp-ui-peek--select-prev)
  (local-set-key "\C-opg" 'lsp-ui-peek--goto-xref)

  ;; Docs
  (local-set-key "\C-o\C-d" 'lsp-ui-doc-show)
  (local-set-key "\C-odh" 'lsp-ui-doc-hide)

  ;; Find
  (local-set-key "\C-ofr" 'lsp-find-references)

  ;; Errors

  ;; Edit
  (local-set-key "\C-oe\C-f" 'lsp-format-buffer)

  ;; Edit Refactor
  (local-set-key "\C-oerr" 'lsp-rename)

  ;; Fix
  (local-set-key "\C-oef" 'lsp-ui-sideline-apply-code-actions)
  )

(defun go-my-setup ()
  ;; (setq go-indent-offset 4)

  (go-setup-go-lsp)
  )


(add-hook 'go-mode-hook 'go-my-setup)
;; (setq interpreter-mode-alist
;;       (cons '("go" . go-mode) interpreter-mode-alist))
