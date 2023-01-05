

(defun jpl/enable-json-mode ()
  (interactive)
  (json-mode)

  ;; Run this to install the LSP server
  ;;
  ;;     npm i -g vscode-langservers-extracted
  ;;

  (lsp)
  (lsp-headerline-breadcrumb-mode)

  ;; (setq lsp-ui-sideline-show-diagnostics t)   ;;  show diagnostics messages in sideline
  ;; (setq lsp-ui-sideline-show-hover t)         ;;  show hover messages in sideline
  ;; (setq lsp-ui-sideline-show-code-actions t)  ;;  show code actions in sideline
  ;; (setq lsp-ui-sideline-update-mode t)        ;;  When set to 'line' the information will be updated when user changes current line otherwise the information will be updated when user changes current point
  ;; (setq lsp-ui-sideline-delay t)              ;;  secon

  (lsp-ui-doc-enable t)


  ;; Full LSP prefix
  (define-key lsp-mode-map (kbd "C-o C-l") lsp-command-map)

  ;; Company completion trigger
  (local-set-key (kbd "C-o C-c") 'company-complete)

  ;; Go to
  (local-set-key "\C-ogb" 'xref-pop-marker-stack)

  ;; Docs
  (local-set-key "\C-o\C-d" 'lsp-ui-doc-show)
  (local-set-key "\C-odh" 'lsp-ui-doc-hide)

  ;; Edit
  (local-set-key "\C-oe\C-f" 'lsp-format-buffer)
  ;; Fix
  (local-set-key "\C-oef" 'lsp-ui-sideline-apply-code-actions)
  )

(add-to-list 'auto-mode-alist '("\\.json$" . jpl/enable-json-mode))
