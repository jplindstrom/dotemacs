
(defun insert-python-self ()
  (interactive)
  (insert "self.")
  (company-complete)
  )

(defun py-setup-python-lsp ()

  ;; Disable documentation related linting moaning
  (setq lsp-pylsp-plugins-pydocstyle-enabled nil)

  (setq lsp-pylsp-server-command '("pylsp" "-v"))
  (setq read-process-output-max 8192)

  ;; Run this to install the LSP server
  ;;
  ;;     pip install "python-lsp-server[all]"
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
  (local-set-key "\C-o\C-g" 'lsp-ui-peek-find-definitions)
  (local-set-key "\C-ogb" 'xref-pop-marker-stack)
  (local-set-key "\C-ogn" 'lsp-ui-find-next-reference)
  (local-set-key "\C-ogp" 'lsp-ui-find-prev-reference)

  ;; Peek
  (local-set-key "\C-o\C-p" 'lsp-ui-peek-find-references)
  (local-set-key "\C-opn" 'lsp-ui-peek--select-next)
  (local-set-key "\C-opp" 'lsp-ui-peek--select-prev)
  (local-set-key "\C-opg" 'lsp-ui-peek--goto-xref)
  ;; Or: M-n, M-p for next/prev
  (local-set-key "\M-return" 'lsp-ui-peek--goto-xref)

  ;; Docs
  (local-set-key "\C-o\C-d" 'lsp-ui-doc-show)
  (local-set-key "\C-odh" 'lsp-ui-doc-hide)

  ;; Find
  (local-set-key "\C-ofr" 'lsp-find-references)

  ;; Errors

  ;; Edit Refactor
  (local-set-key "\C-oe\C-f" 'lsp-format-buffer)
  (local-set-key "\C-oerr" 'lsp-rename)
  ;; Fix
  (local-set-key "\C-oef" 'lsp-ui-sideline-apply-code-actions)
  )

(defun py-my-setup ()
  (setq py-indent-offset 4)
  (make-local-variable 'outline-regexp)
  (make-local-variable 'outline-heading-end-regexp)
  (setq outline-regexp "[ \t]*\\(def\\|class\\)\\>")
  (setq outline-heading-end-regexp ":[ \t]*[\r\n]")
  (outline-minor-mode 1)

  (py-setup-python-lsp)

  ;; pytest
  (add-to-list 'pytest-project-root-files "pyproject.toml")
  (add-to-list 'pytest-project-root-files "pytest.ini")

  (local-set-key (kbd "C-; C-l") 'insert-python-self)
  )


(add-hook 'python-mode-hook 'py-my-setup)
(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))
