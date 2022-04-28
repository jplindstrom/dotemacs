
(defun py-setup-python-lsp ()
  (setq lsp-pylsp-server-command '("pylsp" "-v"))
  (setq read-process-output-max 8192)


  (lsp)
  (lsp-headerline-breadcrumb-mode)
  (lsp-ui-doc-frame-mode)


  ;; Full LSP prefix
  (define-key lsp-mode-map (kbd "C-o C-l") lsp-command-map)

  ;; Company completion trigger
  (local-set-key (kbd "C-o C-c") 'company-complete)

  ;; Go to
  (local-set-key "\C-o\C-g" 'lsp-ui-peek-find-definitions)
  (local-set-key "\C-ogb" 'xref-pop-marker-stack)
  (local-set-key "\C-ogn" 'lsp-ui-find-next-reference)
  (local-set-key "\C-ogp" 'lsp-ui-find-prev-reference)

  ;; Docs
  (local-set-key "\C-o\C-d" 'lsp-ui-doc-show)

  ;; Find
  (local-set-key "\C-ofr" 'lsp-find-references)

  ;; Errors

  ;; Edit
  (local-set-key "\C-oe\C-f" 'lsp-format-buffer)
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
  )


(add-hook 'python-mode-hook 'py-my-setup)
(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))


