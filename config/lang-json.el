

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



(defun jpl/convert-json-region-to-string-literal (beg end)
  (interactive "r")
  (unless (use-region-p)
    (error "Select a valid JSON construct to encode as a JSON string"))
  (shell-command-on-region beg end "jq -sR . | perl -pe 'chomp if eof'" nil t))

(defun jpl/convert-json-string-literal-to-json (beg end)
  (interactive "r")
  (unless (use-region-p)
    (unless (jpl/select-string-literal-at-point)
      (error "Select a valid JSON string literal to parse as JSON")))
  (shell-command-on-region beg end "jq -r . | perl -pe 'chomp if eof'" nil t))

(defun jpl/select-string-literal-at-point ()
  "Selects the string literal at point, if point is inside a string."
  (interactive)
  (let ((syntax (syntax-ppss)))
    (when (nth 3 syntax)
      (goto-char (nth 8 syntax))
      (let ((beg (point)))
        (goto-char (nth 8 syntax))
        (forward-sexp)
        (set-mark beg)
        t))))

(defun jpl/toggle-json-string-literal-and-structure ()
  "If point is in a string literal, this toggles it to a JSON.
If there's an active region, this toggles it to a string literal.
Otherwise, it will try to find the current or outer [] or {} structure and converts that."
  (interactive)
  (cond ((use-region-p)
         (jpl/convert-json-region-to-string-literal (region-beginning) (region-end)))
        ((jpl/in-string-p)
         (when (jpl/select-string-literal-at-point)
           (jpl/convert-json-string-literal-to-json (region-beginning) (region-end))))
        (t
         (when (jpl/find-and-select-surrounding-json)
           (jpl/convert-json-region-to-string-literal (region-beginning) (region-end))))))

(defun jpl/in-string-p ()
  "Check if point is within a string."
  (interactive)
  (nth 3 (syntax-ppss)))

(defun jpl/find-and-select-surrounding-json ()
  "Find the current or outer [] or {} structure and select it."
  (interactive)
  (ignore-errors
    (when (search-forward-regexp "\\[\\|{" nil 'go-end)
      (backward-char)
      (mark-sexp)
      t)))
