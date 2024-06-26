
(defun tf-setup-terraform-lsp ()
  (setq read-process-output-max 8192)

  ;; Run this to install the LSP server
  ;;
  ;;     sudo aptitude install terraform-ls
  ;;

  (lsp)
  (lsp-headerline-breadcrumb-mode)

  (setq lsp-terraform-ls-enable-show-reference t)

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

  ;; Docs
  (local-set-key "\C-o\C-d" 'jpl/terraform-doc-at-point-in-buffer)
  (local-set-key "\C-odu" 'jpl/terraform-copy-doc-url)

  ;; Find
  (local-set-key "\C-ofr" 'lsp-find-references)

  ;; Errors

  ;; Edit
  (local-set-key "\C-oe\C-f" 'lsp-format-buffer)
  ;; Fix
  (local-set-key "\C-oef" 'lsp-ui-sideline-apply-code-actions)


  ;; Tab - toggle heading outline
  (define-key evil-normal-state-local-map (kbd "C-i") 'jpl/terraform-outline-toggle)
  )

(defun terraform-my-setup ()
  (tf-setup-terraform-lsp))


(add-hook 'terraform-mode-hook 'terraform-my-setup)
(setq interpreter-mode-alist
      (cons '("terraform" . terraform-mode) interpreter-mode-alist))




;; Extend terraform-doc to work in Terraform buffer
;;
;; Things that don't work
;; - E.g. data "template_file" "container_definitions_yaml" { }
;;   - Which provider is this?

(require 'terraform-doc)
(require 's)

(defvar jpl/prefix-to-provider-name
  '(("aws" . "AWS")))
(defun jpl/terraform-doc-get-provider-from-prefix (prefix)
  (let* ((provider-name (cdr (assoc-string prefix jpl/prefix-to-provider-name prefix)))
         (provider (assoc provider-name terraform-doc-providers))
         )
    provider
    )
  )

(defun jpl/terraform-doc-find-item (type thing)
  (let* ((short-thing (nth 1 (s-split-up-to "_" thing 1)))
         (regex (format "%s/%s" type short-thing)))
    (goto-char (point-min))
    (search-forward-regexp regex nil t) ;;;JPL: fail, catch and say, not found
    (beginning-of-line)
    ))

(defun jpl/terraform-doc-fetch-provider-data (provider)
  (let* ((terraform-doc-buffer-name (format "*Terraform:%s*" (cdr provider))))
    (terraform-doc provider)
    (switch-to-buffer terraform-doc-buffer-name)))

(defun jpl/terraform-doc-at-point-in-buffer ()
  "Show the Terraform Markdown docs for the thing at point.

The Terraform provider is deduced based on the resource/data
prefix, e.g. 'aws_*'.

The provider docs are fetched from GitHub on first lookup."
  (interactive)
  (let* ((type-thing (jpl/terraform-doc--thing-at-point))
         (type (nth 0 type-thing))
         (thing (nth 1 type-thing))
         (prefix (nth 0 (s-split "_" thing)))
         (provider (jpl/terraform-doc-get-provider-from-prefix prefix)))

    (if provider
        (let* ((doc-buffer (jpl/terraform-doc-fetch-provider-data provider)))

          ;; We're now in the terraform-doc buffer for the provider
          (jpl/terraform-doc-find-item type thing)
          (terraform-doc-at-point)

          ;; Put the main docs buffer at the end, so that it doesn't show
          ;; when the user kills the thing docs buffer.
          (bury-buffer doc-buffer))
      (error "No provider found for '%s %s'" type thing))))


(defun jpl/terraform-doc--thing-at-point ()
  (save-excursion
    (goto-char (line-beginning-position))
    (unless (looking-at-p "^\\(resource\\|data\\)")
      (re-search-backward "^\\(resource\\|data\\)" nil t))
    (let* ((type (substring-no-properties (thing-at-point 'symbol))))
      (forward-symbol 2)
      (let* ((thing (substring-no-properties (thing-at-point 'symbol))))
        (list type thing)))))





;; Extend terraform-mode to copy the

(require 'terraform-mode)

(defun jpl/terraform-copy-doc-url ()
  "Copy the URL documenting the resource at point."
  (interactive)
  (let* ((url (terraform--resource-url-at-point)))
    (kill-new url)
    (message "Copied URL: %s" url)))


(defvar jpl/terraform--prefix-to-provider-namespace
  '(
    ("template" . "hashicorp")
    ("aws" . "hashicorp")
    ))

(defun jpl/terraform--get-configured-resource-provider-namespace (provider)
  "Return provider namespace for PROVIDER."
  (cdr (assoc-string provider jpl/terraform--prefix-to-provider-namespace provider)))


(defun jpl/terraform--advice-get-resource-provider-namespace (orig-fun provider)
  "Return provider namespace for PROVIDER from configuration if
available, otherwise by running `terraform providers`."
  (or (jpl/terraform--get-configured-resource-provider-namespace provider)
      (funcall orig-fun provider)))

(advice-add
 'terraform--get-resource-provider-namespace
 :around #'jpl/terraform--advice-get-resource-provider-namespace)


;; (message (jpl/terraform--get-configured-resource-provider-namespace "aws"))





;;; Toggle hide/show subtrees
(defvar terraform-cycle-global-status 1)
(defun jpl/terraform-outline-toggle (&optional arg)
  "Visibility toggling for Terraform mode."
  (interactive "P")

  (cond
   ;; Move from overview to all
   ((eq terraform-cycle-global-status 2)
    (outline-show-all)
    (message "SHOW ALL")
    (setq terraform-cycle-global-status 1))
   ;; Defaults to overview
   (t
    (outline-hide-body)
    (message "OVERVIEW")
    (setq terraform-cycle-global-status 2)
    )))

