
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


;;; Render markdown for terraform resource
;; https://registry.terraform.io/v2/providers/hashicorp/aws?include=categories,moved-to,potential-fork-of,provider-versions,top-modules&include=categories%2Cmoved-to%2Cpotential-fork-of%2Cprovider-versions%2Ctop-modules&name=aws&namespace=hashicorp
;; https://registry.terraform.io/v2/provider-docs?filter%5Bprovider-version%5D=39513&filter%5Bcategory%5D=resources&filter%5Bslug%5D=lambda_function&page%5Bsize%5D=1
;;    RESOURCE=lambda_function; curl $(curl "https://registry.terraform.io/v2/provider-docs?filter%5Bprovider-version%5D=39513&filter%5Bcategory%5D=resources&filter%5Bslug%5D=$RESOURCE&page%5Bsize%5D=1" | jq '"https://registry.terraform.io" + .data[0].links.self' -r) | jq .data.attributes.content -r 
;;    RESOURCE=lambda_function; curl "https://registry.terraform.io/v2/provider-docs?filter%5Bprovider-version%5D=39513&filter%5Bcategory%5D=resources&filter%5Bslug%5D=$RESOURCE&page%5Bsize%5D=1"

(defun jpl/terraform-doc-markdown-for-resource-at-point ()
  (interactive)
  )


;; https://registry.terraform.io/providers/hashicorp/aws/latest/docs/resources/lambda_function
;; https://registry.terraform.io/v2/providers/hashicorp/aws?include=categories,moved-to,potential-fork-of,provider-versions,top-modules&include=categories%2Cmoved-to%2Cpotential-fork-of%2Cprovider-versions%2Ctop-modules&name=aws&namespace=hashicorp
;; https://registry.terraform.io/v2/provider-versions/40112?include=provider-docs%2Chas-cdktf-docs
;; https://registry.terraform.io/v2/provider-docs/2664154




(require 'json)
(require 'request)

(defun jpl/request-json (url)
  "Make a GET request to URL and parse the JSON response."
  (let (response-data)
    (request url
             :parser 'json-read
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (setq response-data data)))
             :sync t)
    response-data))

(defun jpl/extract-data-attributes-source-url (json)
  "Extract data.attributes.source-url from JSON."
  (cdr (assoc 'source-url (cdr (assoc 'attributes (assoc 'data json))))))

(defun jpl/extract-data-relationships-provider-versions-data-id (json)
  "Extract data.relationships.provider-versions.data.id from JSON."
  (let* ((data (assoc 'data json))
         (_ (message "JPL: data: %s" data))
         (relationships (assoc 'relationships data))
         (_ (message "JPL: relationships: %s" relationships))
         (provider-versions (assoc 'provider-versions relationships))
         (_ (message "JPL: provider-versions: %s" provider-versions))
         (data2 (elt (cdr (assoc 'data provider-versions)) 0)) ;; get the first element of the array
         (_ (message "JPL: data2: %s" data2))
         (id (cdr (assoc 'id data2))))
    (message "JPL: id: %s" id)
    id))

(defun jpl/extract-data-relationships-provider-docs-data-id (json)
  "Extract data.relationships.provider-docs.data.id from JSON."
  (let* ((data (assoc 'data json))
         (_ (message "JPL: data: %s" data))
         (relationships (assoc 'relationships data))
         (_ (message "JPL: relationships: %s" relationships))
         (provider-docs (assoc 'provider-docs relationships))
         (_ (message "JPL: provider-docs: %s" provider-docs))
         (data2 (assoc 'data provider-docs))
         (_ (message "JPL: data2: %s" data2))
         (id (cdr (assoc 'id data2))))
    (message "JPL: id: %s" id)
    id))

(defun jpl/extract-data-attributes-body (json)
  "Extract data.attributes.body from JSON."
  (cdr (assoc 'body (cdr (assoc 'attributes (assoc 'data json))))))

(defun jpl/extract-resource-name (url)
  "Extract the resource name from the URL."
  (car (last (split-string url "/"))))

(defun jpl/terraform-doc-url-to-markdown (url)
  "Fetch the Terraform documentation from URL and return it as Markdown."
  (let* ((resource-name (jpl/extract-resource-name url))
         (jpl1 (message "JPL: resource-name %s" resource-name))
         (json1 (jpl/request-json "https://registry.terraform.io/v2/providers/hashicorp/aws?include=categories,moved-to,potential-fork-of,provider-versions,top-modules&include=categories%2Cmoved-to%2Cpotential-fork-of%2Cprovider-versions%2Ctop-modules&name=aws&namespace=hashicorp"))
         (jpl2 (message "JPL: json1 %s" (json-encode json1)))
         (provider-versions-id (jpl/extract-data-relationships-provider-versions-data-id json1))
         (jpl3 (message "JPL: provider-versions-id %s" provider-versions-id))
         (json2 (jpl/request-json (format "https://registry.terraform.io/v2/provider-versions/%s?include=provider-docs%%2Chas-cdktf-docs" provider-versions-id)))
         (jpl22 (message "JPL: json2 %s" json2))
         (provider-docs-id (jpl/extract-data-relationships-provider-docs-data-id json2))
         (json3 (jpl/request-json (format "https://registry.terraform.io/v2/provider-docs/%s" provider-docs-id))))
    (jpl/extract-data-attributes-body json3)))



(message "JPL: markdown: %s" (jpl/terraform-doc-url-to-markdown "https://registry.terraform.io/providers/hashicorp/aws/latest/docs/resources/lambda_function"))



(defun jpl/terraform-resource-markdown-doc (provider-resource-name)
  "Fetch the Terraform documentation for PROVIDER-RESOURCE-NAME and display it in a buffer."
  (interactive "sEnter provider and resource name (e.g., aws_lambda_function): ")
  (let* ((provider-resource-list (split-string provider-resource-name "_"))
         (provider-name (car provider-resource-list))
         (resource-name (mapconcat 'identity (cdr provider-resource-list) "_"))
         (url (format "https://registry.terraform.io/providers/hashicorp/%s/latest/docs/resources/%s" provider-name resource-name))
         (markdown (jpl/terraform-doc-url-to-markdown url))
         (buffer (get-buffer-create provider-resource-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert markdown)
      (gfm-view-mode))
    (switch-to-buffer buffer)))
