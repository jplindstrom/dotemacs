
(defun jpl/gptel-get-api-key ()
  (getenv "OPENAI_API_KEY"))

;; https://github.com/karthink/gptel
(setq gptel-api-key 'jpl/gptel-get-api-key)
(global-set-key (kbd "C-o a a") 'gptel-send)
(global-set-key (kbd "C-o a c") 'gptel)        ;; Chat


;; https://github.com/rksm/org-ai
;; Requires Emacs 28
;; (setq org-ai-openai-api-token (jpl/gptel-get-api-key))



(setq chatgpt-shell-openai-key (lambda () (jpl/gptel-get-api-key)))






(defun jpl/propertize-busy (message)
  "Return message with a vivid color and bold."
  (message (propertize message 'face '(:foreground "red" :weight "bold"))))

(defun jpl/get-programming-language ()
  "Return the current programming language based on the major mode."
  (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))

(defun jpl/llm-run-template (template model programming-language)
  "Run the specific 'llm' TEMPLATE, MODEL, and
PROGRAMMING-LANGUAGE on current selection or entire buffer."
  (interactive "sTemplate name: \nsModel: \nsProgramming language: ")
  (let* ((start (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (command (concat "llm -t " template " -m " model " -p programming_language " programming-language))
         (original-point (point)))
    (message (format "%s (%s)" (jpl/propertize-busy "Running llm...") command))
    (shell-command-on-region start end command nil t)
    (goto-char original-point)))



(require 'transient)

(transient-define-prefix jpl/llm-fix-transient ()
  "Run 'llm' using the 'fix' template on current selection or entire buffer."
  [["Arguments"
    ("-m" "model" "--model="
     :always-read t
     :init-value (lambda (obj) (oset obj value "fix"))
     )
    ("-t" "model" "--template="
     :always-read t
     :init-value (lambda (obj) (oset obj value "fix"))
     )
    ("-l" "programming language" "--programming-language="
     :always-read t
     :init-value (lambda (obj) (oset obj value (jpl/get-programming-language)))
     )
    ]]
  [["Run"
    ("l" "llm" jpl/llm-fix-transient:llm)]]
  )

(transient-define-suffix jpl/llm-fix-transient:llm (&optional args)
  "Show this command"
  :description "current command"
  (interactive (list (transient-args transient-current-command)))
  (transient-save)
  (let* ((template (transient-arg-value "--template=" args))
         (model (transient-arg-value "--model=" args))
         (programming-language (transient-arg-value "--programming-language=" args)))
    (jpl/llm-run-template template model programming-language)
    )
  )

(defun jpl/llm-fix ()
  (interactive)
  (jpl/llm-fix-transient))

(global-set-key (kbd "C-o a f") 'jpl/llm-fix)
