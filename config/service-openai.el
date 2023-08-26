
(defun jpl/gptel-get-api-key ()
  (getenv "OPENAI_API_KEY"))

;; https://github.com/karthink/gptel
(setq gptel-api-key 'jpl/gptel-get-api-key)
(global-set-key (kbd "C-o a a") 'gptel-send)


;; https://github.com/rksm/org-ai
;; Requires Emacs 28
;; (setq org-ai-openai-api-token (jpl/gptel-get-api-key))



(setq chatgpt-shell-openai-key (lambda () (jpl/gptel-get-api-key)))






(defun jpl/propertize-busy (message)
  "Return message with a vivid color and bold."
  (message (propertize message 'face '(:foreground "red" :weight "bold"))))

(defun jpl/llm-run-template (template model)
  "Run the specific 'llm' TEMPLATE and MODEL on current selection or entire buffer."
  (interactive "sTemplate name: \nsModel: ")
  (let* ((start (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (programming-language (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))
         (command (concat "llm -t " template " -m " model " -p programming_language " programming-language))
         (original-point (point)))
    (message (format "%s (%s)" (jpl/propertize-busy "Running llm...") command))
    (shell-command-on-region start end command nil t)
    (goto-char original-point)))

(require 'transient)
(transient-define-prefix jpl/llm-fix-transient ()
  "Run 'llm' using the 'fix' template on current selection or entire buffer."
  ["Arguments"
   ("-t" "template" ("-t" "--template") :value "fix")
   ("-m" "model" ("-m" "--model") :value "4")]
  [["Run llm"
    ("l" "llm" jpl/llm-fix-transient-llm-run-template)
    ("q" "quit" transient-quit-one)]])

(defun jpl/llm-fix-transient-llm-run-template (template model)
  "Run 'llm' using the 'TEMPLATE' and 'MODEL'."
  (interactive
   (list
    (transient-arg-value "-t" transient-current-cmd)
    (transient-arg-value "-m" transient-current-cmd)
   ))
  (jpl/llm-run-template template model)
  )

(defun jpl/llm-fix ()
  (interactive)
  (jpl/llm-fix-transient)
  )

(global-set-key (kbd "C-o a f") 'jpl/llm-fix)
