
(defun jpl/gptel-get-api-key ()
  (getenv "OPENAI_API_KEY"))

;; https://github.com/karthink/gptel
(setq gptel-api-key 'jpl/gptel-get-api-key)
(global-set-key (kbd "C-o a a") 'gptel-send)


;; https://github.com/rksm/org-ai
;; Requires Emacs 28
;; (setq org-ai-openai-api-token (jpl/gptel-get-api-key))



(setq chatgpt-shell-openai-key (lambda () (jpl/gptel-get-api-key)))






(defun jpl/message-busy (message)
  "Display MESSAGE in the minibuffer with orange color."
  (message (propertize message 'face '(:foreground "orange"))))

(defun jpl/llm-run-template (template)
  "Run the specific 'llm' TEMPLATE on current selection or entire buffer.
Specify the llm variable 'major_mode'"
  (interactive)
  (let* ((start (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (major-mode-string (symbol-name major-mode))
         (command (concat "llm -t " template " -p major_mode " major-mode-string))
         (original-point (point))
         )
    (jpl/message-busy "Running llm...")
    (shell-command-on-region start end command nil t)
    (goto-char original-point)
    ))

(defun jpl/llm-fix ()
  "Run 'llm' using the 'fix' template on current selection or entire buffer."
  (interactive)
  (jpl/llm-run-template "fix"))


(global-set-key (kbd "C-o a f") 'jpl/llm-fix)

