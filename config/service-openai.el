
(defun jpl/gptel-get-api-key ()
  (getenv "OPENAI_API_KEY"))

;; https://github.com/karthink/gptel
(setq gptel-api-key 'jpl/gptel-get-api-key)
(global-set-key (kbd "C-o a a") 'gptel-send)


;; https://github.com/rksm/org-ai
;; Requires Emacs 28
;; (setq org-ai-openai-api-token (jpl/gptel-get-api-key))



(setq chatgpt-shell-openai-key (lambda () (jpl/gptel-get-api-key)))






(defun llm-run-template (template)
  "Run the specific 'llm' TEMPLATE on current selection or entire buffer."
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (shell-command-on-region start end (concat "llm -t " template) nil t)))

(defun llm-fix ()
  "Run 'llm' using the 'fix' template on current selection or entire buffer."
  (interactive)
  (llm-run-template "fix"))


(global-set-key (kbd "C-o a f") 'llm-fix)
