
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

;; FIX: Write an Emacs lisp "transient" menu (not using a hydra, but
;; the "transient" package that's used by Magit) with:
;; * Description "Run llm to do stuff"
;; * Option "template" default "fix"
;; * Option "model" default "4"
;; * Action "llm" to call 'jpl/llm-run-template' with appropriate options
;; * Action "quit"
;; Use the menu in `jpl/llm-fix' to allow the user to select/confirm the options before running the comment.
(defun jpl/llm-fix ()
  "Run 'llm' using the 'fix' template on current selection or entire buffer."
  (interactive)
  (jpl/llm-run-template "fix" "4"))

(global-set-key (kbd "C-o a f") 'jpl/llm-fix)
