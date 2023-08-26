
(defun jpl/gptel-get-api-key ()
  (getenv "OPENAI_API_KEY"))

;; https://github.com/karthink/gptel
(setq gptel-api-key 'jpl/gptel-get-api-key)
(global-set-key (kbd "C-o a a") 'gptel-send)


;; https://github.com/rksm/org-ai
;; Requires Emacs 28
;; (setq org-ai-openai-api-token (jpl/gptel-get-api-key))



(setq chatgpt-shell-openai-key (lambda () (jpl/gptel-get-api-key)))






;; FIX: add a doc string for this, with the outcome of the code. "llm
;; -t fix" will fix/action all "FIX" comments and send back the
;; full output.
(defun llm-fix ()
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (shell-command-on-region start end "llm -t fix" nil t)))
