
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

(defun jpl/propertize-warn (message)
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

;; FIX change the function so that
;;
;; The jpl/llm-get-token-count function takes an argument "callback"
;; which is a function, which takes on argument which is the
;; "token-count-int"
;;
;; Don't call "shell-command-on-region". Instead start a process and
;; call the "ttok" asynchronoysly. Make it so that after the shell command has finished, it
;; calls the "callback" function with the token-count-int
(defun jpl/llm-get-token-count ()
  (interactive)
  (let* ((region (jpl/llm--get-region))
         (begin (car region))
         (end (car (cdr region)))
         (output-buffer (get-buffer-create "*temp*")))
    (shell-command-on-region begin end "ttok" output-buffer)
    (let* ((token-count (with-current-buffer output-buffer
                          (buffer-substring-no-properties
                           (point-min)
                           (progn (goto-char (point-max))
                                  (skip-chars-backward "\n")
                                  (point)))))
           (token-count-int (string-to-number token-count)))
      (kill-buffer output-buffer)
      token-count-int)))


(defun jpl/llm--get-region ()
  (let* ((begin (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max))))
    (list begin end)))


(require 'transient)

(transient-define-prefix jpl/llm-fix-transient ()
  "Run 'llm' using the 'fix' template on current selection or entire buffer."
  [["Arguments"
    ("-m" "model" "--model="
     :always-read t
     :init-value (lambda (obj) (oset obj value "4"))
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

(defun jpl/llm-message-token-info (token-count)
  (let* (
         (token-count-limit 4096)  ;; TODO: Use limit of current model
         (token-count-color
          (if (> token-count token-count-limit)
              (jpl/propertize-warn (number-to-string token-count)) token-count))
         )
    ;; Cost: (but turns out this isn't very interesting, so don't bother)
    ;; gpt-3.5-turbo: $0.002 / 1,000 tokens
    ;; gpt-4: is $0.03 / 1,000 tokens of input and $0.06 / 1,000 for output
    (message "Tokens: %s" token-count-color)))

(defun jpl/llm-fix ()
  (interactive)
  (jpl/llm-message-token-info (jpl/llm-get-token-count))
  (jpl/llm-fix-transient)
  )

(global-set-key (kbd "C-o a f") 'jpl/llm-fix)



;; Ideas
;; tiktokens count in the menu, and $ cost (x2 for output)
