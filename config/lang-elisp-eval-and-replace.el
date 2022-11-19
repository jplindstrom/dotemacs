
;; http://emacs.wordpress.com/2007/01/17/eval-and-replace-anywhere/
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
;; (global-set-key (kbd "\C-p e") 'eval-and-replace)



;; Go to function definition
(fset 'jpl/elisp-goto-defun
   (kmacro-lambda-form [?\C-h ?f return C-tab tab return C-tab ?\C-x ?k return C-tab] 0 "%d"))

(defun jpl/setup-elisp-mode ()
  (interactive)
  (local-set-key "\C-o\C-g" 'jpl/elisp-goto-defun))

(add-hook 'emacs-lisp-mode-hook 'jpl/setup-elisp-mode)


