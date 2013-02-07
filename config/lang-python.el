
(defun py-my-setup ()
 (setq py-indent-offset 4)
 (make-local-variable 'outline-regexp)
 (make-local-variable 'outline-heading-end-regexp)
 (setq outline-regexp "[ \t]*\\(def\\|class\\)\\>")
 (setq outline-heading-end-regexp ":[ \t]*[\r\n]")
 (outline-minor-mode 1))

(add-hook 'python-mode-hook 'py-my-setup)
(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))


