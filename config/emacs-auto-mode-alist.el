

;; JPL: Is this needed any more???
(setq auto-mode-alist
      (append '(("\\.el$" . emacs-lisp-mode)
				("\\.py$" . python-mode)
				("\\.h$" . c++-mode)
				)
			  auto-mode-alist))

