
;; https://github.com/clojure-emacs/cider

(require 'cider)


(add-hook 'cider-mode-hook #'eldoc-mode)

(setq nrepl-log-messages t)


;; (add-hook 'cider-repl-mode-hook #'company-mode)
;; (add-hook 'cider-mode-hook #'company-mode)

