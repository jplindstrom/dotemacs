
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))


(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-m")



(require 'js-doc)

(add-hook 'js2-mode-hook
          #'(lambda ()
              (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
              ;; (define-key js2-mode-map "@" 'js-doc-insert-tag)
	      ))


;; Mocha testing

(setq mocha-reporter "spec")
(setq mocha-options "--recursive --exit")

(global-set-key (format "%s\C-j\C-t" ps/key-prefix) 'mocha-test-at-point)
(global-set-key (format "%s\C-jtf" ps/key-prefix) 'mocha-test-file)
(global-set-key (format "%s\C-jtp" ps/key-prefix) 'mocha-test-project)
(global-set-key (format "%s\C-jtr" ps/key-prefix) 'recompile)


;; Parsing is super slow on just 400 loc, so delay syntax check while typing
(setq js2-idle-timer-delay 3)

(setq inhibit-compacting-font-caches t)
