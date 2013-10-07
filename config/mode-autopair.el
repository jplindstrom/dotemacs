
(require 'autopair)

(add-hook 'graphviz-dot-mode-hook #'(lambda () (setq autopair-dont-activate t)))

(autopair-global-mode) ;; to enable in all buffers
(setq autopair-autowrap t)
