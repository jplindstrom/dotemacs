
; POD mode
(require 'pod-mode)
(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.pod$" . pod-mode))))
(add-hook 'pod-mode-hook 'font-lock-mode)



