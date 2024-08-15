
(setq dired-dwim-target t)

(put 'dired-find-alternate-file 'disabled nil)

(require 'dired)
(define-key dired-mode-map (kbd "C-c p") 'wdired-change-to-wdired-mode)
