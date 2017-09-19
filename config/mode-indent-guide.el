
(require 'indent-guide)
(indent-guide-global-mode)

(set-face-foreground 'indent-guide-face "gray")
(setq indent-guide-char "|")

(setq indent-guide-show-beyond-column 0)
(setq indent-guide-recursive nil)


(custom-set-variables
 '(indent-guide-inhibit-modes (quote (dired-mode org-mode)))
 )
