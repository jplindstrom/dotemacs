
(setq load-path (cons (expand-file-name (concat emacs-home-directory "elisp/expand-region")) load-path))
(require 'expand-region)
(global-set-key (kbd "C-'") 'er/expand-region)




