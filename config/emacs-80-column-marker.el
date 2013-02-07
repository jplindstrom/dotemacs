
(require 'column-marker)
(add-hook 'cperl-mode-hook (lambda () (interactive) (column-marker-1 80)))
(set-face-background 'column-marker-1 "bisque")




