
;; Snippets

(defun my-yas-perl-next-sub-name ()
  (interactive)
  (save-excursion
    (if (search-forward-regexp "sub +\\([a-z0-9_]+\\)" nil t)
        (match-string 1)
      "")))

(setq load-path (cons (expand-file-name (concat emacs-home-directory "elisp/yasnippet")) load-path))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat emacs-home-directory "elisp/snippets"))



