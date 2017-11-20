
;; Snippets

(defun my-yas-perl-next-sub-name ()
  (interactive)
  (save-excursion
    (if (search-forward-regexp "sub +\\([a-z0-9_]+\\)" nil t)
        (match-string 1)
      "")))

(defun my-yas-perl-package-name ()
  (interactive)
  (ps/edit-copy-package-name)
  (or
   (ps/current-package-name)
   (ps/package-name-from-file)))

(require 'yasnippet)
(yas-global-mode t)
(yas-load-directory (concat emacs-home-directory "elisp/snippets"))

;; (yas-global-mode 1)
;; (setq yas/also-auto-indent-first-line t)



