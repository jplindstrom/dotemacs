

;; https://idiomdrottning.org/bad-emacs-defaults
(setq backup-by-copying t)

;; https://kb.iu.edu/d/acxl
;; Ensure hard links remain pointing to the inode when editing
(setq backup-by-copying-when-linked t)


(setq version-control t)

(setq vc-make-backup-files t)

(setq delete-old-versions t)

(setq
 kept-new-versions 10
 kept-old-versions 10
 backup-directory-alist (quote (("^m:" . "") ("." . "~/elisp/emacs-backup-files")))
 )

