

(setq version-control t)

(setq vc-make-backup-files t)

(setq delete-old-versions t)

(setq
 kept-new-versions 10
 kept-old-versions 10
 backup-directory-alist (quote (("^m:" . "") ("." . "~/backup/emacs-backup")))
 )

