

(require 'helm-ls-git)
(require 'helm-projectile)
(require 'helm-git-grep)


(setq helm-projectile-sources-list
  '(
    helm-source-recentf
    helm-source-projectile-files-list
    helm-source-projectile-projects
    helm-source-buffers-list
    ;;helm-source-projectile-buffers-list

    ;; helm-source-ls-git-status
    helm-ls-git-branches-source
    ;; helm-source-ls-git-buffers
    helm-source-ls-git
    ;; helm-ls-git-stashes-source
    ;; helm-ls-git-create-branch-source

    helm-git-grep-source
    ;; helm-git-grep-submodule-source
    ))


(global-set-key (kbd "s-SPC") 'helm-projectile)

;; (helm-mode 1)



