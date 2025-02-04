

(require 'helm-for-files)
(require 'helm-ls-git)
(require 'helm-projectile)
(require 'helm-git-grep)


(setq helm-projectile-sources-list
  '(
    helm-source-projectile-recentf-list
    helm-source-projectile-files-list
    helm-source-projectile-projects

    helm-source-recentf
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


(global-set-key (kbd "s-SPC") 'helm-projectile)    ;; For Linux

;; for macOS, where Cmd-SPC is taken by Spotlight
(global-set-key (kbd "s-C-SPC") 'helm-projectile)

;; (helm-mode 1)
