
;; JPL: remove anything.el etc

(require 'helm-config)
(require 'helm-buffers)
(require 'helm-files)
(require 'helm-regexp)
(require 'helm-ls-git)
(helm-occur-init-source)

(setq helm-ff-transformer-show-only-basename nil)

(defun my-helm-goto ()
  "heml-mode mini set"
  (interactive)
  (helm-other-buffer '(
                       ;; ;; Projectile
                       helm-c-source-projectile-recentf-list
                       helm-c-source-projectile-files-list
                       helm-c-source-projectile-buffers-list

                       ;; Default sources
                       helm-source-buffers-list
                       helm-source-recentf
                       helm-source-files-in-current-dir

                       ;; Additional sources
                       helm-c-source-locate
                       )
                     "*my helm goto*"))

(defun my-helm-search ()
  "heml-mode mini set"
  (interactive)
  (require 'helm-files)
  (helm-other-buffer '(
                       helm-c-source-occur
                       helm-c-source-moccur
                       )
                     "*my helm search*"))


(global-set-key (kbd "s-SPC") 'my-helm-goto)
(global-set-key (kbd "C-s-SPC") 'my-helm-search)

(helm-mode 1)



