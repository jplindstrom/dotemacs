


(require 'helm-config)
(require 'helm-buffers)
(require 'helm-files)
(require 'helm-regexp)
(require 'helm-ls-git)
(require 'helm-projectile)
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
                       ;; helm-source-files-in-current-dir ;; Not that interesting

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

                       ;; Full file search
                       helm-source-buffers-list
                       helm-source-recentf
                       helm-source-files-in-current-dir
                       helm-c-source-locate
                       )
                     "*my helm search*"))



;; Copy-pasted, except remove the ridiculous 15 limit which can easily
;; hide actual matches
;; TODO: fork and fix
(setq helm-c-source-projectile-files-list
  `((name . "Projectile files list")
    ;; Needed for filenames with capitals letters.
    (disable-shortcuts)
    (init . (lambda ()
              (with-current-buffer (helm-candidate-buffer 'local)
                (insert
                 (helm-c-projectile-candidate-buffer-content)))))
    (candidates-in-buffer)
    (candidate-number-limit . 150)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (type . file)
    (action . (lambda (candidate)
                (find-file (projectile-expand-root candidate))))))




(global-set-key (kbd "s-SPC") 'my-helm-goto)
(global-set-key (kbd "C-s-SPC") 'my-helm-search)

;; Try turning it on and off again (to load and init all autoloads)
(helm-mode 1)
(helm-mode -1)

