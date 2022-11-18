
;; YAML
(require 'yaml-mode)
(require 'yaml-pro)

(defun jpl/yaml-pro-next-into ()
  (interactive)
  (next-line)
  (evil-first-non-blank))

(defun jpl/enable-yaml-modes ()
  (interactive)
  (yaml-mode)
  (yaml-pro-mode)

  (define-key evil-normal-state-local-map (kbd "M-j") 'yaml-pro-next-subtree)
  (define-key evil-normal-state-local-map (kbd "M-k") 'yaml-pro-prev-subtree)
  (define-key evil-normal-state-local-map (kbd "M-h") 'yaml-pro-up-level)
  (define-key evil-normal-state-local-map (kbd "M-l") 'jpl/yaml-pro-next-into)

  ;;; Already set globally (maybe shouldn't indent?)
  ;; (define-key evil-normal-state-local-map (kbd "M-J") 'yaml-pro-move-subtree-up)
  ;; (define-key evil-normal-state-local-map (kbd "M-K") 'yaml-pro-move-subtree-down)
  (define-key evil-normal-state-local-map (kbd "M-H") 'evil-shift-left-line)
  (define-key evil-normal-state-local-map (kbd "M-L") 'evil-shift-right-line)

  (define-key evil-normal-state-local-map (kbd "C-M-j") 'yaml-pro-move-subtree-up)
  (define-key evil-normal-state-local-map (kbd "C-M-k") 'yaml-pro-move-subtree-down)
  (define-key evil-normal-state-local-map (kbd "C-M-h") 'yaml-pro-unindent-subtree)
  (define-key evil-normal-state-local-map (kbd "C-M-l") 'yaml-pro-indent-subtree)

  ;;; (define-key evil-normal-state-local-map (kbd "C-a") 'org-beginning-of-line)

  ;; C-c ' -- edit scalar

  ;; C-c C-j -- yaml-pro-consult-jump
  (define-key evil-normal-state-local-map (kbd "C-c C-g") 'yaml-pro-jump)
  )

(add-to-list 'auto-mode-alist '("\\.yml$"  . jpl/enable-yaml-modes))
(add-to-list 'auto-mode-alist '("\\.yaml$" . jpl/enable-yaml-modes))

