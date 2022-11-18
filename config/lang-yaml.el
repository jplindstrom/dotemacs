
;; YAML
(require 'yaml-mode)
(require 'yaml-pro)

(defun jpl/yaml-pro-next-into ()
  (interactive)
  (next-line)
  (evil-first-non-blank))

(defun jpl/move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line)
  (previous-line)
  )

(defun jpl/move-line-down ()
  (interactive)
  (next-line)
  (transpose-lines 1)
  (previous-line))

(defun jpl/enable-yaml-modes ()
  (interactive)
  (yaml-mode)
  (yaml-pro-mode)

  (define-key evil-normal-state-local-map (kbd "M-j") 'yaml-pro-next-subtree)
  (define-key evil-normal-state-local-map (kbd "M-k") 'yaml-pro-prev-subtree)
  (define-key evil-normal-state-local-map (kbd "M-h") 'yaml-pro-up-level)
  (define-key evil-normal-state-local-map (kbd "M-l") 'jpl/yaml-pro-next-into)

  (define-key evil-normal-state-local-map (kbd "M-J") 'jpl/move-line-down)
  (define-key evil-normal-state-local-map (kbd "M-K") 'jpl/move-line-up)
  (define-key evil-normal-state-local-map (kbd "M-H") 'evil-shift-left-line)
  (define-key evil-normal-state-local-map (kbd "M-L") 'evil-shift-right-line)

  (define-key evil-normal-state-local-map (kbd "C-M-j") 'yaml-pro-move-subtree-down)
  (define-key evil-normal-state-local-map (kbd "C-M-k") 'yaml-pro-move-subtree-up)
  (define-key evil-normal-state-local-map (kbd "C-M-h") 'yaml-pro-unindent-subtree)
  (define-key evil-normal-state-local-map (kbd "C-M-l") 'yaml-pro-indent-subtree)


  (define-key evil-normal-state-local-map (kbd "C-S-H") 'yaml-pro-fold-at-point)
  (define-key evil-normal-state-local-map (kbd "C-S-L") 'yaml-pro-unfold-at-point)

  ;; C-c ' -- edit scalar

  ;; C-c C-j -- yaml-pro-consult-jump
  (define-key evil-normal-state-local-map (kbd "C-c C-g") 'yaml-pro-jump)


  ;;; Not sure this is a universally good idea
  ;; (setq evil-shift-width . 2)

  ;; possibly create a .dir-locals-el file with
  ;; ((yaml-mode . ((evil-shift-width . 2))))


  ;; Use different indentation marker
  (highlight-indentation-current-column-mode)
  (indent-guide-mode 'toggle)
  )

(add-to-list 'auto-mode-alist '("\\.yml$"  . jpl/enable-yaml-modes))
(add-to-list 'auto-mode-alist '("\\.yaml$" . jpl/enable-yaml-modes))

