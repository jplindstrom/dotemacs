
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(require 'poly-markdown)

(add-to-list 'auto-mode-alist '("\\.text\\'" . poly-gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . poly-gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . poly-gfm-mode))


(defun jpl/markdown-outline-toggle (&optional arg)
  "Visibility toggling for Markdown mode."
  (interactive "P")

  (cond
   ;; Move from overview to all
   ((eq markdown-cycle-global-status 2)
    (outline-show-all)
    (message "SHOW ALL")
    (setq markdown-cycle-global-status 1))
   ;; Defaults to overview
   (t
    (outline-hide-body)
    (message "OVERVIEW")
    (setq markdown-cycle-global-status 2)
    (markdown-outline-fix-visibility))))



(defun jpl/setup-markdown-mode ()
  (interactive)
  (visual-line-mode 1)
  (markdown-display-inline-images)

  ;; Tab - toggle heading outline
  (define-key evil-normal-state-local-map (kbd "C-i") 'jpl/markdown-outline-toggle)


  (define-key evil-normal-state-local-map (kbd "M-j") 'markdown-outline-next-same-level)
  (define-key evil-normal-state-local-map (kbd "M-k") 'markdown-outline-previous-same-level)
  (define-key evil-normal-state-local-map (kbd "M-h") 'markdown-up-heading)
  (define-key evil-normal-state-local-map (kbd "M-l") 'markdown-outline-next)

  (define-key evil-normal-state-local-map (kbd "M-J") 'jpl/move-line-down)
  (define-key evil-normal-state-local-map (kbd "M-K") 'jpl/move-line-up)
  (define-key evil-normal-state-local-map (kbd "M-H") 'markdown-promote)
  (define-key evil-normal-state-local-map (kbd "M-L") 'markdown-demote)

  (define-key evil-normal-state-local-map (kbd "C-M-j") 'markdown-move-subtree-down)
  (define-key evil-normal-state-local-map (kbd "C-M-k") 'markdown-move-subtree-up)
  (define-key evil-normal-state-local-map (kbd "C-M-h") 'markdown-promote-subtree)
  (define-key evil-normal-state-local-map (kbd "C-M-l") 'markdown-demote-subtree)

  (define-key evil-normal-state-local-map (kbd "C-o e t i") 'markdown-toc-generate-or-refresh-toc)
  (define-key evil-insert-state-local-map (kbd "C-o e t i") 'markdown-toc-generate-or-refresh-toc)
  (define-key evil-normal-state-local-map (kbd "C-o e t d") 'markdown-toc-delete-toc)
  (define-key evil-insert-state-local-map (kbd "C-o e t d") 'markdown-toc-delete-toc)
  )

(add-hook 'markdown-mode-hook 'jpl/setup-markdown-mode)
(add-hook 'gfm-mode-hook 'jpl/setup-markdown-mode)




(require 'markdown-toc)

