


(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(require 'poly-markdown)

(add-to-list 'auto-mode-alist '("\\.text\\'" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . poly-markdown-mode))


(defun jpl/setup-markdown-mode ()
  (interactive)
  (visual-line-mode 1)

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
  )

(add-hook 'markdown-mode-hook 'jpl/setup-markdown-mode)
(add-hook 'gfm-mode-hook 'jpl/setup-markdown-mode)
