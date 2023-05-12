


(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(require 'poly-markdown)

(add-to-list 'auto-mode-alist '("\\.text\\'" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . poly-markdown-mode))


(defun jpl/setup-markdown-mode ()
  (visual-line-mode 1)
  

  )

(add-hook 'markdown-mode-hook 'jpl/setup-markdown-mode)
(add-hook 'gfm-mode-hook 'jpl/setup-markdown-mode)

