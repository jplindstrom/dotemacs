


(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(require 'poly-markdown)

(add-to-list 'auto-mode-alist '("\\.text\\'" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . poly-markdown-mode))

