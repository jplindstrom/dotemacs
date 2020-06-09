
;; CSS
(autoload 'css-mode "css-mode")
(setq auto-mode-alist
     (cons '("\\.css\\'" . css-mode) auto-mode-alist))
(setq cssm-indent-function #'cssm-c-style-indenter)


;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mc\\'" . web-mode))     ;; Mason
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))


(defun jpl/web-mode-element-close-and-indent ()
  "Close element and indent line"
  (interactive)
  (web-mode-element-close)
  (indent-according-to-mode)
  ;; (beginning-of-line-text
  )

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  ;; Clear out C-; in web-mode key map
  (define-key web-mode-map (kbd "C-;") nil)
  (define-key web-mode-map (kbd "C-; c") 'web-mode-comment-or-uncomment)

  (define-key web-mode-map (kbd "C-c /") 'jpl/web-mode-element-close-and-indent)

  ;; Evil mode keys
  (mapcar (lambda (state)
            (evil-declare-key state web-mode-map
              (kbd "M-h") 'web-mode-element-parent
              (kbd "M-l") 'web-mode-element-next

              (kbd "M-j") 'web-mode-element-sibling-next
              (kbd "M-k") 'web-mode-element-sibling-previous
              ))
          '(normal insert visual))
  )
(add-hook 'web-mode-hook 'my-web-mode-hook)


;; Use slim-mode to render emblem files (like haml for Ember)
(add-to-list 'auto-mode-alist '("\\.emblem\\'" . slim-mode))

