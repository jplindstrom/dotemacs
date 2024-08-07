
;; https://codeberg.org/ideasman42/emacs-spell-fu
;; (use-package spell-fu)
(require 'spell-fu)
;; (spell-fu-global-mode nil)

(defun jpl/setup-spell-fu-mode ()
  (interactive)
  (setq spell-fu-word-delimit-camel-case t)

  (let ((filename (concat (getenv "HOME") "/.aspell.en.pws")))
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
    (spell-fu-dictionary-add
     (spell-fu-get-personal-dictionary "en-personal" filename)))

  (spell-fu-mode)

  (define-key evil-normal-state-local-map (kbd "C-o s C-n") 'spell-fu-goto-next-error)
  (define-key evil-normal-state-local-map (kbd "C-o s C-p") 'spell-fu-goto-previous-error)
  (define-key evil-normal-state-local-map (kbd "C-o s i") 'spell-fu-word-add)
  (define-key evil-normal-state-local-map (kbd "C-o s I") 'spell-fu-word-remove)
  (define-key evil-normal-state-local-map (kbd "C-o s b") 'spell-fu-buffer)
  (define-key evil-normal-state-local-map (kbd "C-o s f") 'ispell-word)
  (define-key evil-normal-state-local-map (kbd "C-o s m") 'spell-fu-mode)
  )


(add-hook 'org-mode-hook
  (lambda ()
    (setq spell-fu-faces-exclude
     '(org-block-begin-line
       org-block-end-line
       org-code
       org-date
       org-drawer org-document-info-keyword
       org-ellipsis
       org-link
       org-meta-line
       org-properties
       org-properties-value
       org-special-keyword
       org-src
       org-tag
       org-verbatim))
    (spell-fu-mode)))

(add-hook 'prog-mode-hook (lambda () (jpl/setup-spell-fu-mode)))
(add-hook 'text-mode-hook (lambda () (jpl/setup-spell-fu-mode)))
