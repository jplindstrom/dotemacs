

(set-face-attribute 'default nil :height (if (string-equal system-type "darwin") 140 120))

(setq inhibit-startup-message t)
(set-variable 'tool-bar-mode -1)
(tool-bar-mode -1)
(set-variable 'menu-bar-mode -1)
(menu-bar-mode -1)

;; Show the line-number of point at the mode line
(setq line-number-mode t)
(setq column-number-mode t)

;; get rid of yes-or-no questions - y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

(add-to-list 'custom-theme-load-path "~/elisp")
(load-theme 'flatui t)
(load-theme 'flatui-jpl t)




;; https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html

;;; Performant display
(setq-default bidi-paragraph-direction 'left-to-right)
(if (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))

;;; Performant longlines
(if (version<= "27.1" emacs-version)
    (global-so-long-mode 1))






;; When Tab-completion in minibuffer, don't display the completion
;; list across the bottom, display it in an unused window
(push '("\\*Completions\\*"
        (display-buffer-use-some-window display-buffer-pop-up-window)
        (inhibit-same-window . t))
      display-buffer-alist)

;; Display the completions list vertically
(setq completions-format (quote vertical))



;; (require 'spaceline-config)
;; (spaceline-spacemacs-theme)

;; (setq powerline-default-separator 'wave)


;; (require 'diminish)
;; ;; http://unicodelookup.com/#circled%20latin/1
;; (diminish 'git-gutter-mode "")
;; (diminish 'isearch-mode "")
;; (diminish 'anzu-mode "")
;; (diminish 'autopair-mode "")
;; (diminish 'indent-guide-mode "")

;; (diminish 'yas-minor-mode "ⓨ")
;; (diminish 'projectile-mode "ⓟ")
;; (diminish 'undo-tree-mode "ⓤ")
;; (diminish 'flymake-mode "ⓕ")

;; (diminish 'server-buffer-clients "")
;; (diminish 'flyspell-mode "ⓢ")
;; (diminish 'auto-fill-function "㉠")


