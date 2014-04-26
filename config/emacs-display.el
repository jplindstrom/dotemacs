

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



