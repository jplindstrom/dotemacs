
;;JPL: check if it works without this
;; Integrate the X11 clipboard witht the Gnome window manager
(setq x-select-enable-clipboard t)
(if (functionp 'x-cut-buffer-or-selection-value)
    (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))



