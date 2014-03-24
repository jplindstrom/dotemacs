(deftheme flatui-jpl
  "Fixed unreadable colors. 2014-03-21.")

(custom-theme-set-faces
 'flatui-jpl
 '(grep-edit-file-face ((t (:background "khaki" :weight bold))))
 '(header-line ((t (:inherit mode-line :background "light gray" :foreground "#16a085" :box (:line-width -1 :style released-button)))))

 '(magit-item-highlight ((t (:background "gainsboro"))))
 '(magit-log-head-label-remote ((t (:background "#3498db" :foreground "black" :box (:line-width 1 :color "#2980b9")))))
 '(magit-log-head-label-local  ((t (:background "#3498db" :foreground "black" :box (:line-width 1 :color "#2980b9")))))
 '(magit-log-head-label-head ((t (:background "#1abc9c" :foreground "black" :box (:line-width 1 :color "#16a085")))))

 '(cperl-array-face ((t (:foreground "Blue"))))
 '(cperl-hash-face ((t (:foreground "Red"))))

 '(font-lock-comment-face ((t (:foreground "dim gray")))))

(provide-theme 'flatui-jpl)
