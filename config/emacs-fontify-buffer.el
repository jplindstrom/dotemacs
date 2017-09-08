

;;Colors
;(cond (window-system
;        (global-set-key "\C-cF" 'font-lock-mode) ; Slå av/på font-lock
;        (set-cursor-color "Grey80")
;        (set-background-color "#506050")
;        (set-foreground-color "Grey80")
;        (set-face-background 'modeline "Grey80")
;        (set-face-foreground 'modeline "#506050")
;        (set-face-background 'region "#605060")

;        (setq initial-frame-alist (append initial-frame-alist
; 					 '((internal-border-width . 1))))
;        ))

;; Set color scheme (set lconfig-dark-bg-scheme to t for reverse color scheme)
;(defconst color-scheme 'dark)
;(defconst foreground-color "Black")
;(defconst background-color "Gray65")
;(defconst cursor-color "red3")
;(defconst pointer-color "white")
;
;; (if (featurep 'xemacs)
;;     (let ((frame (selected-frame)))
;;       (set-face-foreground 'default foreground-color)
;;       (set-face-background 'default background-color)
;;       (setq frame-background-mode color-scheme)
;;       color-scheme
;;       (set-frame-property frame
;;                           'custom-properties
;;                           (mapcar (lambda (symbol)
;;                                     (if (eql symbol 'light)
;;                                         'dark
;;                                       symbol))
;;                                   (frame-property frame
;;                                                   'custom-properties))))
;;   (progn
;;      (add-to-list 'default-frame-alist '(foreground-color . "Black"))
;;      (add-to-list 'default-frame-alist '(background-color . "Gray85"))
;;      (add-to-list 'default-frame-alist '(cursor-color . "red3"))
;;      (add-to-list 'default-frame-alist '(background-mode . dark))
;;      (set-cursor-color cursor-color)
;;      (set-mouse-color pointer-color))
;; )

;; ;; Setup font-lock syntax coloring package
;; (defconst lconfig-font-lock-faces
;;   (list '(font-lock-builtin-face
;;           ((((class color) (background dark)) (:foreground "cyan" :bold t))
;;            (((class color)) (:foreground "DarkGreen" :bold t))))
;;         '(font-lock-comment-face
;;           ((((class color) (background dark)) (:foreground "LightPink"))
;;            (((class color)) (:foreground "FireBrick"))))
;;         '(font-lock-constant-face
;;           ((((class color) (background dark)) (:foreground "SpringGreen"))
;;            (((class color)) (:foreground "Gray40"))))
;;         '(font-lock-doc-string-face
;;           ((((class color) (background dark)) (:foreground "SpringGreen"))
;;            (((class color)) (:foreground "Gray40"))))
;;         '(font-lock-function-name-face
;;           ((((class color) (background dark)) (:foreground "wheat3"))
;;            (((class color)) (:foreground "ForestGreen"))))
;;         '(font-lock-keyword-face
;;           ((((class color) (background dark)) (:foreground "SkyBlue" :bold t))
;;            (((class color)) (:foreground "ForestGreen" :bold t))))
;;         '(font-lock-preprocessor-face
;;           ((((class color) (background dark)) (:foreground "SkyBlue"))
;;            (((class color)) (:foreground "gray40"))))
;;         '(font-lock-reference-face
;;           ((((class color) (background dark)) (:foreground "yellow"))
;;            (((class color)) (:foreground "maroon4"))))
;;         '(font-lock-string-face
;;           ((((class color) (background dark)) (:foreground "SpringGreen"))
;;            (((class color)) (:foreground "DarkRed"))))
;;         '(font-lock-type-face
;;           ((((class color) (background dark)) (:foreground "orange1"))
;;            (((class color)) (:foreground "maroon4"))))
;;         '(font-lock-variable-name-face
;;           ((((class color) (background dark)) (:foreground "yellow"))
;;            (((class color)) (:foreground "SaddleBrown"))))
;;         '(font-lock-warning-name-face
;;           ((((class color) (background dark)) (:foreground "DarkOrange"))
;;            (((class color)) (:foreground "DarkOrange")))))
;; If possible set up a custom color scheme, otherwise turn colors off


;; JPL: can be removed?
(autoload 'custom-set-faces "font-lock" "Set the color scheme" t)
(autoload 'font-lock-fontify-buffer "font-lock" "Fontify Buffer" t)
(condition-case err
    (progn (apply 'custom-set-faces lconfig-font-lock-faces)
           (add-hook 'c-mode-common-hook 'font-lock-fontify-buffer)
           (add-hook 'emacs-lisp-mode-hook 'font-lock-fontify-buffer)
           )
  (error (progn
           (message "Could not customize colors, disabling colored fonts.")
           (setq-default font-lock-auto-fontify t))))


