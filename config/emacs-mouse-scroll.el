

(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(2.1))

; Windows
(global-set-key [wheel-up]'(lambda ()(interactive)(scroll-down 4)))
(global-set-key [wheel-down]'(lambda ()(interactive)(scroll-up 4)))

; X11
(global-set-key [mouse-4]'(lambda ()(interactive)(scroll-down 4)))
(global-set-key [mouse-5]'(lambda ()(interactive)(scroll-up 4)))






