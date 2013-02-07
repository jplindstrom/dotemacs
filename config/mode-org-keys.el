
;; Ctrl-tab to change docs
(add-hook
 'org-mode-hook
 (function (lambda () (define-key org-mode-map [(control tab)]
                        'other-window))))


