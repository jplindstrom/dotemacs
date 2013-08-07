



(global-set-key "\C-h" 'help-command)
(global-set-key "\C-ha" 'apropos)	; B{ttre {n command-apropos

(global-set-key "\M-L" 'downcase-word)
(global-set-key "\M-l" 'goto-line)
(global-set-key "\M-v" 'scroll-down)



(global-set-key (kbd "\C-x \C-m") 'call-last-kbd-macro)



;; Fix delete key when using PuTTY and X-windows
;; use view lossage to see what keysym you are getting
;; http://www.ocf.berkeley.edu/~tmtong/howto/delete.php
;; (global-set-key (kbd "<backspace>") 'backward-delete-char)
(global-set-key (kbd "<delete>") 'delete-char)



;; Ctrl-tab to change docs
(global-set-key [C-tab] 'other-window)

;; Ctrl-Meta-tab to change frames
(global-set-key [C-M-tab] 'other-frame)



;; Disable Scoll Lock warnings
(global-set-key [scroll-lock] 'noop)
;;(global-set-key [scroll-lock] nil)
;;(global-set-key [scroll-lock] '(quote nil))
;;(global-set-key (kbd "<Scroll_Lock>") '(lambda () (interactive) nil))


(global-set-key (kbd "C-; q i") 'insert-register)


