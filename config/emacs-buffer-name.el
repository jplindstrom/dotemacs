
;; When there are multiple buffers with the similar prefix like
;; customer/index.js, product/index.js
;;
;; Don't display "index.js<product>", display "product/index.js"

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
