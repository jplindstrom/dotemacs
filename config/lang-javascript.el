
(add-hook
 'js3-mode-hook
 (lambda ()
   (setq js3-auto-indent-p t
         js3-consistent-level-indent-inner-bracket t
         js3-curly-indent-offset 0
         js3-enter-indents-newline nil
         js3-expr-indent-offset 0
         js3-paren-indent-offset 0
         js3-square-indent-offset 0
         js3-indent-dots t
         js3-lazy-dots t
         ;; js3-lazy-commas t
         ;; js3-lazy-operators t

         evil-shift-width 2
         )))




