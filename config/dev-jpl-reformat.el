
;;; Remove when jpl-reformat is updated
(defun ensure-mark()
  "Deprecated function that is needed by jpl-reformat"
  (and (not mark-active) (set-mark-command nil)))

(require 'jpl-reformat)
(global-set-key (kbd "C-S-u") 'jpl-reformat-mark-enclosing-block)
(global-set-key (kbd "\C-o m a") 'jpl-reformat-align-enclosing-block)
(global-set-key (kbd "\C-o m p") 'jpl-reformat-parameter-list-toggle-multiple-single)

(fset 'jpl-reformat-collapse-param-list
   [?\C-r ?{ left ?\C-\; ?\C-w backspace ?\C-( ?\C-\; ?\C-w backspace left ?\C-o ?m ?p ?\C-o ?m ?p down ?\C-o ?m ?a])
(global-set-key (kbd "\C-o m c") 'jpl-reformat-collapse-param-list)


;; (require 'jpl-refactor)
;; (global-set-key (kbd "\C-o e a u") 'add-use)




