
(defmacro with-library (symbol &rest body)
  "Try and require SYMBOL, if all goes well, then eval body."
  `(if (or (featurep ,symbol)
           (require ,symbol nil t))
       (progn ,@body)
       (progn
         (switch-to-buffer (get-buffer-create "*with-library errors*"))
         (insert (format "Failed to load '%s'\n"
                         (symbol-name ,symbol))))))
(put 'with-library 'lisp-indent-function 1)


