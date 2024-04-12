

(defun jpl/increase-number-at-point (&optional increase)
  "Increase the number at point with the number that is the prefix argument"
  (interactive "P")
  (let ((num (or (thing-at-point 'number) 0)))
    (replace-match (number-to-string (+ num (prefix-numeric-value increase))))))


(global-set-key (kbd "\C-o e n") 'jpl/increase-number-at-point)
