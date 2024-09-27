

(defun jpl/increase-number-at-point (&optional increase)
  "Increase the number at point with the number that is the prefix argument"
  (interactive "P")
  (save-excursion
    (let ((num (or (thing-at-point 'number) 0)))
      (replace-match (number-to-string (+ num (prefix-numeric-value increase)))))))


(global-set-key (kbd "\C-o e n i") 'jpl/increase-number-at-point)



(global-set-key (kbd "M-C-q") 'unfill-toggle)

