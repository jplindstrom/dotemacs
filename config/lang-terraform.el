
(defun terraform-my-setup ()
  (company-mode)

  ;; Company completion trigger
  (local-set-key (kbd "C-o C-c") 'company-complete))


(add-hook 'terraform-mode-hook 'terraform-my-setup)
(setq interpreter-mode-alist
      (cons '("terraform" . terraform-mode) interpreter-mode-alist))


