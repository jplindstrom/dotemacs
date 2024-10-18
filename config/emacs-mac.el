

(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)

  ;; Unbind common mistypes of the Cmd button
  (global-set-key (kbd "s-x") nil)
  (global-set-key (kbd "s-h") nil)
  (global-set-key (kbd "s-j") nil)
  (global-set-key (kbd "s-k") nil)
  (global-set-key (kbd "s-l") nil)
  )


