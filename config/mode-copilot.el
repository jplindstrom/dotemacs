
;; (setq max-lisp-eval-depth 2000)  ;; default 800
;; (setq max-specpdl-size 3000)     ;; defaut 1600
(require 'copilot)

(setq copilot-idle-delay 0.5)

(defun jpl/maybe-enable-copilot-mode ()
  "Enable copilot-mode if this is not an org-mode source block"
  (interactive)
  (when (not (jpl/is-org-source-block))
    (copilot-mode)))

;; Don't use global-copilot-mode, it seems broken
;; https://github.com/copilot-emacs/copilot.el/issues/226
(dolist (hook '(typescript-mode-hook
                js-mode-hook
                js2-mode-hook
                python-mode-hook
                cperl-mode-hook
                shell-script-mode-hook
                emacs-list-mode-hook
                terraform-mode-hook
                json-mode-hook
                yaml-mode-hook
                ))
  (add-hook hook 'jpl/maybe-enable-copilot-mode))


;; https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/
(define-key global-map (kbd "C-o c C") #'copilot-mode)


(defun rk/copilot-complete-or-accept ()
  "Command that either triggers a completion or accepts one if one
is available. Useful if you tend to hammer your keys like I do."
  (interactive)
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion)
        ;; (open-line 1)
        ;; (next-line)
        )
    (copilot-complete)))

(define-key copilot-mode-map (kbd "C-o c n") #'copilot-next-completion)
(define-key copilot-mode-map (kbd "C-o c p") #'copilot-previous-completion)
(define-key copilot-mode-map (kbd "C-o c l") #'copilot-accept-completion-by-word)
(define-key copilot-mode-map (kbd "C-o c j") #'copilot-accept-completion-by-line)
(define-key global-map (kbd "C-o c c") #'rk/copilot-complete-or-accept)




(defun rk/copilot-quit ()
  "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
cleared, make sure the overlay doesn't come back too soon."
  (interactive)
  (condition-case err
      (when copilot--overlay
        (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
          (setq copilot-disable-predicates (list (lambda () t)))
          (copilot-clear-overlay)
          (run-with-idle-timer
           1.0
           nil
           (lambda ()
             (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
    (error handler)))

(advice-add 'keyboard-quit :before #'rk/copilot-quit)

