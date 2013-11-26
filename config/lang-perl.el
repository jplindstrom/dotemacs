

(autoload 'perl-mode "cperl-mode" "alternate mode for editing Perl programs" t)


(setq auto-mode-alist
     (append '(("\\.\\([t]|pbl\\)$" . perl-mode))  auto-mode-alist ))


(defalias 'perl-mode 'cperl-mode)

(setq cperl-invalid-face nil)  ;Don't underline trailing whitespace

(add-hook 'cperl-mode-hook 'n-cperl-mode-hook t)
(defun n-cperl-mode-hook ()
;  (setq cperl-extra-newline-before-brace t)
  (setq indent-tabs-mode nil)
  (setq indicate-empty-lines t)
;  (set-face-background 'cperl-array-face "wheat")
;  (set-face-background 'cperl-hash-face "wheat")
  )

(add-to-list 'auto-mode-alist '("\\.t$" . cperl-mode))



;; Alt-up/down arrows to jump between subs
(global-set-key [(meta up)] 'beginning-of-defun)
(global-set-key [(meta down)] 'end-of-defun)



(setq cperl-brace-imaginary-offset 0
      cperl-brace-offset 0
      cperl-close-paren-offset -4
      cperl-continued-brace-offset 0
      cperl-continued-statement-offset 4
      cperl-merge-trailing-else nil
      cperl-indent-level 4
      cperl-indent-parens-as-block t
      cperl-auto-newline nil
      cperl-close-paren-offset -4
      cperl-continued-brace-offset 0
      cperl-continued-statement-offset 4
      cperl-electric-parens nil
      cperl-electric-parens-string "{[]}<"
      cperl-indent-level 4
      cperl-indent-parens-as-block t
      cperl-label-offset 0
      perl-continued-statement-offset 4
      perl-tab-always-indent t
      )


