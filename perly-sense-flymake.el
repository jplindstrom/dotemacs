;;;; Flymake support for PerlySense

;; Flymake is included in Emacs 22 (or available from
;; http://flymake.sourceforge.net/, put flymake.el somewhere in your
;; load-path.



(require 'flymake)


(defun flymake-perlysense-init ()
  (let* ((temp-file
          (flymake-init-create-temp-buffer-copy
           'flymake-create-temp-inplace))
         (local-file
          (file-relative-name
           temp-file
           (file-name-directory buffer-file-name))))
    (list "perly_sense" (list "flymake_file" (format "--file=%s" local-file)))))


(setq
 flymake-allowed-file-name-masks
 (append
  '(("\\.pl\\'" flymake-perlysense-init))
  '(("\\.pm\\'" flymake-perlysense-init))
  '(("\\.t\\'" flymake-perlysense-init))
  flymake-allowed-file-name-masks))


(add-hook 'cperl-mode-hook 'flymake-mode t)



;;END
