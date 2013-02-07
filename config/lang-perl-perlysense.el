

;; *** PerlySense Config ***

;; ** PerlySense **
;; The PerlySense prefix key (unset only if needed)
(global-unset-key "\C-o")
(setq ps/key-prefix "\C-o")


;; ** Flymake **
;; Load flymake if t
;; Flymake must be installed.
;; It is included in Emacs 22, or available from
;;   http://flymake.sourceforge.net/
;; Put flymake.el somewhere in your load-path.
(setq ps/load-flymake t)
;; Note: more flymake config below, after loading PerlySense


;; *** PerlySense load (don't touch) ***
(setq ps/external-dir (shell-command-to-string "perly_sense external_dir"))
(let ((dir "/home/j.lindstrom/dev/personal/p5-Devel-PerlySense/source/lib/Devel/PerlySense/external"))
  (if (file-directory-p dir) (setq ps/external-dir dir)))
(if (string-match "Devel.PerlySense.external" ps/external-dir)
    (progn
      (message
       "PerlySense elisp files  at (%s) according to perly_sense, loading..."
       ps/external-dir)
      (setq load-path (cons
                       (expand-file-name
                        (format "%s/%s" ps/external-dir "emacs")
                        ) load-path))
      (load "perly-sense")
      )
  (message "Could not identify PerlySense install dir.
Is Devel::PerlySense installed properly?
Does 'perly_sense external_dir' give you a proper directory? (%s)" ps/external-dir)
  )


;; ** Flymake Config **

;; If you only want syntax check whenever you save, not continously
(setq flymake-no-changes-timeout 9999)
(setq flymake-start-syntax-check-on-newline nil)

;; ** Code Coverage Visualization **
;; If you have a Devel::CoverX::Covered database handy and want to
;; display the sub coverage in the source, set this to t
(setq ps/enable-test-coverage-visualization nil)

;; Emacs named colors: http://www.geocities.com/kensanata/colors.html
;; These colors work fine with a white X11 background. They may not look
;; that great on a console with the default color scheme.
(set-face-background 'flymake-errline "AntiqueWhite2")
(set-face-background 'flymake-warnline "lavender")
(set-face-background 'dropdown-list-face "lightgrey")
(set-face-foreground 'dropdown-list-face "black")
(set-face-background 'dropdown-list-selection-face "grey")
(set-face-foreground 'dropdown-list-selection-face "black")


;; ;; ** Misc Config **

;; ;; Run calls to perly_sense as a prepared shell command. Experimental
;; ;; optimization, please try it out.
(setq ps/use-prepare-shell-command t)


;; ;; *** PerlySense End ***
;; JPL



(defun jpl/perl-string-quote-from-interpolation ()
  "The current region should be inside a double quoted
string. Surround it with end-quote and string concatenation."
  (interactive)
  (save-excursion
    (let (
          (begin (min (point) (mark)))
          (end (max (point) (mark))))
      (goto-char end)
      (insert " . \"")
      (goto-char begin)
      (insert "\" . ")
      (activate-mark)
      )
    )
  )
(global-set-key (kbd "\C-o e \"") 'jpl/perl-string-quote-from-interpolation)






