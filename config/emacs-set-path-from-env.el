

;; On OSX, Emacs doesn't run in a login shell, so the PATH isn't set
;; correctly. Run .bash_profile and fish out the PATH, and add git so
;; Magit works properly
(when (eq system-type 'darwin) ;; mac specific settings
  (setenv "PATH"
          (shell-command-to-string
           "bash -c 'if [ -r $HOME/.bash_profile ]; then . $HOME/.bash_profile 2>&1 >/dev/null; fi; echo -n $PATH'"))
  (setenv "PATH" (concat "/usr/local/git/bin" ":" (getenv "PATH")))
  )

(dolist (dir (split-string (getenv "PATH") ":" t))
  (add-to-list 'exec-path dir))


