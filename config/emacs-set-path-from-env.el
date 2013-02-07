

(setenv "PATH"
        (shell-command-to-string
         "bash -c '. $HOME/.bash_profile; echo -n $PATH'"))
(setenv "PATH" (concat "/usr/local/git/bin" ":" (getenv "PATH")))

(dolist (dir (split-string (getenv "PATH") ":" t))
  (add-to-list 'exec-path dir))


