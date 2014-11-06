

(setenv "PATH"
        (shell-command-to-string
         "bash -c 'if [ -r $HOME/.bash_profile ]; then . $HOME/.bash_profile; fi; echo -n $PATH'"))
(setenv "PATH" (concat "/usr/local/git/bin" ":" (getenv "PATH")))

(dolist (dir (split-string (getenv "PATH") ":" t))
  (add-to-list 'exec-path dir))


