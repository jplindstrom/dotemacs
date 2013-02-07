

(eval-after-load 'rcirc '(require 'rcirc-notify))
(with-library 'rcirc
  (setq rcirc-server-alist
        '(("irc.nap"
           :nick "johanl"
           :channels ("#backend" "#tech", "#idlechat"))))
  (setq rcirc-startup-channels-alist
        '(
          ;; ("localhost" "#dynamite")
          ;; ("localhost" "#pips")
          ))
  (setq rcirc-default-nick "johan")
  (setq rcirc-default-user-name "jlindstrom")
  (setq rcirc-default-user-full-name "Johan Lindstrom")

  (with-library 'rcirc-late-fix t)
  (with-library 'rcirc-color t)
  (with-library 'rcirc-pounce t)

  (setq rcirc-time-format "%Y-%m-%d %H:%M ")

  (set-face-foreground 'rcirc-my-nick "red" nil)
  )


(defun irc ()
  (interactive)
  (rcirc-connect "grunt.net-a-porter.com" 6667 rcirc-default-nick rcirc-default-nick rcirc-default-nick (assoc "10.161.39.130" rcirc-startup-channels-alist))
  (rcirc-track-minor-mode)
  (set-fill-column 170)

  (server-force-delete)  ;; Assume we run IRC in its own Emacs instance
  )


