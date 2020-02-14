;;; docker-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "docker-compose" "docker-compose.el" (24134
;;;;;;  42259 723585 915000))
;;; Generated autoloads from docker-compose.el
 (autoload 'docker-compose "docker-compose" nil t)

;;;***

;;;### (autoloads nil "docker-container" "docker-container.el" (24134
;;;;;;  42259 727585 865000))
;;; Generated autoloads from docker-container.el

(autoload 'docker-container-eshell "docker-container" "\
Open `eshell' in CONTAINER.

\(fn CONTAINER)" t nil)

(autoload 'docker-container-find-directory "docker-container" "\
Inside CONTAINER open DIRECTORY.

\(fn CONTAINER DIRECTORY)" t nil)

(autoload 'docker-container-find-file "docker-container" "\
Open FILE inside CONTAINER.

\(fn CONTAINER FILE)" t nil)

(autoload 'docker-container-shell "docker-container" "\
Open `shell' in CONTAINER.  When READ-SHELL is not nil, ask the user for it.

\(fn CONTAINER &optional READ-SHELL)" t nil)

(autoload 'docker-containers "docker-container" "\
List docker containers.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "docker-core" "docker-core.el" (24134 42259
;;;;;;  715586 16000))
;;; Generated autoloads from docker-core.el
 (autoload 'docker "docker" nil t)

;;;***

;;;### (autoloads nil "docker-image" "docker-image.el" (24134 42259
;;;;;;  731585 814000))
;;; Generated autoloads from docker-image.el

(autoload 'docker-image-pull-one "docker-image" "\
Pull the image named NAME.  If ALL is set, use \"-a\".

\(fn NAME &optional ALL)" t nil)

(autoload 'docker-images "docker-image" "\
List docker images.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "docker-machine" "docker-machine.el" (24134
;;;;;;  42259 719585 965000))
;;; Generated autoloads from docker-machine.el

(autoload 'docker-machine-create "docker-machine" "\
Create a machine NAME using DRIVER.

\(fn NAME DRIVER)" t nil)

(autoload 'docker-machine-env-one "docker-machine" "\
Parse and set environment variables from \"docker-machine env NAME\" output.

\(fn NAME)" t nil)

(autoload 'docker-machines "docker-machine" "\
List docker machines.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "docker-network" "docker-network.el" (24134
;;;;;;  42259 711586 66000))
;;; Generated autoloads from docker-network.el

(autoload 'docker-networks "docker-network" "\
List docker networks.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "docker-volume" "docker-volume.el" (24134 42259
;;;;;;  731585 814000))
;;; Generated autoloads from docker-volume.el

(autoload 'docker-volume-dired "docker-volume" "\
Enter `dired' in the volume named NAME.

\(fn NAME)" t nil)

(autoload 'docker-volumes "docker-volume" "\
List docker volumes.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("docker-pkg.el" "docker-utils.el" "docker.el")
;;;;;;  (24134 42259 727585 865000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; docker-autoloads.el ends here
