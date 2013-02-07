
;; Project-root

(require 'find-cmd)
;; (require 'project-root)
;; (setq project-roots
;;       '(("Generic Perl Project"
;;          :root-contains-files ("t" "lib")
;;          :on-hit (lambda (p) nil))))

(with-library 'project-root
  (setq project-roots
        '(("Conc"
           :root-contains-files ("conkeror-spawn-helper.c" "chrome"))
          ("Dynamite"
           :anything-highlight
           (
;            ("Dynamite/Schema/DB/" . widget-inactive)
            ("^\\(t\\)/" . diff-added-face)
            ("Controller/iPlayer/\\(Widget\\)/" . compilation-warning)
            ("Dynamite/Interface/\\(Block\\)/"
             . font-lock-keyword-face))
           :default-command prove-run-prove
           :root-contains-files ("lib" "t" "conf/dynamite_common.xml")
           :bookmarks ("lib/Dynamite/Controller/iPlayer/Widget"
                       "lib/Dynamite/Interface/Block"
                       "data/templates"))
          ("Dynamite-view"
           :root-contains-files ("templates"))
          ("Fumo"
           :root-contains-files ("fumo.yml" "lib" "t" "todo.org"))
          ("ETest"
           :root-contains-files ("todo.org" "etest.el"))
          ("ShellArchive"
           :root-contains-files ("project-root.el" "xmlgen.el"))
          ("Dotfiles"
           :anything-highlight
           (("\\([[:digit:]]\\{3\\}-.*\.el\\)" . font-lock-keyword-face)
            ("/.elisp/mine/\\(.+\.el\\)" . diff-added-face))
           :root-contains-files ("go" "generic")
           :bookmarks ("generic/.elisp/lisp.d" "generic/.elisp/mine"))
          ("BLR"
           :root-contains-files ("BLR-Web" "docs" "iclr-docs"))
          ("Blog"
           :root-contains-files ("Rakefile" "rassmalog.rb")
           :default-command
           (lambda ()
             (shell-command "rake >/dev/null")))
          ("Generic Python Project"
           :root-contains-files ("setup.py"))
          ("Generic Perl Project"
           :root-contains-files ("t" "lib")
           :default-command prove-run-prove)
          ("Generic PerlySense Project"
           :root-contains-files (".PerlySenseProject")
           :default-command prove-run-prove)
          ("Generic Git Project"
           :root-contains-files (".git"))))
  (setq project-root-extra-find-args (find-to-string '(prune (name "*~BASE~" "*_flymake*" ".svn" ".git"))))
  )



(add-hook 'find-file-hook 'project-root-fetch)
(add-hook 'dired-mode-hook 'project-root-fetch)


(global-set-key (kbd "C-c p M-x")
                'project-root-execute-extended-command)


(defmacro with-project-root (&rest body)
  `(if (project-root-p)
       (let ((default-directory ,(cdr project-details)))
         ,@body)
     ,@body))

(defun project-root-cwd-call-interactively (command)
  (if (project-root-p)
      (let ((default-directory (cdr project-details)))
        (call-interactively command))
    (call-interactively command)
    ))

(defun project-root-cwd-find-file ()
  "Call find-file from the project root, if there is one"
  (interactive)
  (project-root-cwd-call-interactively 'find-file))

(defun project-root-cwd-grep-find ()
  "Call grep-find from the project root, if there is one"
  (interactive)
  (project-root-cwd-call-interactively 'grep-find))


(defun project-root-copy-buffer-filename ()
  "Copy the current buffer file name to the kill ring. The file
name is relative to the current project root."
  (interactive)
  (with-project-root
   (let* ((relative-buffer-file-name (file-relative-name (buffer-file-name))))
     (kill-new relative-buffer-file-name)
     )))





;; Project-root

(require 'find-cmd)
(require 'project-root)
(setq project-roots
      '(("Generic Perl Project"
         :root-contains-files ("t" "lib")
         :on-hit (lambda (p) nil))))



(global-set-key (kbd "C-; C-f") 'project-root-cwd-grep-find)
(global-set-key (kbd "C-; C-x C-f") 'project-root-cwd-find-file)

(global-set-key (kbd "C-; q f") 'project-root-copy-buffer-filename)



