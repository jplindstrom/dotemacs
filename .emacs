;; -*-emacs-lisp-*-

; Command line to start emacs with the correct size and pos is:
; C:\appl\util\emacs-21.3\bin\runemacs.exe -g 120x74+0+100


(setq emacs-home-directory (concat (getenv "HOME") "/"))
(defun jpl/add-to-load-path (dir)
  (add-to-list 'load-path (expand-file-name (concat emacs-home-directory "elisp" "/" dir)))
  )
(jpl/add-to-load-path "config")
(jpl/add-to-load-path "lib")
(jpl/add-to-load-path "local-lib")


(defun jpl/load (config-file)
  (load (concat config-file)))


(fset 'jpl/extract-dotemacs-chunk
   [?d ?\C-l ?k ?% ?h ?h ?y ?i ?\" ?\C-x ?\C-f ?c ?o ?n ?f ?i ?g ?/ ?\C-y ?. ?e ?l return ?\C-j ?\C-j ?P ?\C-p ?\C-x ?\C-s ?\C-x ?b return ?\C-a C-tab ?\C-x ?b return C-tab])
(global-set-key (kbd "\C-c \C-d") 'jpl/extract-dotemacs-chunk)


(jpl/load "emacs-packages")
(jpl/load "emacs-load-path")
(jpl/load "emacs-set-path-from-env")
(jpl/load "emacs-with-library") ;; used by irc, move up

(jpl/load "emacs-mac")
(jpl/load "emacs-display")
(jpl/load "emacs-global-unset-keys")


(jpl/load "emacs-load-default-but-dont-init")
(jpl/load "emacs-set-font") ;;; Not used
(jpl/load "emcas-cliboard")
(jpl/load "emacs-mouse-scroll")
(jpl/load "emacs-uniquify")
(jpl/load "emacs-server-start")
(jpl/load "emacs-trailing-whitespace")
(jpl/load "emacs-80-column-marker")
(jpl/load "emacs-save-minibuffer-history")
(jpl/load "emacs-buffer-menu")
(jpl/load "emacs-global-replace")
(jpl/load "emacs-confing-insert-mode")
(jpl/load "emacs-font-lock")
(jpl/load "emacs-completion-ignored-extensions")
(jpl/load "emacs-auto-mode-alist") ;; Lang. Should be split up to el, py, h
(jpl/load "emacs-iso-syntax")   ;; Config. What is this???
;; (setq default-major-mode 'text-mode)
(jpl/load "emacs-file") ; final newline
(jpl/load "emacs-calendar")
(jpl/load "emacs-backup") ; emacs
(jpl/load "emacs-fontify-buffer") ; emacs Does this do anything???
; (setq kom-emacs-knows-iso-8859-1 t) ;; Unused?
(jpl/load "emacs-dired")
(jpl/load "emacs-unused-config")
(jpl/load "emacs-grep")
(jpl/load "emacs-ediff")
(jpl/load "emacs-lines") ;; Longlines, truncate lines, etc
(jpl/load "emacs-selection")
(jpl/load "emacs-isearch") ;; Occur, kill
(jpl/load "emacs-show-paren")
(jpl/load "emacs-highlight-changes") ;; Not active, but cool
(jpl/load "emacs-dabbrev-expand") ;; Alt-space completion
;; (put 'scroll-left 'disabled nil) ;; Should be disabled?
(jpl/load "emacs-recent")
(jpl/load "emacs-buffer-scroll")
(jpl/load "emacs-revert-buffer")
(jpl/load "emacs-printer")
(jpl/load "emacs-win32-printing") ;; May work?
(jpl/load "emacs-win32-shell-mode")

(jpl/load "fake-stdin-slurp")


;; Modes
(jpl/load "mode-newsticker") ;; disabled, unused
;; (jpl/load "goto-match-paren") ;; doesn't exist
(jpl/load "mode-autopair")
(jpl/load "buffer-file-eol-type") ;; Used at all?
(jpl/load "goto-last-edit-point") ;; Unused
(jpl/load "mode-align-string")
(jpl/load "mode-ee") ;; disabled, unused
(jpl/load "mode-highlight-tail") ;; Disabled. Useless, but cool
(jpl/load "mode-graphviz")
(jpl/load "mode-projectile")
(jpl/load "mode-evil")
(jpl/load "mode-ansi-term")
(jpl/load "mode-helm")
(jpl/load "lib-jpl-popup")
(jpl/load "mode-confluence")
(jpl/load "mode-longlines")
(jpl/load "mode-ace-jump")
(jpl/load "mode-iedit")
(jpl/load "mode-expand-region")
(jpl/load "mode-auto-complete") ;; disabled
(jpl/load "jpl-s5")
(jpl/load "jpl-jira")


;; Languages
(jpl/load "lang-perl")
(jpl/load "lang-perl-productivity")
(jpl/load "lang-perl-perlysense")
(jpl/load "lang-perl-productivity-keys")
(jpl/load "lang-perl-perltidy")
(jpl/load "lang-perl-prove")
(jpl/load "lang-perl-obsolete")

(jpl/load "lang-pod")
(jpl/load "template-toolkit")

(jpl/load "lang-ruby")
(jpl/load "lang-python")
(jpl/load "javascript")
(jpl/load "lang-C++-boss")
(jpl/load "lang-haskell")
(jpl/load "lang-elisp-eval-and-replace")
(jpl/load "lang-scala")

;; Markup language
(jpl/load "lang-sql")
(jpl/load "nxml")
(jpl/load "css")
(jpl/load "yaml")
(jpl/load "rnc") ;; XML Relax NG schema
(jpl/load "lang-csv")
(jpl/load "mode-org") ;; lang- ?
(jpl/load "mode-org-keys")


;; Dev
(jpl/load "snippets")
(jpl/load "dev-ediff-conflict")
(jpl/load "dev-indent")
(jpl/load "dev-indent-tabstep")
(jpl/load "dev-jpl-reformat")
(jpl/load "jpl-extend-region-to-enclosing-block") ;; unused by this config
(jpl/load "dev-camel-case")
(jpl/load "tool-cede") ;; disabled CEDET and ECB


;; Tools
(jpl/load "htmlize")
(jpl/load "tool-subversion")
(jpl/load "tool-magit")
(jpl/load "version-control")
(jpl/load "tool-irc")
(jpl/load "tool-browse-kill-ring")


;; Projects
(jpl/load "proj-pips")
(jpl/load "proj-iplayer")
(jpl/load "lang-perl-iplayer")


;; Remaining key bindings
(jpl/load "global-key-bindings") ;; Config. Even needed? try to disable



;;; JPL: break this out
(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(auto-revert-interval 1)
 '(emacs-wiki-create-backlinks t)
 '(indent-tabs-mode nil)
 '(recentf-menu-filter nil)
 '(revert-without-query (quote (".*")))
 '(tab-width 4)
 '(truncate-lines t)
 '(scroll-bar-mode (quote right))
 '(size-indication-mode t)
 '(transient-mark-mode (quote identity))
 )

