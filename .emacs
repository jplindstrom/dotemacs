;; -*-emacs-lisp-*-

; Command line to start emacs with the correct size and pos is:
; C:\appl\util\emacs-21.3\bin\runemacs.exe -g 120x74+0+100



;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


(setq emacs-home-directory (concat (getenv "HOME") "/"))
(setq elisp-home-directory (expand-file-name (concat emacs-home-directory "elisp")))
(byte-recompile-directory elisp-home-directory)

(defun jpl/add-to-load-path (dir)
  (add-to-list 'load-path (concat elisp-home-directory "/" dir))
  )
(jpl/add-to-load-path "")
(jpl/add-to-load-path "config")
(jpl/add-to-load-path "lib")


(defun jpl/load (config-file)
  (load config-file))


(jpl/load "emacs-performance")
(jpl/load "emacs-json")

(jpl/load "emacs-packages")
(jpl/load "emacs-set-path-from-env")

(jpl/load "emacs-mac")
(jpl/load "emacs-display")
(jpl/load "emacs-minibuffer")
(jpl/load "emacs-global-unset-keys")


(jpl/load "emacs-clipboard")
(jpl/load "emacs-mouse-scroll")
(jpl/load "emacs-server-start")
(jpl/load "emacs-whitespace")
(jpl/load "emacs-80-column-marker")
(jpl/load "emacs-save-minibuffer-history")
(jpl/load "emacs-buffer-name")
(jpl/load "emacs-buffer-menu")
(jpl/load "emacs-global-replace")
(jpl/load "emacs-config-insert-mode")
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
(jpl/load "emacs-narrow") ;; narrow-or-widen-dwim
(jpl/load "emacs-show-paren")
(jpl/load "emacs-highlight-changes") ;; Not active, but cool
(jpl/load "emacs-dabbrev-expand") ;; Alt-space completion
;; (put 'scroll-left 'disabled nil) ;; Should be disabled?
(jpl/load "emacs-recent")
(jpl/load "emacs-buffer-scroll")
(jpl/load "emacs-revert-buffer")
(jpl/load "emacs-printer")
(jpl/load "emacs-win32-shell-mode")

(jpl/load "mode-helpful")

(jpl/load "fake-stdin-slurp")


;; Modes
(jpl/load "mode-autopair")
(jpl/load "buffer-file-eol-type") ;; Used at all?
(jpl/load "goto-last-edit-point") ;; Unused
(jpl/load "mode-align-string") ;; JPL: not ported
(jpl/load "mode-highlight-tail") ;; Disabled. Useless, but cool
(jpl/load "mode-graphviz")
(jpl/load "mode-evil")
(jpl/load "mode-ansi-term")
(jpl/load "mode-helm")
(jpl/load "mode-projectile")
(jpl/load "lib-jpl-popup")
(jpl/load "mode-longlines")
(jpl/load "mode-iedit")
(jpl/load "mode-avy")
(jpl/load "jpl-jira")
(jpl/load "mode-markdown")
(jpl/load "mode-indent-guide")

;; Org mode (has language specific code blocks)
(jpl/load "mode-org") ;; lang- ?
(jpl/load "mode-org-level-faces")
(jpl/load "mode-org-keys")


;; Editors
(jpl/load "mode-editorconfig")

;; Languages
(jpl/load "lang-perl")
(jpl/load "lang-perl-productivity")
;; (jpl/load "lang-perl-perlysense")    ;; Too low on work Mac
(jpl/load "lang-perl-perlysense-mini")  ;; Port the used bits
(jpl/load "lang-perl-productivity-keys")
(jpl/load "lang-perl-perltidy")
(jpl/load "lang-perl-prove")
(jpl/load "lang-perl-obsolete")

(jpl/load "lang-pod")
(jpl/load "template-toolkit")

(jpl/load "lang-javascript")
(jpl/load "lang-typescript")

(jpl/load "lang-ruby")
(jpl/load "lang-python")
(jpl/load "lang-go")
(jpl/load "lang-C++-boss")
(jpl/load "lang-elisp-eval-and-replace")
;; (jpl/load "lang-clojure")  ;;;JPL: upgrade with 29

;; Config language
(jpl/load "lang-terraform")

;; Markup language
(jpl/load "lang-web")
(jpl/load "lang-sql")
(jpl/load "nxml")
(jpl/load "lang-yaml")
(jpl/load "lang-json")
(jpl/load "lang-text-utils")


;; Dev
(jpl/load "mode-copilot")
(jpl/load "snippets")
(jpl/load "dev-ediff-conflict")
(jpl/load "dev-indent")
(jpl/load "dev-indent-tabstep")
(jpl/load "dev-jpl-reformat")
(jpl/load "jpl-extend-region-to-enclosing-block") ;; unused by this config
(jpl/load "tool-cede") ;; disabled CEDET and ECB


;; Services
(jpl/load "service-openai")

;; Tools
(jpl/load "jpl-make-executable")
(jpl/load "tool-magit")
(jpl/load "git-productivity")
(jpl/load "version-control")
(jpl/load "tool-browse-kill-ring")
(jpl/load "tool-sort")
(jpl/load "tool-spellcheck")


;; Projects
(jpl/load "proj-pips")
(jpl/load "proj-iplayer")
(jpl/load "lang-perl-iplayer")


;; Remaining key bindings
(jpl/load "global-key-bindings") ;; Config. Even needed? try to disable



;;; JPL: break this out
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-interval 1)
 '(custom-safe-themes
   '("cdc7555f0b34ed32eb510be295b6b967526dd8060e5d04ff0dce719af789f8e5" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "d9d703d9591163c5d711d42e01971ac1b8186f5e46596a5529e6ab3ef5014020" "5b577969a67ee2d1c55f44c82609f62d757529cec4b279bef67fdab60215e80b" "b07c445fe973d0d38e4266f29587f90070f65fbfec33b5772bb1be054a197832" "75d4ccc5e912b93f722e57cca3ca1a15e079032cd69fd9bc67268b4c85639663" default))
 '(indent-guide-inhibit-modes '(dired-mode org-mode))
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(js2-missing-semi-one-line-override t)
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-trailing-comma-warning nil)
 '(js3-mode-show-strict-warnings nil)
 '(js3-strict-trailing-comma-warning nil)
 '(magit-todos-auto-group-items 20)
 '(magit-todos-branch-list nil)
 '(magit-todos-exclude-globs '(".git/" "elpa/" "node_modules/"))
 '(magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:")
 '(magit-todos-keywords '("JPL"))
 '(markdown-asymmetric-header t)
 '(markdown-display-remote-images t)
 '(markdown-hide-urls t)
 '(message-log-max 1000)
 '(org-mind-map-dot-output "png")
 '(org-support-shift-select t)
 '(package-selected-packages
   '(unfill magit-org-todos org-rich-yank mermaid-mode ob-mermaid ob-sql-mode browse-kill-ring yaml-mode iedit htmlize eldoc spell-fu markdown-toc jsonrpc s pytest editorconfig jest-test-mode lsp-ui helpful chatgpt-shell ob-chatgpt-shell treemacs terraform-doc org-download gptel org-web-tools git-link hl-todo magit-todos evil-numbers origami yaml-pro beacon csv-mode emacsql-sqlite-module seq ghub prettier-js graphql-mode company-terraform terraform-mode tide code-review forge cask company dap-mode helm-lsp lsp-ivy lsp-treemacs x-path-walker ztree treemacs-icons-dired treemacs-magit string-inflection evil-string-inflection evil-surround web-mode org mocha go-mode wgrep docker dockerfile-mode poly-markdown polymode discover-js2-refactor js2-highlight-vars js2-mode magit-gitflow treemacs-projectile edit-indirect org-sticky-header smartparens org-presie org-mind-map org-jira avy plantuml-mode sparkline spaceline slim-mode scala-mode2 puppet-mode project-explorer pcsv noctilux-theme neotree markdown-mode+ keyfreq js2-refactor js-doc highlight-symbol helm-projectile helm-ls-git helm-helm-commands helm-gtags helm-git-grep helm-git helm-dired-recent-dirs helm-c-yasnippet helm-c-moccur haml-mode graphviz-dot-mode golden-ratio git-messenger git-gutter-fringe flatui-theme expand-region direx diminish cperl-mode cider anzu))
 '(recentf-menu-filter 'recentf-arrange-by-dir)
 '(revert-without-query '(".*"))
 '(safe-local-variable-values
   '((jpl/tide-mode-format-before-save)
     (jpl/tide-mode-format-before-save . t)
     (evil-shift-width . 2)
     (js2-indent-level . 2)))
 '(scroll-bar-mode 'right)
 '(size-indication-mode t)
 '(tab-width 4)
 '(terraform-doc-markdown-mode-function 'gfm-view-mode)
 '(tide-server-max-response-length 1024000)
 '(tide-user-preferences
   '(:includeCompletionsForModuleExports t :includeCompletionsWithInsertText t :allowTextChangesInNewFiles t :generateReturnInDocTemplate nil))
 '(transient-mark-mode 'identity)
 '(treemacs-is-never-other-window t)
 '(truncate-lines t)
 '(typescript-indent-level 2)
 '(web-mode-auto-close-style 2)
 '(web-mode-enable-auto-pairing nil))


;; Fix
;; treemacs--process-file-events produces errors in case of
;; transient files, even gitignored ones. Hiding gitignored files
;; appears to work around the timer error.
;; https://github.com/Alexander-Miller/treemacs/issues/749#issuecomment-1962363871
(setq treemacs-hide-gitignored-files-mode t)



;; org-special-keyword is old org-drawer

;; Automatically added to .emacs

(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-current-diff-B ((t (:extend t :background "DarkSeaGreen2" :foreground "#2c3e50"))))
 '(ediff-even-diff-B ((t (:extend t :background "light gray"))))
 '(ediff-fine-diff-A ((t (:background "coral2" :foreground "#2c3e50" :weight bold))))
 '(ediff-fine-diff-B ((t (:background "DarkSeaGreen2" :foreground "#2c3e50" :slant italic))))
 '(ediff-odd-diff-B ((t (:extend t :background "LightBlue2"))))
 '(font-lock-builtin-face ((t (:foreground "#16a085" :underline t :weight bold))))
 '(highlight ((t (:background "light steel blue"))))
 '(hl-line ((t (:extend t :underline "black"))))
 '(markdown-code-face ((t (:inherit fixed-pitch :extend t :background "gainsboro" :height 0.8))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 2.0))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.5))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.2))))
 '(org-block ((t (:extend t :background "gainsboro"))))
 '(org-block-begin-line ((t (:inherit org-meta-line :extend t :background "gainsboro" :foreground "steel blue"))))
 '(org-drawer ((t (:foreground "dim gray" :height 0.7))))
 '(org-hide ((t (:foreground "#f0f0f0"))))
 '(org-level-10 ((t (:inherit outline-8 :foreground "dark magenta"))))
 '(org-level-11 ((t (:inherit outline-8 :foreground "tomato"))))
 '(org-level-12 ((t (:inherit outline-8 :foreground "saddle brown"))))
 '(org-level-13 ((t (:inherit outline-8 :foreground "dark goldenrod"))))
 '(org-level-14 ((t (:inherit outline-8 :foreground "dark green"))))
 '(org-level-15 ((t (:inherit outline-8 :foreground "dim gray"))))
 '(org-level-16 ((t (:inherit outline-8 :foreground "dark orange"))))
 '(org-level-7 ((t (:inherit outline-7 :underline nil))))
 '(org-level-9 ((t (:inherit outline-8 :foreground "dark orange"))))
 '(org-meta-line ((t (:inherit font-lock-comment-face :foreground "dark gray" :height 0.6))))
 '(org-property-value ((t (:height 0.7))))
 '(org-special-keyword ((t (:inherit font-lock-keyword-face :foreground "gray" :height 0.7))))
 '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "#16a085" :height 1.4))))
 '(outline-2 ((t (:inherit font-lock-variable-name-face :foreground "#2980b9" :height 1.2))))
 '(outline-4 ((t (:inherit font-lock-comment-face :foreground "dark slate gray"))))
 '(outline-6 ((t (:inherit font-lock-constant-face :foreground "#c0392b" :height 0.9))))
 '(outline-7 ((t (:inherit font-lock-builtin-face :height 0.9))))
 '(outline-8 ((t (:inherit font-lock-string-face :height 0.9))))
 '(secondary-selection ((t (:extend t :background "LightYellow3"))))
 '(spell-fu-incorrect-face ((t (:underline (:color "sandy brown" :style wave)))))
 '(terraform-resource-name-face ((t (:foreground "dark green" :underline t))))
 '(terraform-resource-type-face ((t (:foreground "medium sea green" :underline t)))))
