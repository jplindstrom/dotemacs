

;; ORG mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))



;; Performance workaround for large org files: slow if in overview
;; mode, but fast with unfolded
; So start unfolded to cache(?) all the lines
(setq org-startup-folded nil)
; but then restore overview display
(add-hook 'org-mode-hook (function (lambda () (org-overview))))

(add-hook 'org-mode-hook (lambda ()
                           (push '("TODO"    . ?✗) prettify-symbols-alist)
                           ;; (push '("TODO"    . ?□) prettify-symbols-alist)
                           ;; (push '("DOING"   . ?♻) prettify-symbols-alist)
                           (push '("DOING"    . ?✘) prettify-symbols-alist)
                           (push '("DONE"    . ?✔) prettify-symbols-alist)
                           (push '("WAIT" . ?⨻) prettify-symbols-alist)
                           (push '("BLOCKED" . ?⨻) prettify-symbols-alist)
                           (push '("WONTDO"  . ?✅) prettify-symbols-alist)
                           (prettify-symbols-mode)
                           ))
;; △ ⎊ ⨻ ◃ ⧐


(add-hook
 'org-mode-hook
 (lambda ()
   (require 'verb)
   (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
   (company-mode)
   )
 )



;; Restore C-tab to other-window
(org-defkey org-mode-map [(control tab)] nil)

(define-key global-map "\C-cll" 'org-store-link)
(define-key global-map "\C-cl\C-l" 'org-insert-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-hide-leading-stars t)


;; Beginning of line means after the stars
(setq org-special-ctrl-a/e t)


(setq org-link-abbrev-alist
      '(
        ("google"   . "http://www.google.com/search?q=")
        ))

(setq org-todo-keyword-faces
      '(
        ("TODO"  . (:foreground "red" :weight bold))
        ("DOING" . (:foreground "red" :weight bold))
        ("WAIT"  . (:foreground "orange red" :weight bold))
        ("BLOCKED"  . (:foreground "blue" :weight bold))
        ("DONE"  . org-done)

        ("TASK"  . (:foreground "orange red" :weight bold))
        ("STORY"  . (:foreground "turquoise4" :weight bold))
        ("EPIC"  . org-done)
        ))


(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-bullets-bullet-list '("★" "▷" "►" "⊙" "•" "•" "•" "•" "•" "•" "•" "•" "•" "•"))


;; Don't cycle faces when going into undefined levels
(setq org-cycle-level-faces nil)


;; Don't show e.g. *bold* and _underline_ markers, just the text
(setq org-hide-emphasis-markers t)


;; Allow `code span` to show code similar to ~code span~
(push '("`" org-block) org-emphasis-alist)




(defun myorg-convert-line-outline-org-to-wiki (outline-line)
  "Return `outline-line` (a single line of org-mode text as wiki
markup"
  (let ((org-regex "\\(^\\*+\\) "))
    (if (string-match org-regex outline-line)
        (let* ((wiki-line (replace-regexp-in-string org-regex "" outline-line))
               (org-indent (match-string-no-properties 1 outline-line))
               (wiki-indent (replace-regexp-in-string "*" "  " org-indent))
               )
          (format "%s* %s" wiki-indent wiki-line)
          )
      outline-line)))
;; (myorg-convert-line-outline-org-to-wiki "** Hello there")

(defun myorg-convert-text-outline-org-to-wiki (outline-text)
  "Return `outline-text` as wiki markup"
  (mapconcat
   'identity
   (mapcar 'myorg-convert-line-outline-org-to-wiki (split-string outline-text "\n"))
   "\n"))
;; (myorg-convert-text-outline-org-to-wiki "* Hello
;; ** There
;; * And
;; ** here too
;; ")

(defun myorg-copy-convert-region-outline-org-to-wiki (beg end)
  "Return `outline-text` as wiki markup"
  (interactive "r")
  (kill-new (myorg-convert-text-outline-org-to-wiki( buffer-substring beg end))))



(fset 'myorg-find-next-heading-and-make
   [?\C-s ?\C-q ?\C-j ?* ?  home delete ?= end ?  ?= right])


(require 'org-fstree)





;;; Babel

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (emacs-lisp . t)
   (python . t)
   (dot . t)
   (plantuml . t)
   (R . t)
   (shell . t)
   (calc . t)
   (verb . t)
   (chatgpt-shell . t)
   ))

(defun my/org-confirm-babel-evaluate (lang body)
  (not (member lang '("dot" "plantuml" "python" "R"))))
(setq org-confirm-babel-evaluate 'my/org-confirm-babel-evaluate)



(setq org-startup-with-inline-images t)

(defun my/org-redisplay-inline-images ()
  (interactive)
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))
(add-hook 'org-babel-after-execute-hook 'my/org-redisplay-inline-images)



;;; GraphViz

;; Example
;; #+NAME: my_graphviz_result
;; #+BEGIN_SRC dot :file my_graphviz_result.png :cmdline -Kdot -Tpng
;; digraph G {
;;     overlap = false; ranksep = 0.5; nodesep = 0.1;
;;     rankdir = BT; # LR, TB
;;     fontname = "Verdana";
;;     labelloc = "b";
;;     graph[ style = invis ];

;;     node [
;;         shape    = "box",
;;         width    = 0.1, height   = 0.4,
;;         fontname = "Verdana", fontsize = 8,
;;     ];
;;     edge [
;;         arrowsize = 0.5,
;;         fontname  = "helvetica", fontsize  = 9,
;;     ];

;;     subgraph cluster_abc {
;;         label = "ABC"

;;         first [ label = "The first\none" ]
;;         Third [
;;             style     = "filled",
;;             fillcolor = "lightblue",
;;         ]
;;     }

;;     Second [ shape = "box3d" ]

;;     first -> Second [ label = "the thin, grey line" ]
;;     Second -> Third [
;;         label = "going\nto 3rd",
;;         style = "dashed",
;;         arrowhead = none,
;;     ]
;; }
;;  #+END_SRC

;; #+RESULTS: name: my_graphviz_result



;;; PlantUML

;; http://plantuml.com/sequence-diagram
;; http://plantuml.com/class-diagram
;; http://plantuml.com/salt
;;   e.g. <&person>
;;   https://useiconic.com/open/
;;   http://s.plantuml.com/imgp/1c6-salt-012.png

;; Download from http://plantuml.com/download
;; wget https://github.com/plantuml/plantuml/releases/download/v1.2022.7/plantuml-1.2022.7.jar ~/bin/plantuml.jar
(setq org-plantuml-jar-path
      (expand-file-name "~/bin/plantuml.jar"))


;;; http://plantuml.com/sequence-diagram

;; #+NAME: my_sequence_result
;; #+BEGIN_SRC plantuml :file my_sequence_result.png
;; actor Alice
;; Alice -> Bob: synchronous\ncall
;; ... time passes ...
;; Alice ->> Bob: asynchronous call
;; note right: so this happened
;; == That's all folks ==
;; #+END_SRC

;; #+results: name: my_sequence_result
;; file:my_sequence_result.png


;;; http://plantuml.com/salt

;; #+NAME: my_gui_result
;; #+BEGIN_SRC plantuml :file my_gui_result.png
;; salt
;; {
;;   {* File | Edit | View | About
;;   About | PlantUML | salt | Help }
;;   .
;;   .
;;   {/ <b>Tab1 | Tab2 | Tab3 }
;;   Just plain text | [This is my button]
;;   ()  Unchecked radio | (X) Checked radio
;;   .
;;   {+
;;     []  Unchecked box | [X] Checked box
;;     .
;;     User <&person> | "input field   "
;;     --
;;     ^This is a droplist^
;;   } | {
;;   () Radio
;;   ==
;;   (X) Button
;;   }
;; }
;; #+END_SRC

;; #+results: name: my_gui_result



(defun org-export-subtree-as-confluence-wiki-markup (prefix-arg)
  "Export current subtreee."
  (interactive "P")
  (save-excursion
    ;; Copy tree to temp buffer
    (org-mark-subtree)
    (let ((original-org-text (buffer-substring-no-properties (point) (mark))))
    (with-temp-buffer
      (org-mode)
      (insert original-org-text)
      (goto-char (point-min))

      ;; Promote subtree to top level
      (while (> (or (org-current-level) 1) 1)
        (org-promote-subtree)
        )

      (goto-char (point-min))
      (insert "
")
      ;; Rename * to h1.
      (goto-char (point-min))
      (replace-string "\n* " "\n\n\n\nh1. ")

      ;; rename ** to h2.
      (goto-char (point-min))
      (replace-string "\n** " "\n\n\nh2. ")

      (if prefix-arg
          (progn
            ;; rename *** to h3.
            (goto-char (point-min))
            (replace-string "\n*** " "\n\nh3. ")

            ;; rename **** and below to * and below
            (goto-char (point-min))
            (replace-string "\n****" "\n*")
            )
        ;; rename *** and below to * and below
        (goto-char (point-min))
        (replace-string "\n***" "\n*")
        )

      ;; Copy
      (kill-new (buffer-substring-no-properties (point-min) (point-max)))
      )
      )
    )
  )


(defun org-export-subtree-as-markdown (prefix-arg)
  "Export current subtreee."
  (interactive "P")
  (save-excursion
    ;; Copy tree to temp buffer
    (org-mark-subtree)
    (let ((original-org-text (buffer-substring-no-properties (point) (mark))))
    (with-temp-buffer
      (org-mode)
      (insert original-org-text)
      (goto-char (point-min))

      ;; Promote subtree to top level
      (while (> (or (org-current-level) 1) 1)
        (org-promote-subtree)
        )

      (goto-char (point-min))
      (insert "
")
      ;; Rename * to #
      (goto-char (point-min))
      (replace-string "\n* " "\n\n\n\n# ")

      ;; rename ** to h2.
      (goto-char (point-min))
      (replace-string "\n** " "\n\n\n## ")

      (if prefix-arg
          (progn
            ;; rename *** to h3.
            (goto-char (point-min))
            (replace-string "\n*** " "\n\n### ")

            ;; rename **** and below to * and below
            (goto-char (point-min))
            (replace-string "\n****" "\n*")
            )
        ;; rename *** and below to * and below
        (goto-char (point-min))
        (replace-string "\n***" "\n*")
        )

      ;; Indent remaining stars into nested bullet points
      (goto-char (point-min))
      (replace-string "\n**" "\n    *")

      (dotimes (i 10)
        (goto-char (point-min))
        (replace-string "    **" "        *"))


      ;; Make code blocks ```
      (goto-char (point-min))
      (replace-string "#+begin_src" "```")
      (goto-char (point-min))
      (replace-string "#+end_src" "```")

      ;; Copy
      (kill-new (buffer-substring-no-properties (point-min) (point-max)))
      )
      )
    )
  )



(defun jpl/issue-from-branch-name (branch-name)
  (save-excursion
    (if (string-match "\\([a-z]+-[0-9]+\\)" branch-name)
        (match-string 1 branch-name)
      nil)))

(defun jpl/org-jira-link-from-branch-name (branch-name)
  (interactive)
  (when branch-name
    (let* ((jira-issue-text (jpl/issue-from-branch-name branch-name))
           (jira-base-url "https://zpgltd.atlassian.net/"))
      (if jira-issue-text
          (format "%sbrowse/%s" jira-base-url (upcase jira-issue-text))
        nil))))

(defun jpl/org-jira-issue-url-property-from-branch-name (branch-name)
  (interactive)
  (let* ((jira-issue-url (jpl/org-jira-link-from-branch-name branch-name)))
    (if jira-issue-url
        (format ":PROPERTIES:
:issue-url: %s
:END:
" jira-issue-url)
      "")))


;; Copy current jira URL, branch name

(defun jpl/org-current-branch-name ()
  (interactive)
  (save-excursion
    (if (not (re-search-backward "^\\*\\* \\(\\w+ \\)?\\(\\b\\w+/\\w+-[0-9]+-.+\\)" nil t))
        nil
      (match-string 2))))

(defun jpl/copy-org-jira-current-issue-url ()
  (interactive)
  (let* ((branch-name (jpl/org-current-branch-name))
         (url (jpl/org-jira-link-from-branch-name branch-name)))
    (if (not url)
        (error "No URL found")
      (message "%s (%s)" url branch-name)
      (kill-new url))))

(defun jpl/copy-org-current-branch-name ()
  (interactive)
  (let* ((branch-name (jpl/org-current-branch-name)))
    (message "%s" branch-name)
    (kill-new branch-name)))

(define-key org-mode-map "\C-oecu" 'jpl/copy-org-jira-current-issue-url)
(define-key org-mode-map "\C-oecb" 'jpl/copy-org-current-branch-name)



;; Insert source block with a default language

(defun org-find-previous-begin_src-language ()
  "Return e.g. 'json' for the previous line '#+begin_src json'"
  (save-excursion
    (previous-line)
    (if (not (re-search-backward "#\\+begin_src \\([a-zA-Z]+\\)" nil t))
        nil
      (match-string 1))))

(defun org-insert-src-block (region-start region-end)
  (interactive "r")
  (org-insert-structure-template "src")
  (move-end-of-line nil)
  (when (search-backward "#+begin_src" nil t)
    (move-end-of-line nil)
    (let* ((default-language (org-find-previous-begin_src-language)))
      (if default-language
          (progn
            (insert default-language)
            (move-beginning-of-line nil))

        ;; Remove trailing whitespace
        (backward-char)
        (delete-char 1)

        (move-beginning-of-line nil)
        )))
  (when
      (save-excursion
        (forward-line)
        (looking-at " *#\\+end_src"))
    (forward-line)
    (open-line 1)))

(define-key org-mode-map "\C-oeis" 'org-insert-src-block)


;; Insert images from clipboard, screenshots, URLs

(require 'org-download)
;; Make sure images are displayed and not just the URL
(advice-add 'org-download-clipboard        :after #'org-redisplay-inline-images)
(advice-add 'org-download-screenshot       :after #'org-redisplay-inline-images)
(advice-add 'org-download-yank             :after #'org-redisplay-inline-images)
(advice-add 'org-download-rename-last-file :after #'org-redisplay-inline-images)
(advice-add 'org-download-rename-at-point  :after #'org-redisplay-inline-images)

(setq-default org-download-image-dir "images")
;; Insert Clipboard image
(define-key org-mode-map "\C-oeic" 'org-download-clipboard)
;; Insert Screenshot
(define-key org-mode-map "\C-oeiS" 'org-download-screenshot)
;; Insert Image from URL
(define-key org-mode-map "\C-oeii" 'org-download-yank)


(define-key org-mode-map "\C-oDd" 'org-download-delete)
(define-key org-mode-map "\C-oDR" 'org-download-rename-last-file)
(define-key org-mode-map "\C-oDr" 'org-download-rename-at-point)



;;; todo status

;; Go to DONE
(defun jpl/org-mode-set-todo-DONE ()
  (interactive)
  (org-todo "DONE")
  )

;; Should really be y and o, but for ergonomics x usability, let's use
;; these instead
(define-key org-mode-map "\M-\C-u" 'org-shiftleft)
(define-key org-mode-map "\M-\C-i" 'org-shiftright)
(define-key org-mode-map "\M-\C-p" 'jpl/org-mode-set-todo-DONE)





(fset 'jpl/markdown-make-code-block
   (kmacro-lambda-form [?V ?o ?` ?` ?` escape ?g ?v ?o ?V ?O ?` ?` ?` escape ?g ?v ?o ?V ?j ?j] 0 "%d"))

