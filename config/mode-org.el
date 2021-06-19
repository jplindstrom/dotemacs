

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
                           (push '("WAIT" . ?∅) prettify-symbols-alist)
                           (push '("BLOCKED" . ?∅) prettify-symbols-alist)
                           (push '("WONTDO"  . ?✅) prettify-symbols-alist)
                           (prettify-symbols-mode)
                           ))



;; Restore C-tab to other-window
(org-defkey org-mode-map [(control tab)] nil)

(define-key global-map "\C-cl" 'org-store-link)
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
   (dot . t)
   (plantuml . t)
   ;; (sh . t)
   ))

(defun my/org-confirm-babel-evaluate (lang body)
  (not (member lang '("dot" "plantuml"))))
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
