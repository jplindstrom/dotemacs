

;; ORG mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))



;; Performance workaround for large org files: slow if in overview
;; mode, but fast with unfolded
; So start unfolded to cache(?) all the lines
(setq org-startup-folded nil)
; but then restore overview display
(add-hook 'org-mode-hook (function (lambda () (org-overview))))



;; Restore C-tab to other-window
(org-defkey org-mode-map [(control tab)] nil)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-hide-leading-stars t)


(setq org-link-abbrev-alist
      '(
        ("google"   . "http://www.google.com/search?q=")
        ))

(setq org-todo-keyword-faces
      '(
        ("TODO"  . (:foreground "orange red" :background "white smoke" :weight bold))
        ("DOING" . (:foreground "red" :background "white smoke" :weight bold))
        ("WAIT"  . (:foreground "turquoise4" :background "white smoke" :weight bold))
        ("DONE"  . org-done)
        ))


(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-bullets-bullet-list '("★" "▷" "►" "⊙" "•" "•" "•" "•" "•" "•" "•" "•" "•" "•"))





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







()


(defun org-export-subtree-as-simple-markup ()
  "Export current subtreee."
  (interactive)
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
      (replace-string "\n* " "\n\n\nh1. ")

      ;; rename ** to h2.
      (goto-char (point-min))
      (replace-string "\n** " "\n\nh2. ")

      ;; rename *** and below to * and below
      (goto-char (point-min))
      (replace-string "\n***" "\n*")

      ;; Copy
      (kill-new (buffer-substring-no-properties (point-min) (point-max)))
      )
      )
    )
  )
