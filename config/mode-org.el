

;; ORG mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
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





