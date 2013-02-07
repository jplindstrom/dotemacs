
;; assuming confluence.el and xml-rpc.el are in your load path
(require 'confluence)

;; ;; note, all customization must be in *one* custom-set-variables block
;; (custom-set-variables
;;  ;; ... other custimization

;;  ;; confluence customization
;;  '(confluence-url "http://intranet/confluence/rpc/xmlrpc")
;;  '(confluence-default-space-alist (list (cons confluence-url "your-default-space-name"))))

(autoload 'confluence-get-page "confluence" nil t)

(eval-after-load "confluence"
  '(progn
     (require 'longlines)
     (progn
       (add-hook 'confluence-mode-hook 'longlines-mode)
       ;; (add-hook 'confluence-before-save-hook 'longlines-before-revert-hook)
       ;; (add-hook 'confluence-before-revert-hook 'longlines-before-revert-hook)
       (add-hook 'confluence-mode-hook '(lambda () (local-set-key "\C-j" 'confluence-newline-and-indent))))))

(defun cf-save-page ()
  "Saves the current confluence page and updates the buffer with the latest
page.
Really does nothing extra."
  t)



;; ;; keybindings (change to suit)

;; ;; open confluence page
;; (global-set-key "\C-xwf" 'confluence-get-page)

;; ;; setup confluence mode
;; (add-hook 'confluence-mode-hook
;;           '(lambda ()
;;              (local-set-key "\C-xw" confluence-prefix-map)))





(defun myorg-convert-line-outline-org-to-confluence (outline-line)
  "Return `outline-line` (a single line of org-mode text as confluence
markup"
  (let ((org-regex "\\(^\\*+\\) "))
    (if (string-match org-regex outline-line)
        (let* ((confluence-line (replace-regexp-in-string org-regex "" outline-line))
               (org-indent (match-string-no-properties 1 outline-line))
               (confluence-level (length org-indent))
               )
          (format "\n\nh%s. %s" confluence-level confluence-line)
          )
      outline-line)))
;; (myorg-convert-line-outline-org-to-confluence "** Hello there")

(defun myorg-convert-line-bullet-org-to-confluence (outline-line)
  "Return `outline-line` (a single line of org-mode text as confluence
markup"
  (replace-regexp-in-string  "^- " "* " outline-line))

(defun myorg-convert-text-outline-org-to-confluence (outline-text)
  "Return `outline-text` as confluence markup"
  (mapconcat
   'identity
   (mapcar 'myorg-convert-line-bullet-org-to-confluence
           (mapcar 'myorg-convert-line-outline-org-to-confluence
                   (split-string outline-text "\n")))
           "\n"))
;; (myorg-convert-text-outline-org-to-confluence "* Hello
;; ** There
;; * And
;; ** here too
;; ")

(defun myorg-copy-convert-region-outline-org-to-confluence (beg end)
  "Return `outline-text` as confluence markup"
  (interactive "r")
  (kill-new (myorg-convert-text-outline-org-to-confluence( buffer-substring beg end))))

(fset 'myorg-make-word-nolink
   [?y ?s ?a ?w ?\} ?l ?i ?n ?l ?: escape])


