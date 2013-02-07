
;; (defun phil/fake-stdin-slurp (filename)
;;   "Emulate stdin slurp using emacsclient hack"
;;   (if (> (nth 7 (file-attributes filename)) 0)
;;       (progn
;;         (switch-to-buffer (generate-new-buffer "*stdin*"))
;;         (insert-file filename)
;;         (end-of-buffer))
;;     (message (concat (format-time-string "%H:%M:%S")
;;                      " - Filename with a nil size opened with client."))))


;; Pipe from shell to Emacs
;; http://web.archive.org/web/20070703163718/http://www.shellarchive.co.uk/Emacs.html
(defun phil/fake-stdin-slurp (filename)
  "Emulate stdin slurp using emacsclient hack"
  (switch-to-buffer (generate-new-buffer "*stdin*"))
  (insert-file filename)
  (end-of-buffer))





