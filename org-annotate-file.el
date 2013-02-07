;;; org-annotate-file.el --- Annotate a file with org syntax

;; Copyright (C) 2008 Philip Jackson

;; Author: Philip Jackson <phil@shellarchive.co.uk>
;; Version: 0.1

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is yet another implementation to allow the annotation of a
;; file without modification of the file itself. The annotation is in
;; org syntax.

;; To use you might put the following in your .emacs:
;;
;; (require 'org-annotate-file)
;; (global-set-key (kbd "C-c C-l") 'org-annotate-file)
;;
;; To change the location of the annotation file:
;;
;; (setq org-annotate-file-storage-file "~/annotated.org")

(require 'org)

(defvar org-annotate-file-storage-file "~/.org-annotate-file.org"
  "Annotation file.")

(defvar org-annotate-file-always-open t
  "non-nil means always expand the full tree when you visit
`org-annotate-file-storage-file'.")

(defun org-annotate-file ()
  "Associate any buffer with an underlying file with an org
section."
  (interactive)
  (unless (buffer-file-name)
    (error "This buffer has no associated file."))
  (org-annotate-file-show-section))

(defun org-annotate-file-show-section (&optional buffer)
  (let* ((filename (abbreviate-file-name (or buffer (buffer-file-name))))
         (link (concat "[[file://" filename "][" filename "]]")))
    (with-current-buffer (find-file org-annotate-file-storage-file)
      (unless (org-mode-p)
        (org-mode))
      (goto-char (point-min))
      (widen)
      (when org-annotate-file-always-open
        (show-all))
      (unless (search-forward-regexp
               (concat "^* " (regexp-quote link)) nil t)
          (progn
            (goto-char (point-max))
            (unless (eq (current-column) 0)
              (insert "\n"))
            (insert (concat "* " link "\n"))))
      (beginning-of-line)
      (org-narrow-to-subtree))))

(provide 'org-annotate-file)
;;; org-annotate-file.el ends here

