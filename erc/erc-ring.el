;; erc-ring.el -- Command history handling for erc using ring.el

;; Author: Alex Schroeder <alex@gnu.org>

;; This file is not part of GNU Emacs, but the same license applies.

(require 'erc)
(require 'comint)
(require 'ring)

(defvar erc-input-ring nil "Input ring for erc.")
(defvar erc-input-ring-index nil "Position in the input ring for erc.")

(define-key erc-mode-map "\M-p" 'erc-previous-command)
(define-key erc-mode-map "\M-n" 'erc-next-command)

(defun erc-input-ring-setup ()
  "Do the setup required so that we can use comint style input rings.
Call this function when setting up the mode."
  (set (make-local-variable 'erc-input-ring)
       (make-ring comint-input-ring-size))
  (set (make-local-variable 'erc-input-ring-index) nil))
(add-hook 'erc-mode-hook 'erc-input-ring-setup)

(defun erc-add-to-input-ring (s)
  "Add string S to the input ring and reset history position."
  (ring-insert erc-input-ring s)
  (setq erc-input-ring-index 0))
(add-hook 'erc-send-pre-hook 'erc-add-to-input-ring)

(defun erc-previous-command ()
  "Replace current command with the previous one from the history."
  (interactive)
  (let* ((len (ring-length erc-input-ring))
	 (s (ring-ref erc-input-ring erc-input-ring-index)))
    (erc-replace-current-command s)
    (setq erc-input-ring-index (ring-plus1 erc-input-ring-index len))))

(defun erc-next-command ()
  "Replace current command with the next one from the history."
  (interactive)
  (let ((len (ring-length erc-input-ring)))
    (setq erc-input-ring-index (ring-minus1 erc-input-ring-index len))
    (let ((s (ring-ref erc-input-ring erc-input-ring-index)))
      (erc-replace-current-command s))))

(defun erc-replace-current-command (s)
  "Replace current command with string S."
  ;; delete line
  (let ((inhibit-read-only t))
    (delete-region
     erc-insert-marker
     (point-max))
    (erc-display-prompt)
    (insert s)))

(provide 'erc-ring)
