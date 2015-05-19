;;; corral.el --- Incrementally wrap delimiters around s-expressions

;; Copyright (C) 2015 Kevin Liu
;; Author: Kevin Liu <nivekuil@gmail.com>
;; Created: 16 May 2015
;; Homepage: http://github.com/nivekuil/corral
;; Version: 0.1.1
;; Package-Version: 20150519.455

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package contains functions that incrementally wrap (or "corral")
;; s-expressions with delimiters, such as parentheses and brackets.
;; After calling one of the interactive commands, repeated calls will shift
;; the corral instead of inserting new delimiters, expanding the amount of
;; text contained within the delimiters.

;;; Code:

(require 'thingatpt)

(defcustom corral-preserve-point nil
  "Preserve the position of the point instead of following a delimiter."
  :type 'boolean
  :group 'corral)

(defvar corral--virtual-point 0
  "Virtual point position to use for shifting, when preserving the real point.")


(defun corral-wrap-backward (open close)
  "Wrap OPEN and CLOSE delimiters around sexp, leaving point at OPEN."
  ;; If char before point is whitespace, move to end of sexp first
  (when (string-match-p "\\S-" (char-to-string (char-before)))
    (beginning-of-sexp))
  (insert open)
  (save-excursion
    (forward-sexp) (insert close))
  (backward-char))

(defun corral-wrap-forward (open close)
  "Wrap OPEN and CLOSE around sexp, leaving point at CLOSE."
  ;; If char before point is a word, move to beginning of sexp first
  (when (string-match-p "\\S-" (char-to-string (char-before)))
    (beginning-of-sexp))
  (insert open)
  (forward-sexp)
  (insert close))

(defun corral-shift-backward (open close)
  "Shift OPEN delimiter backward one sexp.  CLOSE is not moved."
  (cond
   ((eq (char-before) open)
    (backward-char) (corral-shift-backward open close))
   ((eq (char-after) open)
    (delete-char 1) (backward-sexp) (insert open) (backward-char))
   (t (backward-sexp) (corral-shift-backward open close))))

(defun corral-shift-forward (open close)
  "Without moving OPEN, shift CLOSE delimiter forward one sexp."
  (cond
   ((eq (char-after) close)
    (forward-char) (corral-shift-forward open close))
   ((eq (char-before) close)
    (delete-char -1) (forward-sexp) (insert close))
   (t (forward-sexp) (corral-shift-forward open close))))


;;;###autoload
(defun corral-parentheses-forward ()
  "Wrap parentheses around sexp, moving point to the closing parentheses."
  (interactive)
  (save-excursion
    (if (or (eq last-command 'corral-parentheses-forward)
            (eq last-command 'corral-parentheses-backward))
        (progn (goto-char corral--virtual-point)
               (corral-shift-forward ?( ?)))
      (corral-wrap-forward ?( ?)))
    (setq corral--virtual-point (point)))
  (unless corral-preserve-point
    (goto-char corral--virtual-point)))

;;;###autoload
(defun corral-parentheses-backward ()
  "Wrap parentheses around sexp, moving point to the closing parentheses."
  (interactive)
  (save-excursion
    (if (or (eq last-command 'corral-parentheses-forward)
            (eq last-command 'corral-parentheses-backward))
        (progn (goto-char corral--virtual-point)
               (corral-shift-backward ?( ?)))
      (corral-wrap-backward ?( ?)))
    (setq corral--virtual-point (point)))
  (unless corral-preserve-point
    (goto-char corral--virtual-point)))

;;;###autoload
(defun corral-brackets-forward ()
  "Wrap brackets around sexp, moving point to the closing bracket."
  (interactive)
  (save-excursion
    (if (or (eq last-command 'corral-brackets-forward)
            (eq last-command 'corral-brackets-backward))
        (progn (goto-char corral--virtual-point)
               (corral-shift-forward ?[ ?]))
      (corral-wrap-forward ?[ ?]))
    (setq corral--virtual-point (point)))
  (unless corral-preserve-point
    (goto-char corral--virtual-point)))

;;;###autoload
(defun corral-brackets-backward ()
  "Wrap brackets around sexp, moving point to the opening bracket."
  (interactive)
  (save-excursion
    (if (or (eq last-command 'corral-brackets-forward)
            (eq last-command 'corral-brackets-backward))
        (progn (goto-char corral--virtual-point)
               (corral-shift-backward ?[ ?]))
      (corral-wrap-backward ?[ ?]))
    (setq corral--virtual-point (point)))
  (unless corral-preserve-point
    (goto-char corral--virtual-point)))

;;;###autoload
(defun corral-double-quotes-forward ()
  "Wrap double quotes around sexp, moving point to the closing double quote."
  (interactive)
  (save-excursion
    (if (or (eq last-command 'corral-double-quotes-forward)
            (eq last-command 'corral-double-quotes-backward))
        (progn (goto-char corral--virtual-point)
               (corral-shift-forward ?\" ?\"))
      (corral-wrap-forward ?\" ?\"))
    (setq corral--virtual-point (point)))
  (unless corral-preserve-point
    (goto-char corral--virtual-point)))

;;;###autoload
(defun corral-double-quotes-backward ()
  "Wrap double quotes around sexp, moving point to the opening double quote."
  (interactive)
  (save-excursion
    (if (or (eq last-command 'corral-double-quotes-forward)
            (eq last-command 'corral-double-quotes-backward))
        (progn (goto-char corral--virtual-point)
               (corral-shift-backward ?\" ?\"))
      (corral-wrap-backward ?\" ?\"))
    (setq corral--virtual-point (point)))
  (unless corral-preserve-point
    (goto-char corral--virtual-point)))

(provide 'corral)

;;; corral.el ends here
