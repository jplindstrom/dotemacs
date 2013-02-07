;;; vcs-activity-sparkline.el --- Version Vontrol Activity Sparkline in the Emacs mode line
;; Copyright (C) 2009- by Johan Lindstrom

;; Author: Johan Lindstrom <johanl aet buzzwordninja.com >

;; vcs-activity-sparkline.el is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; vcs-activity-sparkline.el is distributed in the hope that it will
;; be useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary


;; TODO:


;; Comments / suggestions and bug reports are welcome!

;; Development notes
;; -----------------


;;; Code:


(defvar vas/update-mode-line-p t) ; mode-line mark display or not
(defvar vas/state-mark-tooltip nil) ; mode-line tooltip display


(defun vas/sparkline-image (sparkline-image-file)
  (propertize " "
              'help-echo 'vas/state-mark-tooltip
              'display
              `(image :type png
                      :file ,sparkline-image-file
                      :ascent center
                      )))


;; (defun vas/uninstall-state-mark-mode-line ()
;;   (setq mode-line-format
;;         (remove-if #'(lambda (mode) (eq (car-safe mode)
;;                                         'vas/update-mode-line-p))
;;                    mode-line-format))
;;   (force-mode-line-update t))


;; (defun vas/update-state-mark (color)
;;   (vas/uninstall-state-mark-mode-line)
;;   (vas/install-state-mark-mode-line color))


;; (defun vas/update-mode-line ()
;;   "Update mode-line state dot mark properly"
;;   (when (and buffer-file-name (vas/in-vc-mode?))
;;     (vas/update-state-mark
;;      (vas/interprete-state-mode-color
;;       (vc-svn-state buffer-file-name)))))



(defun vas/install-mode-line (sparkline-image-file)
  (push `(vas/update-mode-line-p ,(vas/sparkline-image sparkline-image-file))
        mode-line-format)
  (force-mode-line-update t))


(defun vas/set-mode-line-maybe ()
  "Set the mode line if
* this Emacs supports images
* There's a supported VCS present
* This buffer file is under version control
* A sparkline could be created
"
  (interactive)
  (when (file-directory-p ".svn")
    (vas/install-mode-line (expand-file-name "/home/lindsj05/source/pips3/branches/gold_activity_sparkline/spark.png"))
    )
  )


(add-hook
 'find-file-hook
 (lambda ()
   (run-with-idle-timer
    1
    nil
    (lambda ()
      (when (buffer-live-p (current-buffer))
        (vas/set-mode-line-maybe))))))


;; Hook for visit file

