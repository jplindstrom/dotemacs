;; $Header: /cvsroot/erc/erc/erc-replace.el,v 1.3 2002/04/11 23:11:21 jwiegley Exp $

;; erc-replace.el -- wash and massage messages inserted into the buffer

;; Author: Andreas Fuchs <asf@void.at>
;; Maintainer: Mario Lang (mlang@delysid.org)
;; Version: 1.0 ($Revision: 1.3 $)
;; Keywords: IRC, client, Internet

;; Copyright (C) 2001 Andreas Fuchs <asf@void.at>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Commentary:

;; ERC is an IRC client for Emacs.

;; For more information, see the following URLs:
;; * http://sf.net/projects/erc/
;; * http://www.emacswiki.org/cgi-bin/wiki.pl?EmacsIRCClient

;; This piece of code can be integrated into ERC by putting:
;; (require 'erc-smiley)
;; and customizing the erc-replace-list variable.

;; Require:

;; Code:

(defcustom erc-replace-list nil
  "Alist describing text to be replaced by other text in incoming
messages.  This is useful for implementing a smiley-filter or a filter
for abusive words. This variable is a list of lists containing as
their first element a regexp and as their second element a replacement
function."
  :group 'erc
  :type '(repeat (list :tag "Search & Replace"
		  (choice :tag "Replacement Regexp"
		   (variable :tag "Variable name")
		   (string :tag "Literal String"))
		  (function :tag "Replacement function"))))

(defun erc-replace-insert-hook ()
  "Hook Function run from erc-insert-hook to replace regexps from messages."
  (mapcar (lambda (elt)
	    (goto-char (point-min))
	    (erc-replace-replacer elt))
	  erc-replace-list))

(defun erc-replace-replacer (what)
  (let ((replace-re (eval (nth 0 what)))
	(replace-fun (nth 1 what)))
    (while (re-search-forward replace-re nil t)
      (replace-match (funcall replace-fun (match-string 0))))))

(add-hook 'erc-insert-hook 'erc-replace-insert-hook)

;;; Actual Replacer stuff:
;; Helpful regexps

(defvar erc-replace-smiley-mouth-re "[][<\|/)(>pqPQO]")
(defvar erc-replace-smiley-nose-re "\\([-o^]\\)?")
(defvar erc-replace-smiley-eyes-re "[:;|8]")
(defvar erc-replace-smiley-haircut-re "\\([])><*]\\)?")

(defvar erc-replace-lefthanded-smiley-re
  (concat
   erc-replace-smiley-mouth-re
   erc-replace-smiley-nose-re
   erc-replace-smiley-eyes-re
   erc-replace-smiley-haircut-re)
  "Regexp matching left-handed smileys.
Examples:
(-:
q^:")

(defvar erc-replace-righthanded-smiley-re
  (concat
   erc-replace-smiley-haircut-re
   erc-replace-smiley-eyes-re
   erc-replace-smiley-nose-re
   erc-replace-smiley-mouth-re)
  "Regexp matching right-handed smileys.
Examples:
:-)
:o)")


;; Helpful Replacement Functions

(defun erc-replace-drop-function (found-text)
  "Returns an empty string, i.e. replace the match with nothing."
  "")

(defun erc-replace-smiley-reverse-function (smiley)
  "Returns a reversed smiley.
E.g.:
\")-:\" -> \":-)\"
\":-)\" -> \"(-:\"
\"q-:\" -> \":-p\" (notice the p instead of the q.
		    That's a feature, not a bug (-:"
  (let ((reversed-chars '((?q  . ?p)
			  (?P  . ?q)
			  (?<  . ?>)
			  (?>  . ?<)))
	(smiley-list    (string-to-list smiley)))
    (concat
     (reverse
      (mapcar (lambda (char)
		(let* ((rev-char-pair (assoc char reversed-chars))
		       (rev-char    (and rev-char-pair
					 (cdr rev-char-pair)))
		       (match-paren (matching-paren char)))
		  (or rev-char
		      match-paren
		      char)))
	      smiley-list)))))

(provide 'erc-replace)
