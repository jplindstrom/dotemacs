
;;
;; Add 8 additional font face levels to org-mode, because 8 is way too
;; small
;;
;; Probably also a good idea to do this:
;; (setq org-cycle-level-faces nil)


(defface org-level-9 '((t :inherit outline-8))
  "Face used for level 9 headlines."
  :group 'org-faces)

(defface org-level-10 '((t :inherit outline-8))
  "Face used for level 10 headlines."
  :group 'org-faces)

(defface org-level-11 '((t :inherit outline-8))
  "Face used for level 11 headlines."
  :group 'org-faces)

(defface org-level-12 '((t :inherit outline-8))
  "Face used for level 12 headlines."
  :group 'org-faces)

(defface org-level-13 '((t :inherit outline-8))
  "Face used for level 13 headlines."
  :group 'org-faces)

(defface org-level-14 '((t :inherit outline-8))
  "Face used for level 14 headlines."
  :group 'org-faces)

(defface org-level-15 '((t :inherit outline-8))
  "Face used for level 15 headlines."
  :group 'org-faces)

(defface org-level-16 '((t :inherit outline-8))
  "Face used for level 16 headlines."
  :group 'org-faces)

(defconst org-level-faces
  '(org-level-1 org-level-2 org-level-3 org-level-4
		org-level-5 org-level-6 org-level-7 org-level-8
		org-level-9 org-level-10 org-level-11 org-level-12
		org-level-13 org-level-14 org-level-15 org-level-16))


(setq org-n-level-faces (length org-level-faces))
