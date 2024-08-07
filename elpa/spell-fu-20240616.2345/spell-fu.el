;;; spell-fu.el --- Fast & light spelling highlighter -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2020  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-spell-fu
;; Keywords: convenience
;; Version: 0.3
;; Package-Requires: ((emacs "26.2"))

;;; Commentary:

;; This package checks the spelling of on-screen text.
;;

;;; Usage

;;
;; Write the following code to your .emacs file:
;;
;;   (require 'spell-fu)
;;   (spell-fu-global-mode)
;;
;; Or with `use-package':
;;
;;   (use-package spell-fu)
;;   (spell-fu-global-mode)
;;
;; If you prefer to enable this per-mode, you may do so using
;; mode hooks instead of calling `spell-fu-global-mode'.
;; The following example enables this for org-mode:
;;
;;   (add-hook 'org-mode-hook
;;     (lambda ()
;;       (setq spell-fu-faces-exclude '(org-meta-line))
;;       (spell-fu-mode)))
;;

;;; Code:

;; ---------------------------------------------------------------------------
;; Require Dependencies

;; For `face-list-p'.
(require 'faces)
;; For variables we read `ispell-personal-dictionary' local dictionary, etc.
(require 'ispell)
;; For `string-blank-p'.
(require 'subr-x)


;; ---------------------------------------------------------------------------
;; Compatibility

(when (and (version< emacs-version "29.1") (not (and (fboundp 'pos-bol) (fboundp 'pos-eol))))
  (defun pos-bol (&optional n)
    "Return the position at the line beginning."
    (declare (important-return-value nil) (side-effect-free t))
    (let ((inhibit-field-text-motion t))
      (line-beginning-position n)))
  (defun pos-eol (&optional n)
    "Return the position at the line end."
    (declare (important-return-value nil) (side-effect-free t))
    (let ((inhibit-field-text-motion t))
      (line-end-position n))))


;; ---------------------------------------------------------------------------
;; Custom Variables

(defgroup spell-fu nil
  "Fast, configurable spell checking of visible text, updated on a timer."
  :group 'ispell)

(defcustom spell-fu-directory (locate-user-emacs-file "spell-fu" ".emacs-spell-fu")
  "The directory to store dictionary data."
  :type 'string)

(defcustom spell-fu-idle-delay 0.25
  "Idle time to wait before highlighting.
Set to 0.0 to highlight immediately (as part of syntax highlighting)."
  :type 'float)

(defcustom spell-fu-ignore-modes nil
  "List of major-modes to exclude when `spell-fu' has been enabled globally."
  :type '(repeat symbol))

(defcustom spell-fu-word-delimit-camel-case nil
  "Support camel-case for delimiting word boundaries.

So `HelloWorld' would be checked a two words instead of one.
This is performed as an additional check for words that would
otherwise be marked as incorrect."
  :type 'boolean)

(defcustom spell-fu-debug nil
  "Enable debug messages, use for troubleshooting unexpected behavior."
  :type 'boolean)

(defvar-local spell-fu-buffer-session-localwords nil
  "Optional buffer-local word-list of words.
This is intended to be set by file-locals or dir-locals.
Call `spell-fu-buffer-session-localwords-refresh' after run-time modifications.")

;;;###autoload
(put 'spell-fu-buffer-session-localwords 'safe-local-variable #'spell-fu-list-of-strings-p)

(define-obsolete-variable-alias
  'global-spell-fu-ignore-buffer 'spell-fu-global-ignore-buffer "0.4")

(defvar-local spell-fu-global-ignore-buffer nil
  "When non-nil, the global mode will not be enabled for this buffer.
This variable can also be a predicate function, in which case
it'll be called with one parameter (the buffer in question), and
it should return non-nil to make Global `spell-fu' Mode not
check this buffer.")

;; A spell-fu dictionary is a symbol, the variable value of which is a hash table with the
;; dictionary's words.  The symbol additionally has these properties:
;; - 'description - short human-readable description of the dictionary.
;; - 'update - function, called to update the words hash table.
;; - 'add-word - function, called to permanently add a word to the dictionary.
;;     Not set for read-only dictionaries.
;; - 'remove-word - ditto

(defvar-local spell-fu-dictionaries nil
  "List of dictionaries enabled in the current buffer.

Use the spell-fu-get-...-dictionary functions to construct values
suitable for populating this list.  To add a dictionary, please
use `spell-fu-dictionary-add'.")

(defface spell-fu-incorrect-face
  '((((supports :underline (:style wave))) :underline (:style wave :color "red"))
    (t :underline t :inherit error))
  "Face for incorrect spelling.")

;; See '-' as a word boundary \b, so 'full-screen' is detected as two words.
(defvar-local spell-fu-syntax-table
    (let ((table (copy-syntax-table (standard-syntax-table))))
      (modify-syntax-entry ?- "-" table)
      table)
  "The syntax table to use when scanning words.")

;; This regex handles:
;;
;; - ``Quotation'' <= don't match multiple trailing apostrophes.
;;     ^^^^^^^^^
;;
;; - we're <= Connect letters with a single apostrophe.
;;   ^^^^^
;;
;; - don't''join <= don't connect multiple apostrophes.
;;   ^^^^^  ^^^^
;;
;; - word' <= don't count the final apostrophe.
;;   ^^^^
(defvar-local spell-fu-word-regexp "\\b\\([[:alpha:]]+\\(['\u2019][[:alpha:]]+\\)?\\)\\b"
  "Regex used to scan for words to check.
Used by `spell-fu-check-range'.")

(defvar-local spell-fu-faces-include nil
  "List of faces to check or nil to include all.
Used by `spell-fu-check-range'.")

(defvar-local spell-fu-faces-exclude nil
  "List of faces not to check or nil to exclude none.
Used by `spell-fu-check-range'.")

(defvar-local spell-fu-check-range 'spell-fu-check-range-default
  "Function that takes a beginning & end points to check for the current buffer.

Users may want to write their own functions to have more control
over which words are being checked.

Notes:

- The ranges passed in a guaranteed to be on line boundaries.
- Calling `spell-fu-check-word' on each word.

- You may explicitly mark a range as incorrect using
  `spell-fu-mark-incorrect' which takes the range to mark as arguments.")

;; ---------------------------------------------------------------------------
;; Internal Variables

;; Use to ensure the cache is not from a previous release.
;; Only ever increase.
(defconst spell-fu--cache-version "0.2")

;; Keep track of the last overlay, this allows expanding the existing overlay where possible.
;; Useful since font-locking often uses multiple smaller ranges which can be merged into one range.
;; Always check this has not been deleted (has a valid buffer) before use.
(defvar-local spell-fu--idle-overlay-last nil)

;; Cache the result of: `(mapcar (lambda (dict) (symbol-value dict)) spell-fu-dictionaries)'
(defvar-local spell-fu--cache-table-list nil)

;; The buffer local dictionary generated from `spell-fu-buffer-session-localwords'.
(defvar-local spell-fu--buffer-localwords-cache-table nil)

;; Map `spell-fu-buffer-session-localwords' identity to existing
;; `spell-fu--buffer-localwords-cache-table' entries to avoid conversions from
;; word lists to dictionaries by checking if the conversion has already been done.
;;
;; NOTE: The keys are the objects for the local-word list,
;; so this relies on the lists being shared between buffers (not just matching contents).
(defvar spell-fu--buffer-localwords-global-cache-table-map nil)


;; ---------------------------------------------------------------------------
;; Internal Macros

;; Developer note, don't use this for logging the checking of individual words,
;; that is far too verbose, this is mainly for checking why dictionaries aren't
;; being properly initialized.

(defmacro spell-fu--debug-message (fmt &rest args)
  "Debug message logging passing FMT and ARGS to `message'."
  ;; When emacs 28.2 support is dropped,
  ;; this can simply check `spell-fu-debug', see: #36.
  (when (bound-and-true-p spell-fu-debug)
    `(apply #'message (list (concat "spell-fu-debug: " ,fmt) ,@args))))

;; ---------------------------------------------------------------------------
;; Dictionary Utility Functions

(defsubst spell-fu--canonicalize-word-downcase (word)
  "Return lowercase UTF-8 encoded WORD (must already be `downcase')."
  (encode-coding-string word 'utf-8))

(defsubst spell-fu--canonicalize-word (word)
  "Return lowercase UTF-8 encoded WORD."
  (spell-fu--canonicalize-word-downcase (downcase word)))


(defun spell-fu--default-dictionaries ()
  "Construct the default value of `spell-fu-dictionaries'."
  (declare (important-return-value t))
  (let ((result))
    ;; Push in reverse order (so first used dictionary is last).

    (cond
     (spell-fu-buffer-session-localwords
      (push (spell-fu-get-buffer-session-localwords-dictionary) result)
      (spell-fu--debug-message
       "default-dictionary: `spell-fu-buffer-session-localwords' found, using!"))
     (t
      (spell-fu--debug-message
       "default-dictionary: `spell-fu-buffer-session-localwords' not found, skipping!")))

    (cond
     ((and ispell-personal-dictionary (file-exists-p ispell-personal-dictionary))
      (push (spell-fu-get-personal-dictionary "default" ispell-personal-dictionary) result)
      (spell-fu--debug-message
       "default-dictionary: `ispell-personal-dictionary' found at \"%s\" using!"
       ispell-personal-dictionary))
     (t
      (spell-fu--debug-message
       "default-dictionary: `ispell-personal-dictionary' not found, skipping!")))

    (let ((dict (or ispell-local-dictionary ispell-dictionary "default")))
      (push (spell-fu-get-ispell-dictionary dict) result)
      (spell-fu--debug-message "default-dictionary: name \"%s\" main ispell dictionary!" dict))

    result))

(defun spell-fu--dictionary-ensure-update (dict)
  "Call DICT update function if it exists."
  (declare (important-return-value nil))
  (let ((update-fun (get dict 'update)))
    (when update-fun
      (funcall update-fun)
      (spell-fu--debug-message "updating [%s], found [%d] word(s)"
                               (get dict 'description)
                               (hash-table-size (symbol-value dict))))))

(defun spell-fu--dictionaries-test-any (test-fn)
  "Remove any dictionaries that match TEST-FN."
  (declare (important-return-value t))
  (let ((result nil))
    (let ((dict-list spell-fu-dictionaries))
      (while dict-list
        (let ((dict (pop dict-list)))
          (when (funcall test-fn dict)
            (setq result t)
            (setq dict-list nil)))))
    result))

(defun spell-fu--dictionaries-remove-any (test-fn)
  "Return non-nil if any dictionaries match TEST-FN."
  (declare (important-return-value nil))
  (setq spell-fu-dictionaries
        (remq
         nil
         (mapcar
          (lambda (dict)
            (cond
             ((funcall test-fn dict)
              dict)
             (t
              nil)))
          spell-fu-dictionaries))))

(defun spell-fu--cache-file (dict)
  "Return the location of the cache file with dictionary DICT."
  (declare (important-return-value t))
  (expand-file-name (format "words_%s.el.data" (symbol-name dict)) spell-fu-directory))

(defun spell-fu--words-file (dict)
  "Return the location of the word-list with dictionary DICT."
  (declare (important-return-value t))
  (expand-file-name (format "words_%s.txt" (symbol-name dict)) spell-fu-directory))

(defun spell-fu--refresh-cache-table-list ()
  "Refresh internal list `spell-fu--cache-table-list'."
  (declare (important-return-value nil))
  (setq spell-fu--cache-table-list
        (mapcar (lambda (dict) (symbol-value dict)) spell-fu-dictionaries)))

(defun spell-fu--refresh ()
  "Reset spell-checked overlays in the current buffer."
  (declare (important-return-value nil))
  ;; For now simply clear syntax highlighting.
  (unless (<= spell-fu-idle-delay 0.0)
    (spell-fu--idle-overlays-remove))
  (spell-fu--overlays-remove)
  (font-lock-flush))

(defun spell-fu--buffers-refresh-with-dict (dict)
  "Reset spell-checked overlays for buffers using the dictionary DICT."
  (declare (important-return-value nil))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (bound-and-true-p spell-fu-mode)
                 (bound-and-true-p spell-fu-dictionaries)
                 (member dict spell-fu-dictionaries))
        (spell-fu--refresh)))))

(defun spell-fu--get-edit-candidate-dictionaries (word action)
  "Return dictionaries for which it make sense to perform ACTION on WORD.

ACTION is `'remove' or `'add'.  Returned candidates are dictionaries
which support the operation, and correspondingly do / do not
already contain WORD."
  (declare (important-return-value t))
  (let ((adding (eq action 'add))
        (encoded-word (spell-fu--canonicalize-word word)))
    (delq
     nil
     (mapcar
      (lambda (dict)
        (and
         ;; Operation supported?
         (get
          dict
          (cond
           (adding
            'add-word)
           (t
            'remove-word)))
         ;; Word is / is not in dictionary?
         (eq adding (null (gethash encoded-word (symbol-value dict))))
         ;; Result.
         dict))
      spell-fu-dictionaries))))

(defun spell-fu--read-dictionary (candidate-dicts prompt)
  "Ask the user to select one dictionary from CANDIDATE-DICTS.
PROMPT is shown to the users completing read."
  (declare (important-return-value t))
  (cond
   ((<= (length candidate-dicts) 1)
    ;; Return the single choice
    (car candidate-dicts))
   (t
    (let ((completion-extra-properties
           '(:annotation-function (lambda (candidate) (get (intern candidate) 'description)))))
      (intern (completing-read prompt (mapcar #'symbol-name candidate-dicts)))))))


;; ---------------------------------------------------------------------------
;; Generic Utility Functions
;;
;; Helpers, not directly related to checking spelling.
;;

(defmacro spell-fu--with-advice (advice &rest body)
  "Execute BODY with ADVICE temporarily enabled.

Advice are triplets of (SYMBOL HOW FUNCTION),
see `advice-add' documentation."
  (declare (indent 1))
  (let ((advice-list advice)
        (body-let nil)
        (body-advice-add nil)
        (body-advice-remove nil)
        (item nil))
    (unless (listp advice-list)
      (error "Advice must be a list"))
    (cond
     ((null advice-list)
      (error "Advice must be a list containing at least one item"))
     (t
      (while (setq item (pop advice-list))
        (unless (and (listp item) (eq 3 (length item)))
          (error "Each advice must be a list of 3 items"))
        (let ((fn-sym (gensym))
              (fn-advise (pop item))
              (fn-advice-ty (pop item))
              (fn-body (pop item)))
          ;; Build the calls for each type.
          (push (list fn-sym fn-body) body-let)
          (push (list 'advice-add fn-advise fn-advice-ty fn-sym) body-advice-add)
          (push (list 'advice-remove fn-advise fn-sym) body-advice-remove)))
      (setq body-let (nreverse body-let))
      (setq body-advice-add (nreverse body-advice-add))

      ;; Compose the call.
      `(let ,body-let
         (unwind-protect
             (progn
               ,@body-advice-add
               ,@body)
           ,@body-advice-remove))))))

(defmacro spell-fu--with-message-prefix (prefix &rest body)
  "Add text before the message output.
Argument PREFIX is the text to add at the start of the message.
Optional argument BODY runs with the message prefix."
  (declare (indent 1))
  `(spell-fu--with-advice ((#'message
                            :around
                            (lambda (fn-orig arg &rest args)
                              (apply fn-orig
                                     (append (list (concat "%s" arg)) (list ,prefix) args)))))
     ,@body))

(defmacro spell-fu--with-add-hook-depth-override (depth-override &rest body)
  "Support overriding the depth of a hook added by an indirect call.
Argument DEPTH-OVERRIDE the depth value to call `add-hook' with.
Optional argument BODY runs with the depth override."
  (declare (indent 1))
  `(spell-fu--with-advice ((#'add-hook
                            :around
                            (lambda (fn-orig hook function &optional _depth local)
                              (funcall fn-orig hook function ,depth-override local))))
     ,@body))

(defmacro spell-fu--setq-expand-range-to-line-boundaries (pos-beg pos-end)
  "Set POS-BEG the the line beginning, POS-END to the line end."
  ;; Ignore field boundaries.
  (let ((inhibit-field-text-motion t))
    `(save-excursion
       ;; Extend the ranges to line start/end.
       (goto-char ,pos-end)
       (setq ,pos-end (pos-eol))
       (goto-char ,pos-beg)
       (setq ,pos-beg (pos-bol)))))

(defun spell-fu--buffer-as-line-list (buffer lines)
  "Add lines from BUFFER to LINES, returning the updated LINES."
  (declare (important-return-value t))
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (push (buffer-substring-no-properties (pos-bol) (pos-eol)) lines)
        (forward-line 1))))
  lines)

(defun spell-fu--removed-changed-overlay (overlay after _beg _end &optional _len)
  "Hook for removing OVERLAY which is being edited.
Argument AFTER, ignore when true."
  (declare (important-return-value nil))
  (unless after
    (delete-overlay overlay)))

(defun spell-fu--faces-at-point (pos)
  "Add the named faces that the `read-face-name' or `face' property use.
Argument POS return faces at this point."
  (declare (important-return-value t))
  (let ((faces nil) ; List of faces to return.
        ;; NOTE: use `get-text-property' instead of `get-char-property' so overlays are excluded,
        ;; since this causes overlays with `hl-line-mode' (for example) to mask other faces.
        ;; If we want to include faces of overlays, this could be supported.
        (faceprop (or (get-text-property pos 'read-face-name) (get-text-property pos 'face))))
    (cond
     ((facep faceprop)
      (push faceprop faces))
     ((face-list-p faceprop)
      (dolist (face faceprop)
        (when (facep face)
          (push face faces)))))
    faces))

(defun spell-fu--next-faces-prop-change (pos limit)
  "Return the next face change from POS restricted by LIMIT."
  (declare (important-return-value t))
  (next-single-property-change pos 'read-face-name
                               nil
                               (next-single-property-change pos 'face nil limit)))

(defun spell-fu--file-is-older-list (file-test file-list)
  "Return t when FILE-TEST is older than any files in FILE-LIST."
  (declare (important-return-value t))
  (catch 'result
    (let ((file-test-time (file-attribute-modification-time (file-attributes file-test))))
      (dolist (file-new file-list)
        (when (time-less-p
               file-test-time (file-attribute-modification-time (file-attributes file-new)))
          (throw 'result t)))
      nil)))

(defun spell-fu--file-is-older (file-test &rest file-list)
  "Return t when FILE-TEST is older than any files in FILE-LIST."
  (declare (important-return-value t))
  (spell-fu--file-is-older-list file-test file-list))

;; Auto load as this is a callback for `safe-local-variable'.
;;;###autoload
(defun spell-fu-list-of-strings-p (obj)
  "Return t when OBJ is a list of strings."
  (declare (important-return-value t))
  (and (listp obj) (not (memq nil (mapcar #'stringp obj)))))

;; ---------------------------------------------------------------------------
;; Word List Cache

(defun spell-fu--cache-from-word-list-impl (words-file cache-file)
  "Create CACHE-FILE from WORDS-FILE.

The resulting cache is returned as a minor optimization for first-time loading,
where we need to create this data in order to write it,
save some time by not spending time reading it back."
  (declare (important-return-value t))
  (message "%S" (file-name-nondirectory cache-file))
  (let ((cache-header
         ;; The header, an associative list of items.
         (list (cons "version" spell-fu--cache-version)))
        (word-table nil)
        ;; Needed for Windows to prevent CRLF including new-lines in strings.
        (coding-system-for-read 'utf-8-unix)
        (coding-system-for-write 'utf-8-unix))

    (with-temp-buffer
      (insert-file-contents-literally words-file)
      (setq word-table (make-hash-table :test #'equal :size (count-lines (point-min) (point-max))))
      (while (not (eobp))
        (let ((l (buffer-substring-no-properties (pos-bol) (pos-eol))))
          ;; Value of 't' is just for simplicity, it's no used except for check the item exists.
          (puthash (spell-fu--canonicalize-word l) t word-table)
          (forward-line 1))))

    ;; Write write it to a file.
    (with-temp-buffer
      (prin1 cache-header (current-buffer))
      (prin1 word-table (current-buffer))
      (write-region nil nil cache-file nil 0))

    ;; Return the resulting word table.
    word-table))

(defun spell-fu--cache-from-word-list (words-file cache-file)
  "Create CACHE-FILE from WORDS-FILE."
  (declare (important-return-value t))
  (spell-fu--with-message-prefix "Spell-fu generating cache: "
    (condition-case err
        (spell-fu--cache-from-word-list-impl words-file cache-file)
      (error
       ;; Should be rare: if the file is corrupt or cannot be read for any reason.
       (progn
         (message "failed, %s" (error-message-string err))
         nil)))))

(defun spell-fu--cache-words-load-impl (cache-file)
  "Return the Lisp content from reading CACHE-FILE.

On failure of any kind, return nil,
the caller will need to regenerate the cache."
  (declare (important-return-value t))
  (with-temp-buffer
    (insert-file-contents-literally cache-file)
    (goto-char (point-min))

    ;; Check header.
    (let ((cache-header (read (current-buffer))))
      (unless (listp cache-header)
        (error "Expected cache-header to be list, not %S" (type-of cache-header)))

      (let ((version (assoc-default "version" cache-header)))
        (unless (string-equal version spell-fu--cache-version)
          (error "Require cache version %S, not %S" spell-fu--cache-version version))))

    ;; Read the contents.
    (let ((word-table (read (current-buffer))))
      (unless (hash-table-p word-table)
        (error "Expected cache to contain a hash-table, not %S" (type-of word-table)))
      word-table)))

(defun spell-fu--cache-words-load (cache-file)
  "Return the Lisp content from reading CACHE-FILE."
  (declare (important-return-value t))
  (spell-fu--with-message-prefix "Spell-fu reading cache: "
    (condition-case err
        (spell-fu--cache-words-load-impl cache-file)
      (error
       ;; Should be rare: if the file is corrupt or cannot be read for any reason.
       (progn
         (message "failed, %s" (error-message-string err))
         nil)))))


;; ---------------------------------------------------------------------------
;; Explode Words (Calculate Extra Delimiters)

(defun spell-fu--maybe-explode-word-by-camel-case (word word-locase word-upcase)
  "Explode WORD by camel-case.
Arguments WORD-LOCASE & WORD-UPCASE are simply to avoid extra computation."
  (declare (important-return-value t))
  (let* ((was-caps t)
         (was-ignore nil)
         (word-length (length word))
         (i-prev word-length)
         (i word-length)
         ;; Build list in reverse so it's ordered from first to last.
         (result nil))
    (while (not (zerop i))
      (setq i (1- i))
      (let* ((ch-nat (aref word i))
             (ch-locase (aref word-locase i))
             (ch-upcase (aref word-upcase i))
             (is-caps (not (eq ch-nat ch-locase))))

        (cond
         ;; Ignore punctuation (typically apostrophe).
         ;; Needed for "TODO's" not to be split into ("TOD" "O's").
         ((eq ch-locase ch-upcase)
          (setq was-caps is-caps)
          (setq was-ignore t))
         (t
          (cond
           (was-ignore
            (setq was-ignore nil))
           ((and is-caps (not was-caps))
            (when (< i i-prev)
              (push (cons i i-prev) result)
              (setq i-prev i)))
           ((and was-caps (not is-caps))
            (let ((i-ofs (1+ i)))
              (when (< i-ofs i-prev)
                (push (cons i-ofs i-prev) result)
                (setq i-prev i-ofs)))))))
        (setq was-caps is-caps)))
    (when result
      (unless (zerop i-prev)
        (push (cons 0 i-prev) result)))
    result))

(defsubst spell-fu--maybe-explode-word-ex (word word-locase word-upcase)
  "Return a list of ranges or return nil when no delimiters found.
Uses WORD, WORD-LOCASE & WORD-UPCASE to calculate delimiting."
  (cond
   ;; The option to delimit by camel-case isn't enabled, early exit.
   ((null spell-fu-word-delimit-camel-case)
    nil)
   ;; Early exit common case Where only the first letter is capitalized.
   ((string-equal (substring word 1) (substring word-locase 1))
    nil)
   (t
    (spell-fu--maybe-explode-word-by-camel-case word word-locase word-upcase))))

(defun spell-fu--maybe-explode-word (word)
  "Explode WORD into components or return nil."
  (declare (important-return-value t))
  (spell-fu--maybe-explode-word-ex word (downcase word) (upcase word)))

;; ---------------------------------------------------------------------------
;; Shared Functions

(defun spell-fu--overlays-remove (&optional pos-beg pos-end)
  "Remove symbol `spell-fu-mode' overlays from current buffer.
If optional arguments POS-BEG and POS-END exist
remove overlays from range POS-BEG to POS-END
Otherwise remove all overlays."
  (declare (important-return-value nil))
  (remove-overlays pos-beg pos-end 'spell-fu-mode t))

(defun spell-fu-mark-incorrect (pos-beg pos-end)
  "Mark the text from POS-BEG to POS-END with incorrect spelling overlay."
  (declare (important-return-value nil))
  (let ((item-ov (make-overlay pos-beg pos-end)))
    (overlay-put item-ov 'spell-fu-mode t)
    (overlay-put item-ov 'face 'spell-fu-incorrect-face)
    (overlay-put item-ov 'modification-hooks 'spell-fu--removed-changed-overlay)
    (overlay-put item-ov 'insert-in-front-hooks 'spell-fu--removed-changed-overlay)
    (overlay-put item-ov 'insert-behind-hooks 'spell-fu--removed-changed-overlay)
    (overlay-put item-ov 'evaporate t)
    item-ov))

(defsubst spell-fu--check-word-in-dict-list (encoded-word)
  "Return t if ENCODED-WORD is found in `spell-fu-dictionaries'."
  (let ((found nil)
        (cache-table-list spell-fu--cache-table-list))
    (while cache-table-list
      (let ((cache-table (pop cache-table-list)))
        (when (gethash encoded-word cache-table nil)
          (setq found t)
          (setq cache-table-list nil))))
    found))


(defun spell-fu-check-word (pos-beg pos-end word)
  "Run the spell checker on a word.

Marking the spelling as incorrect using `spell-fu-incorrect-face' on failure.
Argument POS-BEG the beginning position of WORD.
Argument POS-END the end position of WORD."
  (declare (important-return-value nil))
  ;; Dictionary search.
  (let ((word-locase (downcase word)))
    (unless (spell-fu--check-word-in-dict-list (spell-fu--canonicalize-word-downcase word-locase))
      (let ((word-upcase (upcase word)))
        ;; Ignore all uppercase words.
        (unless (equal word word-upcase)
          (let ((bounds (spell-fu--maybe-explode-word-ex word word-locase word-upcase)))
            (cond
             ;; Handle bounds.
             (bounds
              (pcase-dolist (`(,beg . ,end) bounds)
                ;; Dictionary search.
                (let ((subword-locase (substring word-locase beg end)))
                  (unless (spell-fu--check-word-in-dict-list
                           (spell-fu--canonicalize-word-downcase subword-locase))
                    (let ((subword (substring word beg end))
                          (subword-upcase (substring word-upcase beg end)))
                      ;; Ignore all uppercase words.
                      (unless (equal subword subword-upcase)
                        (spell-fu-mark-incorrect (+ pos-beg beg) (+ pos-beg end))))))))
             (t
              ;; Mark as incorrect otherwise.
              (spell-fu-mark-incorrect pos-beg pos-end)))))))))

(defun spell-fu--word-at-point ()
  "Return the word at the current point or nil."
  (declare (important-return-value t))
  (let ((point-init (point))
        (pos-beg (pos-bol))
        (pos-end (pos-eol)))
    (save-excursion
      (goto-char pos-beg)
      (catch 'result
        (with-syntax-table spell-fu-syntax-table
          (save-match-data
            (while (re-search-forward spell-fu-word-regexp pos-end t)
              (let* ((match-beg (match-beginning 0))
                     (match-end (match-end 0))
                     (word (buffer-substring-no-properties match-beg match-end)))
                (when (and (<= match-beg point-init) (<= point-init match-end))
                  (let ((bounds
                         (or (spell-fu--maybe-explode-word word) (list (cons 0 (length word))))))
                    (pcase-dolist (`(,beg . ,end) bounds)
                      (when (and (<= (+ match-beg beg) point-init)
                                 (<= point-init (+ match-beg end)))
                        (throw 'result (substring word beg end))))))))))
        (throw 'result nil)))))


;; ---------------------------------------------------------------------------
;; Range Checking Commands
;;
;; These functions are value values for the `spell-fu-check-range' buffer local variable.
;;
;; Note that the callers of these function extends the range to line delimiters,
;; to ensure there no chance of the points being in the middle of a word.
;;

(defun spell-fu--check-faces-at-point (pos)
  "Check if position POS has faces that match include/exclude."
  (declare (important-return-value t))
  (let ((result (null spell-fu-faces-include))
        (faces-at-pos (spell-fu--faces-at-point pos)))
    (while faces-at-pos
      (let ((face (pop faces-at-pos)))
        (when (memq face spell-fu-faces-exclude)
          (setq faces-at-pos nil)
          (setq result nil))
        (when (and (null result) (memq face spell-fu-faces-include))
          (setq result t))))
    result))

(defun spell-fu--check-range-with-faces (pos-beg pos-end)
  "Check spelling for POS-BEG & POS-END.

This only checks the text matching face rules."
  (declare (important-return-value nil))
  (spell-fu--overlays-remove pos-beg pos-end)
  (with-syntax-table spell-fu-syntax-table
    (save-match-data ; For regex search.
      (save-excursion ; For moving the point.
        (save-restriction ; For narrowing.
          ;; Avoid duplicate calls that check if `pos-beg' passes the face test.
          (let ((ok-beg (spell-fu--check-faces-at-point pos-beg)))
            ;; It's possible the face changes part way through the word.
            ;; In practice this is likely caused by escape characters, e.g.
            ;; "test\nthe text" where "\n" may have separate highlighting.
            (while (< pos-beg pos-end)
              (let* ((point-end-iter ; Set to `ok-beg' next iteration to avoid duplicate checks.
                      (spell-fu--next-faces-prop-change pos-beg pos-end))
                     (ok-end-iter
                      (and (< point-end-iter pos-end)
                           (spell-fu--check-faces-at-point point-end-iter))))

                ;; No need to check faces of each word
                ;; as face-changes are being stepped over.
                (when ok-beg

                  ;; Extend `point-end-iter' out for as long as the face isn't being ignored,
                  ;; needed when `whitespace-mode' sets a margin,
                  ;; splitting words in this case isn't desirable, see: #16.
                  ;;
                  ;; This may also have some advantage
                  ;; in reducing the number of narrowing calls.
                  ;;
                  ;; NOTE: this could be made into an option.
                  ;; Currently there doesn't seem much need for this at the moment.
                  (while ok-end-iter
                    (setq point-end-iter (spell-fu--next-faces-prop-change point-end-iter pos-end))
                    (setq ok-end-iter
                          (and (< point-end-iter pos-end)
                               (spell-fu--check-faces-at-point point-end-iter))))

                  ;; Use narrowing so the regex correctly handles boundaries
                  ;; that happen to fall on face changes.
                  (narrow-to-region pos-beg point-end-iter)
                  (goto-char pos-beg)
                  (while (re-search-forward spell-fu-word-regexp point-end-iter t)
                    (let ((word-beg (match-beginning 0))
                          (word-end (match-end 0)))
                      (spell-fu-check-word
                       word-beg word-end (buffer-substring-no-properties word-beg word-end))))
                  (widen))

                (setq pos-beg point-end-iter)
                (setq ok-beg ok-end-iter)))))))))

(defun spell-fu--check-range-without-faces (pos-beg pos-end)
  "Check spelling for POS-BEG & POS-END, checking all text."
  (declare (important-return-value nil))
  (spell-fu--overlays-remove pos-beg pos-end)
  (with-syntax-table spell-fu-syntax-table
    (save-match-data
      (save-excursion
        (goto-char pos-beg)
        (while (re-search-forward spell-fu-word-regexp pos-end t)
          (let ((word-beg (match-beginning 0))
                (word-end (match-end 0)))
            (spell-fu-check-word word-beg word-end (match-string-no-properties 0))))))))

(defun spell-fu-check-range-default (pos-beg pos-end)
  "Check spelling POS-BEG & POS-END, checking comments and strings."
  (declare (important-return-value nil))
  (cond
   ((or spell-fu-faces-include spell-fu-faces-exclude)
    (spell-fu--check-range-with-faces pos-beg pos-end))
   (t
    (spell-fu--check-range-without-faces pos-beg pos-end))))


;; ---------------------------------------------------------------------------
;; Immediate Style (spell-fu-idle-delay zero or lower)

(defun spell-fu--font-lock-fontify-region (pos-beg pos-end)
  "Update spelling for POS-BEG & POS-END to the queue, checking all text."
  (declare (important-return-value nil))
  (spell-fu--setq-expand-range-to-line-boundaries
   ;; Warning these values are set in place.
   pos-beg pos-end)
  (funcall spell-fu-check-range pos-beg pos-end))

(defun spell-fu--immediate-enable ()
  "Enable immediate spell checking."
  (declare (important-return-value nil))

  ;; It's important this is added with a depth of 100,
  ;; because we want the font faces (comments, string etc) to be set so
  ;; the spell checker can read these values which may include/exclude words.
  (spell-fu--with-add-hook-depth-override 100
    (jit-lock-register #'spell-fu--font-lock-fontify-region)))

(defun spell-fu--immediate-disable ()
  "Disable immediate spell checking."
  (declare (important-return-value nil))
  (jit-lock-unregister #'spell-fu--font-lock-fontify-region)
  (spell-fu--overlays-remove))


;; ---------------------------------------------------------------------------
;; Timer Style (spell-fu-idle-delay over zero)

(defun spell-fu--idle-overlays-remove (&optional pos-beg pos-end)
  "Remove `spell-fu-pending' overlays from current buffer.
If optional arguments POS-BEG and POS-END exist
remove overlays from range POS-BEG to POS-END
Otherwise remove all overlays."
  (declare (important-return-value nil))
  (remove-overlays pos-beg pos-end 'spell-fu-pending t))

(defun spell-fu--idle-handle-pending-ranges-impl (visible-beg visible-end)
  "VISIBLE-BEG and VISIBLE-END typically from `window-start' and `window-end'.

Although you can pass in specific ranges as needed,
when checking the entire buffer for example."
  (declare (important-return-value nil))
  (let ((overlays-in-view (overlays-in visible-beg visible-end)))
    (while overlays-in-view
      (let ((item-ov (pop overlays-in-view)))
        (when (and (overlay-get item-ov 'spell-fu-pending)
                   ;; It's possible these become invalid while looping over items.
                   (overlay-buffer item-ov))

          ;; Window clamped range.
          (let ((pos-beg (max visible-beg (overlay-start item-ov)))
                (pos-end (min visible-end (overlay-end item-ov))))

            ;; Expand so we don't spell check half a word.
            (spell-fu--setq-expand-range-to-line-boundaries
             ;; Warning these values are set in place.
             pos-beg pos-end)

            (when (condition-case err
                      ;; Needed so the idle timer won't quit mid-spelling.
                      (let ((inhibit-quit nil))
                        (funcall spell-fu-check-range pos-beg pos-end)
                        t)
                    (error
                     (progn
                       ;; Keep since this should be very rare.
                       (message "Early exit 'spell-fu-mode': %s" (error-message-string err))
                       ;; Break out of the loop.
                       (setq overlays-in-view nil)
                       nil)))

              ;; Don't delete the overlay since it may extend outside the window bounds,
              ;; always delete the range instead.
              ;;
              ;; While we could remove everything in the window range,
              ;; avoid this because it's possible `spell-fu-check-range' is interrupted.
              ;; Allowing interrupting is important, so users may set this to a slower function
              ;; which doesn't lock up Emacs as this is run from an idle timer.
              (spell-fu--idle-overlays-remove pos-beg pos-end))))))))

(defun spell-fu--idle-handle-pending-ranges ()
  "Spell check the on-screen overlay ranges."
  (declare (important-return-value nil))
  (spell-fu--idle-handle-pending-ranges-impl (window-start) (window-end)))

(defun spell-fu--idle-font-lock-region-pending (pos-beg pos-end)
  "Track the range to spell check, adding POS-BEG & POS-END to the queue."
  (declare (important-return-value nil))
  (when (and spell-fu--idle-overlay-last (not (overlay-buffer spell-fu--idle-overlay-last)))
    (setq spell-fu--idle-overlay-last nil))

  (unless (and spell-fu--idle-overlay-last
               (let ((last-beg (overlay-start spell-fu--idle-overlay-last))
                     (last-end (overlay-end spell-fu--idle-overlay-last)))
                 (cond
                  ((> last-beg pos-end)
                   nil)
                  ((< last-end pos-beg)
                   nil)
                  (t ; Extend when overlap.
                   (move-overlay
                    spell-fu--idle-overlay-last (min pos-beg last-beg) (max pos-end last-end))
                   t))))

    (let ((item-ov (make-overlay pos-beg pos-end)))
      ;; Handy for debugging pending regions to be checked.
      ;; (overlay-put item-ov 'face '(:background "#000000" :extend t))
      (overlay-put item-ov 'spell-fu-pending t)
      (overlay-put item-ov 'evaporate 't)
      (setq spell-fu--idle-overlay-last item-ov)))

  ;; Use `inhibit-quit' as a way to check if `jit-lock-stealth' is in use.
  (when inhibit-quit
    (spell-fu--idle-handle-pending-ranges-impl pos-beg pos-end)))


;; ---------------------------------------------------------------------------
;; Internal Timer Management
;;
;; This works as follows:
;;
;; - The timer is kept active as long as the local mode is enabled.
;; - Entering a buffer runs the buffer local `window-state-change-hook'
;;   immediately which checks if the mode is enabled,
;;   set up the global timer if it is.
;; - Switching any other buffer wont run this hook,
;;   rely on the idle timer it's self running, which detects the active mode,
;;   canceling it's self if the mode isn't active.
;;
;; This is a reliable way of using a global,
;; repeating idle timer that is effectively buffer local.
;;

;; Global idle timer (repeating), keep active while the buffer-local mode is enabled.
(defvar spell-fu--global-timer nil)
;; When t, the timer will update buffers in all other visible windows.
(defvar spell-fu--dirty-flush-all nil)
;; When true, the buffer should be updated when inactive.
(defvar-local spell-fu--dirty nil)

(defun spell-fu--time-callback-or-disable ()
  "Callback that run the repeat timer."
  (declare (important-return-value nil))

  ;; Ensure all other buffers are highlighted on request.
  (let ((is-mode-active (bound-and-true-p spell-fu-mode)))
    ;; When this buffer is not in the mode, flush all other buffers.
    (cond
     (is-mode-active
      ;; Don't update in the window loop to ensure we always
      ;; update the current buffer in the current context.
      (setq spell-fu--dirty nil))
     (t
      ;; If the timer ran when in another buffer,
      ;; a previous buffer may need a final refresh, ensure this happens.
      (setq spell-fu--dirty-flush-all t)))

    (when spell-fu--dirty-flush-all
      ;; Run the mode callback for all other buffers in the queue.
      (dolist (frame (frame-list))
        (dolist (win (window-list frame -1))
          (let ((buf (window-buffer win)))
            (when (and (buffer-local-value 'spell-fu-mode buf)
                       (buffer-local-value 'spell-fu--dirty buf))
              (with-selected-frame frame
                (with-selected-window win
                  (with-current-buffer buf
                    (setq spell-fu--dirty nil)
                    (spell-fu--idle-handle-pending-ranges)))))))))
    ;; Always keep the current buffer dirty
    ;; so navigating away from this buffer will refresh it.
    (when is-mode-active
      (setq spell-fu--dirty t))

    (cond
     (is-mode-active
      (spell-fu--idle-handle-pending-ranges))
     (t ; Cancel the timer until the current buffer uses this mode again.
      (spell-fu--time-ensure nil)))))

(defun spell-fu--time-ensure (state)
  "Ensure the timer is enabled when STATE is non-nil, otherwise disable."
  (declare (important-return-value nil))
  (cond
   (state
    (unless spell-fu--global-timer
      (setq spell-fu--global-timer
            (run-with-idle-timer
             spell-fu-idle-delay
             :repeat #'spell-fu--time-callback-or-disable))))
   (t
    (when spell-fu--global-timer
      (cancel-timer spell-fu--global-timer)
      (setq spell-fu--global-timer nil)))))

(defun spell-fu--time-reset ()
  "Run this when the buffer was changed."
  (declare (important-return-value nil))
  ;; Ensure changing windows doesn't leave other buffers with stale highlight.
  (cond
   ((bound-and-true-p spell-fu-mode)
    (setq spell-fu--dirty-flush-all t)
    (setq spell-fu--dirty t)
    (spell-fu--time-ensure t))
   (t
    (spell-fu--time-ensure nil))))

(defun spell-fu--time-buffer-local-enable ()
  "Ensure buffer local state is enabled."
  (declare (important-return-value nil))
  ;; Needed in case focus changes before the idle timer runs.
  (setq spell-fu--dirty-flush-all t)
  (setq spell-fu--dirty t)
  (spell-fu--time-ensure t)
  (add-hook 'window-state-change-hook #'spell-fu--time-reset nil t))

(defun spell-fu--time-buffer-local-disable ()
  "Ensure buffer local state is disabled."
  (declare (important-return-value nil))
  (kill-local-variable 'spell-fu--dirty)
  (spell-fu--time-ensure nil)
  (remove-hook 'window-state-change-hook #'spell-fu--time-reset t))

(defun spell-fu--idle-enable ()
  "Enable the idle style of updating."
  (declare (important-return-value nil))
  ;; Unlike with immediate style, idle / deferred checking isn't as likely to
  ;; run before fonts have been checked.
  ;; Nevertheless, this avoids the possibility of spell checking
  ;; running before font-faces have been set.
  (spell-fu--with-add-hook-depth-override 100
    (jit-lock-register #'spell-fu--idle-font-lock-region-pending))
  (spell-fu--time-buffer-local-enable))

(defun spell-fu--idle-disable ()
  "Disable the idle style of updating."
  (declare (important-return-value nil))
  (jit-lock-unregister #'spell-fu--idle-font-lock-region-pending)
  (spell-fu--overlays-remove)
  (spell-fu--idle-overlays-remove)
  (spell-fu--time-buffer-local-disable)
  (kill-local-variable 'spell-fu--idle-overlay-last))

;; ---------------------------------------------------------------------------
;; Public Functions

(defun spell-fu-region (&optional pos-beg pos-end verbose)
  "Spell check the region between POS-BEG and POS-END.

The VERBOSE argument reports the findings."
  (declare (important-return-value nil))
  ;; Expand range to line bounds, when set.
  (when (or pos-beg pos-end)
    (unless pos-beg
      (setq pos-beg (point-min)))
    (unless pos-end
      (setq pos-end (point-max)))
    (spell-fu--setq-expand-range-to-line-boundaries
     ;; Warning these values are set in place.
     pos-beg pos-end))

  (setq pos-beg (or pos-beg (point-min)))
  (setq pos-end (or pos-end (point-max)))

  (jit-lock-fontify-now pos-beg pos-end)

  ;; Ensure idle timer is handled immediately.
  (cond
   ((<= spell-fu-idle-delay 0.0)
    nil)
   (t
    (spell-fu--idle-handle-pending-ranges-impl pos-beg pos-end)))

  (when verbose
    (let ((count 0))
      (dolist (item-ov (overlays-in pos-beg pos-end))
        (when (overlay-get item-ov 'spell-fu-mode)
          (setq count (1+ count))))
      (message "Spell-fu: %d misspelled word(s) found!" count))))


(defun spell-fu-buffer ()
  "Spell check the whole buffer."
  (declare (important-return-value nil))
  (interactive)
  (spell-fu-region nil nil t))

(defun spell-fu--goto-next-or-previous-error (dir)
  "Jump to the next or previous error using DIR.

Return t when found, otherwise nil."
  (declare (important-return-value nil))

  (unless (bound-and-true-p spell-fu-mode)
    (user-error "Spell-fu: enable `spell-fu-mode' before using this command!"))

  (let ((point-found-delta most-positive-fixnum) ; Track the closest point in a given line.
        (point-init (point))
        (point-prev nil)
        (point-found nil))
    (save-excursion
      (while (and (null point-found) (not (equal (point) point-prev)))
        (let ((pos-beg (pos-bol))
              (pos-end (pos-eol)))

          (jit-lock-fontify-now pos-beg pos-end)

          ;; Ensure idle timer is handled immediately.
          (cond
           ((<= spell-fu-idle-delay 0.0)
            nil)
           (t
            (spell-fu--idle-handle-pending-ranges-impl pos-beg pos-end)))

          (dolist (item-ov (overlays-in pos-beg pos-end))
            (when (overlay-get item-ov 'spell-fu-mode)
              (let ((item-beg (overlay-start item-ov))
                    (item-end (overlay-end item-ov)))
                (when (cond
                       ((< dir 0)
                        (< item-end point-init))
                       (t
                        (> item-beg point-init)))
                  (let ((test-delta (abs (- point-init item-beg))))
                    (when (< test-delta point-found-delta)
                      (setq point-found item-beg)
                      (setq point-found-delta test-delta))))))))
        (setq point-prev (point))
        (forward-line dir)))

    (cond
     (point-found
      (goto-char point-found)
      t)
     (t
      (message "Spell-fu: no %s spelling error found"
               (cond
                ((< dir 0)
                 "previous")
                (t
                 "next")))
      nil))))

(defun spell-fu-goto-next-error ()
  "Jump to the next error, return t when found, otherwise nil."
  (declare (important-return-value nil))
  (interactive)
  (spell-fu--goto-next-or-previous-error 1))

(defun spell-fu-goto-previous-error ()
  "Jump to the previous error, return t when found, otherwise nil."
  (declare (important-return-value nil))
  (interactive)
  (spell-fu--goto-next-or-previous-error -1))

(defun spell-fu-word-add (dict)
  "Add the current word to the dictionary DICT.

Return t when the word has been added."
  (declare (important-return-value nil))
  (interactive (list
                (spell-fu--read-dictionary
                 (spell-fu--get-edit-candidate-dictionaries
                  (spell-fu--word-at-point) 'add)
                 "Add to dictionary: ")))
  (let ((word (spell-fu--word-at-point)))
    (cond
     (dict
      (let ((encoded-word (spell-fu--canonicalize-word word)))
        (funcall (get dict 'add-word) encoded-word)
        (puthash encoded-word t (symbol-value dict))
        t))
     (t
      (message "Cannot add %S to any active dictionary." word)
      nil))))

(defun spell-fu-word-remove (dict)
  "Remove the current word from the dictionary DICT.

Return t when the word has been removed."
  (declare (important-return-value nil))
  (interactive (list
                (spell-fu--read-dictionary
                 (spell-fu--get-edit-candidate-dictionaries
                  (spell-fu--word-at-point) 'remove)
                 "Remove from dictionary: ")))
  (let ((word (spell-fu--word-at-point)))
    (cond
     (dict
      (let ((encoded-word (spell-fu--canonicalize-word word)))
        (funcall (get dict 'remove-word) encoded-word)
        (remhash encoded-word (symbol-value dict))
        t))
     (t
      (message "Cannot remove %S from any active dictionary." word)
      nil))))

(defun spell-fu-dictionary-add (dict)
  "Add DICT to the list of active dictionaries."
  (declare (important-return-value nil))
  (add-to-list 'spell-fu-dictionaries dict)
  (when (bound-and-true-p spell-fu-mode)
    (spell-fu--dictionary-ensure-update dict)
    (spell-fu--refresh-cache-table-list)
    (spell-fu--refresh)))

(defun spell-fu-dictionary-remove (dict)
  "Remove DICT from the list of active dictionaries."
  (declare (important-return-value nil))
  (setq spell-fu-dictionaries (delq dict spell-fu-dictionaries))
  (when (bound-and-true-p spell-fu-mode)
    (spell-fu--refresh-cache-table-list)
    (spell-fu--refresh)))

(defun spell-fu-reset ()
  "Reset spell-fu and it's cache, useful if the cache has somehow become invalid."
  (declare (important-return-value nil))
  (interactive)
  (let ((buffers-in-mode nil))
    (dolist (buf (buffer-list))
      (when (buffer-local-value 'spell-fu-mode buf)
        (push buf buffers-in-mode)))

    (with-demoted-errors "spell-fu-reset: %S"
      (dolist (buf buffers-in-mode)
        (with-current-buffer buf
          (spell-fu--mode-disable))))

    (when (file-directory-p spell-fu-directory)
      (delete-directory spell-fu-directory t nil))

    (with-demoted-errors "spell-fu-reset: %S"
      (dolist (buf buffers-in-mode)
        (with-current-buffer buf
          (spell-fu--mode-enable))))

    (message "spell-fu: reset complete%s"
             (cond
              (buffers-in-mode
               "")
              (t
               " no buffers in spell-fu found, generate cache on next use!")))))

;; ---------------------------------------------------------------------------
;; Ispell / Aspell dictionary support
;;

;; Word List Generation

(defun spell-fu--aspell-word-list-ensure (words-file dict-name)
  "Ensure the word list is generated for Aspell dictionary DICT-NAME.
Argument WORDS-FILE is the file to write the word list into.

Return t if the file was updated."
  (declare (important-return-value nil))
  (let* ((has-words-file (file-exists-p words-file))
         (dict-aspell-name (cadr (nth 5 (assoc dict-name ispell-aspell-dictionary-alist))))
         (dict-file (and dict-aspell-name (spell-fu--aspell-find-data-file dict-name)))
         (is-dict-outdated
          (and has-words-file
               dict-file
               (spell-fu--file-is-older
                words-file
                ;; Chase links is needed as checking the symbolic-link date isn't correct, #31.
                (file-chase-links dict-file))))
         ;; Return value, failure to run `aspell' leaves this nil.
         (updated nil))

    (when (or (not has-words-file) is-dict-outdated)

      (spell-fu--with-message-prefix "Spell-fu generating words: "
        (message "%S" (file-name-nondirectory words-file))

        ;; Build a word list, sorted case insensitive.
        (let ((word-list nil))

          ;; Insert dictionary from aspell.
          (with-temp-buffer
            (let ((aspell-bin
                   ;; Use the pre-configured aspell binary, or call aspell directly.
                   (or (and ispell-really-aspell ispell-program-name) (executable-find "aspell"))))

              (cond
               ((null aspell-bin)
                (message "\"aspell\" command not found!"))
               ((string-equal dict-name "default")
                (condition-case err
                    (progn
                      (call-process aspell-bin nil t nil "dump" "master")
                      (setq updated t))
                  (error
                   (message "failed to run \"aspell\" with default dictionary with error: %s"
                            (error-message-string err)))))
               (t
                (condition-case err
                    (progn
                      (call-process aspell-bin nil t nil "-d" dict-name "dump" "master")
                      (setq updated t))
                  (error
                   (message "failed to run aspell with %S dictionary with error: %s"
                            dict-name
                            (error-message-string err))))))

              ;; Check whether the dictionary has affixes, expand if necessary.
              (when updated
                (when (re-search-backward "^[[:alpha:]]*/[[:alnum:]]*$" nil t)
                  (let ((lang (spell-fu--aspell-lang-from-dict dict-name)))
                    (unless
                        (zerop
                         (shell-command-on-region
                          (point-min) (point-max)
                          (cond
                           (lang
                            (format "%s -l %s expand" aspell-bin lang))
                           (t
                            (format "%s expand" aspell-bin)))
                          t
                          t
                          ;; Output any errors into the message buffer instead of the word-list.
                          "*spell-fu word generation errors*"))
                      (message
                       "spell-fu: affix extension for dictionary '%s' failed (with language: %S)."
                       dict-name lang))
                    (goto-char (point-min))
                    (while (search-forward " " nil t)
                      (replace-match "\n"))))

                (setq word-list (spell-fu--buffer-as-line-list (current-buffer) word-list)))))

          ;; Case insensitive sort is important if this is used for `ispell-complete-word-dict'.
          ;; Which is a handy double-use for this file.
          (when updated
            (let ((word-list-ncase nil))
              (dolist (word word-list)
                (push (cons (downcase word) word) word-list-ncase))

              ;; Sort by the lowercase word.
              (setq word-list-ncase
                    (sort word-list-ncase (lambda (a b) (string-lessp (car a) (car b)))))

              ;; Needed for Windows to prevent CRLF including new-lines in strings.
              (let ((coding-system-for-write 'utf-8-unix))
                ;; Write to 'words-file'.
                (with-temp-buffer
                  (dolist (line-cons word-list-ncase)
                    (insert (cdr line-cons) "\n"))
                  (write-region nil nil words-file nil 0)))))))

      updated)))

;; Word List Initialization

(defun spell-fu--aspell-update (dict dict-name)
  "Set up the Aspell DICT, named DICT-NAME initializing it as necessary."
  (declare (important-return-value t))
  ;; Get the paths of temporary files,
  ;; ensure the cache file is newer, otherwise regenerate it.
  (let ((words-file (spell-fu--words-file dict))
        (cache-file (spell-fu--cache-file dict))
        ;; We have to reload the words hash table, if it was not yet loaded.
        (forced (not (symbol-value dict))))

    (when (or (spell-fu--aspell-word-list-ensure words-file dict-name) forced)
      ;; Load cache or create it, creating it returns the cache
      ;; to avoid some slow-down on first load.
      (set
       dict
       (or (and (file-exists-p cache-file)
                (not (spell-fu--file-is-older cache-file words-file))
                (spell-fu--cache-words-load cache-file))
           (spell-fu--cache-from-word-list words-file cache-file))))))


(defun spell-fu--aspell-find-data-file (dict-aspell-name)
  "For Aspell DICT-ASPELL-NAME, return an associated data file path or nil."
  (declare (important-return-value t))
  ;; Based on `ispell-aspell-find-dictionary'.

  ;; Make sure `ispell-aspell-dict-dir' is defined.
  (unless ispell-aspell-dict-dir
    (setq ispell-aspell-dict-dir (ispell-get-aspell-config-value "dict-dir")))

  ;; Make sure `ispell-aspell-data-dir' is defined.
  (unless ispell-aspell-data-dir
    (setq ispell-aspell-data-dir (ispell-get-aspell-config-value "data-dir")))

  ;; Try finding associated data-file. aspell will look for master .dat
  ;; file in `dict-dir' and `data-dir'. Associated .dat files must be
  ;; in the same directory as master file.
  (catch 'datafile
    (save-match-data
      (dolist (tmp-path (list ispell-aspell-dict-dir ispell-aspell-data-dir))
        ;; Try `xx.dat' first, strip out variant, country code, etc,
        ;; then try `xx_YY.dat' (without stripping country code),
        ;; then try `xx-alt.dat', for `de-alt' etc.
        (dolist (dict-re (list "^[[:alpha:]]+" "^[[:alpha:]_]+" "^[[:alpha:]]+-\\(alt\\|old\\)"))
          (let ((dict-match
                 (and (string-match dict-re dict-aspell-name) (match-string 0 dict-aspell-name))))
            (when dict-match
              (let ((fullpath (concat (file-name-as-directory tmp-path) dict-match ".dat")))
                (when (file-readable-p fullpath)
                  (throw 'datafile fullpath))))))))))

(defun spell-fu--aspell-lang-from-dict (dict-name)
  "Return the language of a DICT-NAME or nil if identification fails.

Supports aspell alias dictionaries, e.g. \"german\" or \"deutsch\",
for \"de_DE\" using Ispell's lookup routines.
The language is identified by looking for the data file
associated with the dictionary."
  (declare (important-return-value t))
  (unless ispell-aspell-dictionary-alist
    (ispell-find-aspell-dictionaries))
  (let ((dict-aspell-name (cadr (nth 5 (assoc dict-name ispell-aspell-dictionary-alist)))))
    (when dict-aspell-name
      (let ((data-file (spell-fu--aspell-find-data-file dict-aspell-name)))
        (when data-file
          (file-name-base data-file))))))

;; Dictionary Definition

(defun spell-fu-get-ispell-dictionary (name)
  "Get the ispell dictionary named NAME."
  (declare (important-return-value t))
  (let ((dict (intern (concat "spell-fu-ispell-words-" name))))
    (unless (boundp dict)
      ;; Start with no words - construct them lazily
      (set dict nil)
      ;; Set description
      (put dict 'description (concat "Ispell " name " dictionary"))
      ;; Set update function
      (put dict 'update (lambda () (spell-fu--aspell-update dict name))))
    dict))

;; ---------------------------------------------------------------------------
;; Personal dictionary support
;;

(defun spell-fu--personal-word-list-ensure (words-file personal-words-file)
  "Ensure the word list is generated for the personal dictionary DICT-NAME.
Argument WORDS-FILE is the destination file to write the word list into.
Argument PERSONAL-WORDS-FILE is the source file to read words from.

Return t if the file was updated."
  (declare (important-return-value nil))
  (let* ((has-words-file (file-exists-p words-file))
         (has-dict-personal (and personal-words-file (file-exists-p personal-words-file)))
         (is-dict-outdated
          (and has-words-file
               has-dict-personal
               ;; Chase links is needed as checking the symbolic-link date isn't correct, #31.
               (spell-fu--file-is-older words-file (file-chase-links personal-words-file)))))

    (when (or (not has-words-file) is-dict-outdated)

      (spell-fu--with-message-prefix "Spell-fu generating words: "
        (message "%S" (file-name-nondirectory words-file))

        ;; Build a word list, sorted case insensitive.
        (let ((word-list nil))

          ;; Insert the personal dictionary, stripping header and inserting a newline.
          (with-temp-buffer
            (when has-dict-personal
              (insert-file-contents personal-words-file)
              (goto-char (point-min))
              (when (looking-at "personal_ws-")
                (delete-region (pos-bol) (1+ (pos-eol))))
              (goto-char (point-max))
              (unless (eq ?\n (char-after))
                (insert "\n")))

            (setq word-list (spell-fu--buffer-as-line-list (current-buffer) word-list)))

          ;; Case insensitive sort is important if this is used for `ispell-complete-word-dict'.
          ;; Which is a handy double-use for this file.
          (let ((word-list-ncase nil))
            (dolist (word word-list)
              (push (cons (downcase word) word) word-list-ncase))

            ;; Sort by the lowercase word.
            (setq word-list-ncase
                  (sort word-list-ncase (lambda (a b) (string-lessp (car a) (car b)))))

            ;; Write to 'words-file'.
            (with-temp-buffer
              (dolist (line-cons word-list-ncase)
                (insert (cdr line-cons) "\n"))
              (write-region nil nil words-file nil 0)))))
      t)))

(defun spell-fu--personal-update (dict dict-file)
  "Set up the personal dictionary DICT, initializing it as necessary.
Argument DICT-FILE is the absolute path to the dictionary."
  (declare (important-return-value t))
  ;; Get the paths of temporary files,
  ;; ensure the cache file is newer, otherwise regenerate it.
  (let ((words-file (spell-fu--words-file dict))
        (cache-file (spell-fu--cache-file dict))
        ;; We have to reload the words hash table, if it was not yet loaded.
        (forced (not (symbol-value dict))))

    (when (or (spell-fu--personal-word-list-ensure words-file dict-file) forced)
      ;; Load cache or create it, creating it returns the cache
      ;; to avoid some slow-down on first load.
      (set
       dict
       (or (and (file-exists-p cache-file)
                (not (spell-fu--file-is-older cache-file words-file))
                (spell-fu--cache-words-load cache-file))
           (spell-fu--cache-from-word-list words-file cache-file))))))

(defun spell-fu--personal-word-add-or-remove (word dict dict-file action)
  "Apply ACTION to WORD for the personal dictionary DICT-FILE for dictionary DICT."
  (declare (important-return-value t))
  (catch 'result
    (spell-fu--with-message-prefix "Spell-fu: "
      (unless word
        (message "word not found!")
        (throw 'result nil))
      (unless dict-file
        (message "personal dictionary not defined!")
        (throw 'result nil))

      (with-temp-buffer
        (insert-file-contents-literally dict-file)

        ;; Ensure newline at EOF,
        ;; not essential but complicates sorted add if we don't do this.
        ;; also ensures we can step past the header which _could_ be a single line
        ;; without anything below it.
        (goto-char (point-max))
        (unless (string-blank-p (buffer-substring-no-properties (pos-bol) (pos-eol)))
          (insert "\n"))
        ;; Delete extra blank lines.
        ;; So we can use line count as word count.
        (while (and (zerop (forward-line -1))
                    (string-blank-p (buffer-substring-no-properties (pos-bol) (pos-eol))))
          (delete-region
           (pos-bol)
           (progn
             (forward-line -1)
             (point))))

        (goto-char (point-min))

        ;; Case insensitive.
        (let ((changed nil)
              (header-match
               (save-match-data
                 ;; Match a line like: personal_ws-1.1 en 66
                 (when (looking-at
                        (concat
                         "personal_ws-[[:digit:]\\.]+"
                         "[[:blank:]]+"
                         "[A-Za-z_]+"
                         "[[:blank:]]+"
                         "\\([[:digit:]]+\\)"))
                   (forward-line 1)
                   (match-data))))
              (word-point
               (save-match-data
                 (let ((case-fold-search t))
                   (when (re-search-forward (concat "^" (regexp-quote word) "[[:blank:]]*$") nil t)
                     (match-beginning 0))))))

          (cond
           ((eq action 'add)
            (when word-point
              (message "\"%s\" already in the personal dictionary." word)
              (throw 'result nil))


            (let ((keep-searching t))
              (while (and keep-searching
                          (string-lessp (buffer-substring-no-properties (pos-bol) (pos-eol)) word))
                (setq keep-searching (zerop (forward-line 1)))))

            (insert word "\n")

            (message "\"%s\" successfully added!" word)
            (setq changed t))

           ((eq action 'remove)
            (unless word-point
              (message "\"%s\" not in the personal dictionary." word)
              (throw 'result nil))

            ;; Delete line.
            (goto-char word-point)
            (delete-region (pos-bol) (or (and (zerop (forward-line 1)) (point)) (pos-eol)))

            (message "\"%s\" successfully removed!" word)
            (setq changed t))

           (t ; Internal error, should never happen.
            (error "Invalid action %S" action)))

          (when changed
            (when header-match
              (save-match-data
                (set-match-data header-match)
                (replace-match (number-to-string (1- (count-lines (point-min) (point-max))))
                               t
                               nil
                               nil
                               1)))

            (write-region nil nil dict-file nil 0)

            (spell-fu--buffers-refresh-with-dict dict)
            t))))))

(defun spell-fu-get-personal-dictionary (name dict-file)
  "Get the personal dictionary NAME.
Argument DICT-FILE is the absolute path to the dictionary."
  (declare (important-return-value t))
  (let ((dict (intern (concat "spell-fu-ispell-personal-" name))))
    (unless (boundp dict)
      ;; Start with no words - construct them lazily
      (set dict nil)
      ;; Set description
      (put dict 'description (format "Personal dictionary %s, located at %s" name dict-file))
      ;; Set update function
      (put dict 'update (lambda () (spell-fu--personal-update dict dict-file)))
      ;; Set add/remove functions
      (put
       dict
       'add-word
       (lambda (word) (spell-fu--personal-word-add-or-remove word dict dict-file 'add)))
      (put
       dict
       'remove-word
       (lambda (word) (spell-fu--personal-word-add-or-remove word dict dict-file 'remove))))
    dict))

;; ---------------------------------------------------------------------------
;; Buffer Local Words

(defun spell-fu--buffer-localwords-cache-table-update ()
  "Set `spell-fu--buffer-localwords-cache-table' from the local word list."
  (declare (important-return-value nil))
  (let ((word-table
         ;; Reuse the previous table if possible.
         (and spell-fu--buffer-localwords-global-cache-table-map
              (gethash spell-fu-buffer-session-localwords
                       spell-fu--buffer-localwords-global-cache-table-map
                       nil))))

    (unless word-table
      (setq word-table
            (make-hash-table :test #'equal :size (length spell-fu-buffer-session-localwords)))
      (dolist (word spell-fu-buffer-session-localwords)
        (puthash (spell-fu--canonicalize-word word) t word-table))
      (unless spell-fu--buffer-localwords-global-cache-table-map
        (setq spell-fu--buffer-localwords-global-cache-table-map
              (make-hash-table :test #'eq :weakness 'value)))
      (puthash
       spell-fu-buffer-session-localwords
       word-table
       spell-fu--buffer-localwords-global-cache-table-map))
    (setq spell-fu--buffer-localwords-cache-table word-table)))

(defun spell-fu--buffer-localwords-add-or-remove (word action)
  "Add or remove WORD from buffer local names depending on ACTION."
  (declare (important-return-value t))
  (catch 'result
    (spell-fu--with-message-prefix "Spell-fu: "
      (unless word
        (message "word not found!")
        (throw 'result nil))
      ;; Case insensitive.
      (let ((encoded-word (spell-fu--canonicalize-word word))
            (changed nil))
        (let ((word-in-dict (gethash encoded-word spell-fu--buffer-localwords-cache-table nil)))
          (cond
           ((eq action 'add)
            (when word-in-dict
              (message "\"%s\" already in the local dictionary." word)
              (throw 'result nil))

            (push encoded-word spell-fu-buffer-session-localwords)
            (spell-fu--buffer-localwords-cache-table-update)

            (message "\"%s\" successfully added!" word)
            (setq changed t))

           ((eq action 'remove)
            (unless word-in-dict
              (message "\"%s\" not in the personal dictionary." word)
              (throw 'result nil))

            (setq spell-fu-buffer-session-localwords
                  (delete encoded-word spell-fu-buffer-session-localwords))
            (spell-fu--buffer-localwords-cache-table-update)

            (message "\"%s\" successfully removed!" word)
            (setq changed t))

           (t ; Internal error, should never happen.
            (error "Invalid action %S" action)))

          (when changed
            ;; TODO: update file local variables?
            t))))))

(defun spell-fu-get-buffer-session-localwords-dictionary ()
  "Get the personal dictionary NAME.
Argument DICT-FILE is the absolute path to the dictionary."
  (declare (important-return-value t))
  (let ((dict 'spell-fu--buffer-localwords-cache-table))
    ;; Start with no words - construct them lazily
    (set dict nil)
    ;; Set description
    (put dict 'description "Buffer local dictionary")
    ;; Set update function
    (put dict 'update #'spell-fu--buffer-localwords-cache-table-update)
    ;; Set add/remove functions
    (put dict 'add-word (lambda (word) (spell-fu--buffer-localwords-add-or-remove word 'add)))
    (put
     dict 'remove-word (lambda (word) (spell-fu--buffer-localwords-add-or-remove word 'remove)))
    dict))

(defun spell-fu--buffer-localwords-dictionary-test (dict)
  "Return non-nil when DICT is a local-words dictionary."
  (declare (important-return-value t))
  (eq (get dict 'update) #'spell-fu--buffer-localwords-cache-table-update))

(defun spell-fu--buffer-localwords-update-impl ()
  "Implementation for `spell-fu-buffer-session-localwords-update'."
  (declare (important-return-value nil))
  (let ((do-refresh-cache-table-list nil)
        (has-localwords-dict
         (spell-fu--dictionaries-test-any #'spell-fu--buffer-localwords-dictionary-test)))
    (cond
     (spell-fu-buffer-session-localwords
      (unless has-localwords-dict ; Add dict.
        (setq spell-fu-dictionaries
              (append
               spell-fu-dictionaries (list (spell-fu-get-buffer-session-localwords-dictionary))))
        (setq do-refresh-cache-table-list t)))
     (t
      (when has-localwords-dict ; Remove dict.
        (spell-fu--dictionaries-remove-any #'spell-fu--buffer-localwords-dictionary-test)
        (setq do-refresh-cache-table-list t))))
    (cond
     (spell-fu-buffer-session-localwords
      (spell-fu--buffer-localwords-cache-table-update))
     (t
      (kill-local-variable 'spell-fu--buffer-localwords-cache-table)))
    (when do-refresh-cache-table-list
      (spell-fu--refresh-cache-table-list))))

;;;###autoload
(defun spell-fu-buffer-session-localwords-update ()
  "Refresh after changing `spell-fu-buffer-session-localwords'."
  (declare (important-return-value nil))
  (when (bound-and-true-p spell-fu-mode)
    (spell-fu--buffer-localwords-update-impl)))

;; ---------------------------------------------------------------------------
;; Define Minor Mode
;;
;; Developer note, use global hooks since these run before buffers are loaded.
;; Each function checks if the local mode is active before operating.

(defun spell-fu--mode-enable ()
  "Turn on option `spell-fu-mode' for the current buffer."
  (declare (important-return-value nil))

  (spell-fu--debug-message "enabling for buffer: %S, major-mode: [%S]" (current-buffer) major-mode)

  ;; Set the default dictionaries.
  (unless spell-fu-dictionaries
    (setq spell-fu-dictionaries (spell-fu--default-dictionaries)))

  ;; Ensure our path exists.
  (unless (file-directory-p spell-fu-directory)
    (make-directory spell-fu-directory))

  ;; Update dictionaries
  (dolist (dict spell-fu-dictionaries)
    (spell-fu--dictionary-ensure-update dict))

  (spell-fu--refresh-cache-table-list)

  ;; We may want defaults for other modes,
  ;; although keep this general.
  (cond
   ((derived-mode-p 'prog-mode)
    (unless spell-fu-faces-include
      (setq spell-fu-faces-include
            '(font-lock-comment-face font-lock-doc-face font-lock-string-face)))
    (unless spell-fu-faces-exclude
      (setq spell-fu-faces-exclude '(font-lock-constant-face)))))

  (cond
   ((<= spell-fu-idle-delay 0.0)
    (spell-fu--immediate-enable))
   (t
    (spell-fu--idle-enable))))

(defun spell-fu--mode-disable ()
  "Turn off option `spell-fu-mode' for the current buffer."
  (declare (important-return-value nil))

  (spell-fu--debug-message "disabling mode for buffer %S" (current-buffer))

  (kill-local-variable 'spell-fu--cache-table-list)

  (cond
   ((<= spell-fu-idle-delay 0.0)
    (spell-fu--immediate-disable))
   (t
    (spell-fu--idle-disable))))

;;;###autoload
(define-minor-mode spell-fu-mode
  "Toggle variable `spell-fu-mode' in the current buffer."
  :global nil

  (cond
   (spell-fu-mode
    (spell-fu--mode-enable))
   (t
    (spell-fu--mode-disable))))

(defun spell-fu--mode-turn-on ()
  "Enable the option `spell-fu-mode' where possible."
  (declare (important-return-value nil))
  (when (and
         ;; Not already enabled.
         (not spell-fu-mode)
         ;; Not in the mini-buffer.
         (not (minibufferp))
         ;; Not a special mode (package list, tabulated data ... etc)
         ;; Instead the buffer is likely derived from `text-mode' or `prog-mode'.
         (not (derived-mode-p 'special-mode))
         ;; Not explicitly ignored.
         (not (memq major-mode spell-fu-ignore-modes))
         ;; Optionally check if a function is used.
         (or (null spell-fu-global-ignore-buffer)
             (cond
              ((functionp spell-fu-global-ignore-buffer)
               (not (funcall spell-fu-global-ignore-buffer (current-buffer))))
              (t
               nil))))
    (spell-fu-mode 1)))

;;;###autoload
(define-globalized-minor-mode spell-fu-global-mode
  spell-fu-mode
  spell-fu--mode-turn-on)

(define-obsolete-function-alias 'global-spell-fu-mode #'spell-fu-global-mode "0.4")

(provide 'spell-fu)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; spell-fu.el ends here
