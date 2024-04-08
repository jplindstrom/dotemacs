;; Forward compat with Emacs 28? which may have native json parsing support
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-JSON.html
(when (< emacs-major-version 28)
  (defun json-available-p () nil))
