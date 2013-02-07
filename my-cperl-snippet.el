(defun my-snippet-cperl ()
  (define-abbrev-table 'cperl-mode-abbrev-table '())
  (snippet-with-abbrev-table
   'cperl-mode-abbrev-table
   ("if"      .  ">if ($${test}) {\n>$.\n>}")
   ("for"     .  ">for $$($${sequence}) {\n>$.\n>}")
   ("while"   .  ">while ($${test}) {\n>$.\n>}")
   ("map"     .  ">map {\n>$.\n>}")
   ("grep"    .  ">grep {\n>$.\n>}")
   ))

(add-hook 'cperl-mode-hook 'my-snippet-cperl)


