;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((lisp-mode
  (eval add-cl-indent-rule '|let|
        '(4 &body))
  (eval cl-indent-rules
        '(4 2 2 &body)
        'def-op-macro 'defbinop)
  (eval cl-indent-rules
        '(&body)
        'with-paren 'with-square-brackets)
  (eval cl-indent-rules
        '(&lambda 4 &body)
        'mv-bind 'ds-bind)))
