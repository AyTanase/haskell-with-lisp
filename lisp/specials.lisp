(in-package :hs)


(defspecial |let| (defs &optional (val nil svar))
  (when defs
    (with-indent 1
      (write-string "let")
      (map-indent #'%define defs)
      (when svar
        (indent)
        (write-string "in "))))
  (when svar
    (%define-print val)))


(shadow-haskell '|where|)

(setf (gethash '|where| *specials*) 'special)

(defmethod apply-syntax ((spec (eql '|where|)) expr)
  (apply-syntax '|let| expr))


(defspecial |if| (&rest args)
  (ds-bind (x y &optional (z nil svar))
      (mapcar #'%define-expand args)
    (cond
      ((not svar)
        (assert (truep x) () "if: missing the else-form")
        (%haskell-top y))
      ((and (atom y) (atom z))
        (%haskell-tops "if " x " then " y " else " z))
      (t (with-indent 1
           (%haskell-tops "if " x)
           (indent)
           (%haskell-tops "then " y)
           (indent)
           (%haskell-tops "else " z))))))


(defspecial |case| (x &body xs)
  (flet ((case-val (x y)
           (%define x y " -> ")))
    (write-string "case ")
    (%define-print x)
    (write-string " of")
    (with-indent 1
      (map-indent #'case-val xs))))


(defspecial |bind| (x y)
  (haskell-tops x " <- ")
  (%define-print y))

(defspecial |do| (&body body)
  (write-string "do")
  (with-indent 1
    (dolist (expr body)
      (indent)
      (%define-print expr))))


(defspecial |lambda| (args expr)
  (write-char #\\)
  (map-hs args)
  (write-string " -> ")
  (%define-print expr))

(defpattern |as| (var expr)
  (haskells var "@" expr))


(defhasq |list| "pure")

(defpattern |list| (&rest args)
  (with-square-brackets
    (arrange args)))

(defpattern |enum-from| (expr &rest args)
  (with-square-brackets
    (haskell-top expr)
    (loop
      for (x . xs) on args
      until (eq x :|to|)
      do (haskell-tops "," x)
      finally
         (write-string "..")
         (when (consp xs)
           (haskell-top (car xs))))))

;; Local Variables:
;; eval: (add-cl-indent-rule (quote with-square-brackets) (quote (&body)))
;; End:
