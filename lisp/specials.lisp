(in-package :hs)


(defspecial |let| (defs val)
  (if defs
    (with-indent 1
      (write-string "let")
      (map-indent #'%define defs)
      (indent)
      (haskell-tops "in " val))
    (haskell-tops "let in " val)))


(shadow-haskell '|where|)

(setf (gethash '|where| *specials*) 'special)

(defmethod apply-syntax ((spec (eql '|where|)) expr)
  (apply-syntax '|let| expr))


(defspecial |if| (x y &optional (z nil svar))
  (cond
    ((not svar)
      (assert (truep x) () "if: missing the else-form")
      (haskell-top y))
    ((and (atom x) (atom y) (atom z))
      (haskells "if " x " then " y " else " z))
    (t (with-indent 1
         (haskell-tops "if " x)
         (indent)
         (haskell-tops "then " y)
         (indent)
         (haskell-tops "else " z)))))


(defspecial |case| (x &body xs)
  (flet ((case-val (x y)
           (%define x y " -> ")))
    (haskells "case " x " of")
    (with-indent 1
      (map-indent #'case-val xs))))


(defpattern |setf| (x y)
  (haskell-tops x " <- " y))

(defspecial |do| (&body body)
  (write-string "do")
  (with-indent 1
    (dolist (expr body)
      (indent)
      (haskell-top expr))))


(defspecial |lambda| (args expr)
  (write-char #\\)
  (rechask args)
  (write-string " -> ")
  (%define-print expr))

(defspecial |curry| (&rest args)
  (rechask args))

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
      for xs on args
      for x = (car xs)
      until (eq x :|to|)
      do (haskell-tops "," x)
      finally
         (write-string "..")
         (if (consp (cdr xs))
           (haskell-top (cadr xs))))))

;; Local Variables:
;; eval: (add-cl-indent-rule (quote with-square-brackets) (quote (&body)))
;; End:
