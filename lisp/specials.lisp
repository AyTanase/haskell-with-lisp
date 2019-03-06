(in-package :hs)


(defspecial |let| (defs &optional (val nil svar))
  (if defs
    (with-indent 1
      (write-string "let")
      (map-indent #'%define defs)
      (when svar
        (indent)
        (write-string "in ")
        (%define-print val)))
    (%define-print val)))


(shadow-haskell '|where|)

(setf (get-keytype '|where|) 'special)

(defmethod apply-syntax ((spec (eql '|where|)) expr)
  (apply-syntax '|let| expr))


(defspecial |if| (&rest args)
  (ds-bind (test then &optional (else nil else-supplied-p))
      (mapcar #'%define-expand args)
    (cond
      (else-supplied-p
        (let ((indentp (or (consp then) (consp else))))
          (with-indent 1
            (%haskell-tops "if " test)
            (indent-if indentp)
            (%haskell-tops "then " then)
            (indent-if indentp)
            (%haskell-tops "else " else))))
      ((truep test) (%haskell-top then))
      (t (error "if: The else-form is required because ~
                     the test-form ~S is neither ~S nor ~S."
                test '|True| '|otherwise|)))))


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
    (map-indent #'%define-print body :apply nil)))


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
    (loop for (x . xs) on args
      until (eq x :|to|)
      do (haskell-tops "," x)
      finally
         (write-string "..")
         (call-car #'haskell-top xs))))
