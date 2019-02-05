(in-package :hs)


(defun collect-decs (body)
  (loop
    for exps on body
    for expr = (car exps)
    while (typecase expr
            (cons (eq (car expr) 'declare))
            (string (cdr exps)))
    collect expr into decs
    finally (return (values decs exps))))


(defpackage :|haskell| (:nicknames :|hs|))

(defun shadow-haskell (x)
  (export (intern (string x) :|hs|) :|hs|))

(defmacro defshadow (macro args &body body)
  (mv-bind (decs rest) (collect-decs body)
    `(defmacro ,macro ,args
       ,@decs
       `(progn
          (shadow-haskell ',name)
          ,(progn ,@rest)))))


(defshadow def-hs-macro (name &body body)
  `(defmacro ,name ,@body))

(defmacro defkeyword (name args &body body)
  (mv-bind (decs rest) (collect-decs body)
    `(def-hs-macro ,name ,args
       ,@decs
       `(progn ,(progn ,@rest) (fresh-line)))))


(defmacro defparen (name open close)
  (with-gensyms (body)
    `(defmacro ,name (&body ,body)
       `(progn
          (write-string ,,open)
          ,@,body
          (write-string ,,close)))))

(defparen with-paren "(" ")")
(defparen with-square-brackets "[" "]")


(defgeneric haskell (x)
  (:documentation "Print X as Haskell code."))

(defun strhask (x)
  (with-output-to-string (*standard-output*)
    (haskell x)))

(defun haskells (&rest args)
  (mapc #'haskell args))


(defun %rechask (x fn between)
  (flet ((call-1 (xs)
           (funcall fn (car xs))
           (if (cdr xs)
             (write-string between))))
    (if (listp x)
      (mapl #'call-1 x)
      (funcall fn x))))

(defmacro defrechask (name fn default)
  (with-gensyms (x between)
    `(defun ,name (,x &optional (,between ,default))
       (%rechask ,x ,fn ,between))))

(defrechask rechask #'haskell " ")


(defgeneric macro-apply (spec expr)
  (:documentation "Expand macros in Haskell code."))

(defmethod macro-apply (spec expr)
  (declare (ignore spec))
  expr)

(declaim (inline hs-macro-expand))
(defun hs-macro-expand (expr)
  (if (atom expr)
    expr
    (macro-apply (car expr) expr)))

(defshadow def-syntax-macro (name args &body body)
  (with-gensyms (spec expr)
    `(defmethod macro-apply ((,spec (eql ',name)) ,expr)
       (declare (ignore ,spec))
       (hs-macro-expand (ds-bind ,args (cdr ,expr) ,@body)))))


(defshadow defapply-1 (method name fn)
  (with-gensyms (spec expr)
    `(defmethod ,method ((,spec (eql ',name)) ,expr)
       (declare (ignore ,spec))
       (apply ,fn (cdr ,expr)))))

(defmacro defapply (method name f)
  (flet ((generate (name f)
           `(defapply-1 ,method ,name ,f)))
    (if (atom name)
      (generate name f)
      `(let ((g ,f))
         ,@(loop for v in name
             collect (generate v 'g))))))


(defgeneric apply-syntax (spec expr)
  (:documentation "Apply syntax rules to EXPR."))

(defmethod apply-syntax (spec expr)
  (declare (ignore spec))
  (rechask expr))

(defmacro defsyntax (name &body body)
  `(defapply apply-syntax ,name #'(lambda ,@body)))

(defun haskell-top (x)
  (let ((expr (hs-macro-expand x)))
    (if (atom expr)
      (haskell expr)
      (apply-syntax (car expr) expr))))

(defun haskell-tops (&rest args)
  (mapc #'haskell-top args))

(defrechask arrange #'haskell-top ", ")


(defgeneric apply-sexp-rule (spec expr)
  (:documentation "Apply S-Expression syntax rules to EXPR."))

(defmethod apply-sexp-rule (spec expr)
  (with-paren
    (apply-syntax spec expr)))

(defmacro def-sexp-rule (name &body body)
  `(defapply apply-sexp-rule ,name #'(lambda ,@body)))


(defmacro defpattern (name &body body)
  `(let ((fn #'(lambda ,@body)))
     (defapply apply-syntax ,name fn)
     (defapply apply-sexp-rule ,name fn)))

(defshadow defhasq (name expr)
  `(defmethod haskell ((x (eql ',name)))
     (write-string ,expr)))


(defmethod haskell (x) (princ x))

(defmethod haskell ((x character))
  (cond
    ((char= x #\')
      (write-string "'\\''"))
    ((char= x #\\)
      (write-string "'\\\\'"))
    ((graphic-char-p x)
      (format t "'~c'" x))
    (t (format t "'\\x~x'" (char-code x)))))

(defmethod haskell ((x null))
  (write-string "()"))

(defmethod haskell ((x cons))
  (let ((expr (hs-macro-expand x)))
    (if (atom expr)
      (haskell expr)
      (apply-sexp-rule (car expr) expr))))


(load-relative "keywords.lisp")
(load-relative "cl-keywords.lisp")
(load-relative "unify.lisp")
(load-relative "functions.lisp")

;; Local Variables:
;; eval: (add-cl-indent-rule (quote mv-bind) (quote (&lambda 4 &body)))
;; eval: (add-cl-indent-rule (quote with-paren) (quote (&body)))
;; End:
