(in-package :hs)


(defun collect-decs (body)
  (loop
    for xs on body
    for x = (car xs)
    while (typecase x
            (cons (eq (car x) 'declare))
            (string (cdr xs)))
    collect x into decs
    finally (return (values decs xs))))


(defmacro def-hs-macro (name args &body body)
  (mv-bind (decs rest) (collect-decs body)
    `(defmacro ,name ,args
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


(defvar *indent* 0
  "the current indentation level")

(defun indent (&optional (n *indent*))
  (fresh-line)
  (loop repeat n
    do (write-string "  ")))

(defmacro with-indent (n &body body)
  `(let ((*indent* (+ *indent* ,n)))
     ,@body))

(defun map-indent (fn xs &optional (n *indent*))
  (dolist (x xs)
    (indent n)
    (apply fn x)))


(defpackage :|haskell|
  (:nicknames :|hs|)
  (:documentation
   "shadowing keywords to use them as names in Haskell code"))

(defun shadow-haskell (x)
  (export (intern (string x) :|hs|) :|hs|))

(defmacro defshadow (macro args &body body)
  (mv-bind (decs rest) (collect-decs body)
    `(defmacro ,macro ,args
       ,@decs
       `(progn
          (shadow-haskell ',name)
          ,(progn ,@rest)))))


(defgeneric apply-macro (spec expr))

(defmethod/i apply-macro (_ expr) expr)

(defshadow def-syntax-macro (name args &body body)
  (with-gensyms (expr)
    `(defmethod/i apply-macro ((_ (eql ',name)) ,expr)
       (hs-macro-expand (ds-bind ,args (cdr ,expr) ,@body)))))

(defmacro defsynonym (synonym original)
  `(def-syntax-macro ,synonym (&rest args)
     `(,',original ,@args)))


(defvar *symbol-macros* (make-hash-table :test 'eq))

(defshadow |define-symbol-macro| (name expr)
  `(setf (gethash ',name *symbol-macros*) ',expr))


(defgeneric hs-macro-expand (expr))

(defmethod hs-macro-expand (expr) expr)

(defmethod hs-macro-expand ((expr symbol))
  (mv-bind (value present-p)
      (gethash expr *symbol-macros*)
    (if present-p
      (hs-macro-expand value)
      expr)))

(defmethod hs-macro-expand ((expr real))
  (if (< expr 0)
    `(|negate| ,(- expr))
    expr))

(defmethod hs-macro-expand ((expr vector))
  (if (stringp expr)
    expr
    `(|tuple| ,@(coerce expr 'list))))

(defmethod hs-macro-expand ((expr cons))
  (apply-macro (car expr) expr))


(defgeneric %haskell (expr)
  (:documentation "print EXPR as Haskell code"))

(definline haskell (expr)
  (%haskell (hs-macro-expand expr)))

(defmapc %haskells #'%haskell)
(defmapc haskells #'haskell)


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


(defgeneric apply-syntax (spec expr))

(defmethod/i apply-syntax (_ expr)
  (rechask expr))

(defshadow defsyntax (name args &body body)
  (with-gensyms (expr)
    `(defmethod/i apply-syntax ((_ (eql ',name)) ,expr)
       (ds-bind ,args (cdr ,expr) ,@body))))


(definline %haskell-top (expr)
  (if (atom expr)
    (%haskell expr)
    (apply-syntax (car expr) expr)))

(definline haskell-top (expr)
  (%haskell-top (hs-macro-expand expr)))

(defmapc %haskell-tops #'%haskell-top)
(defmapc haskell-tops #'haskell-top)

(defrechask arrange #'haskell-top ", ")


(defvar *specials* (make-hash-table :test 'eq))

(defvar *operators* (make-hash-table :test 'eq))

(defmacro defspecial (name &body body)
  `(progn
     (setf (gethash ',name *specials*) 'special)
     (defsyntax ,name ,@body)))

(defmacro defpattern (name &body body)
  `(progn
     (setf (gethash ',name *specials*) 'pattern)
     (defsyntax ,name ,@body)))

(definline keytypep (key symbol)
  (eq (gethash key *specials*) symbol))


(defmethod %haskell (expr) (princ expr))

(defmethod %haskell ((expr symbol))
  (let ((op (gethash expr *operators*)))
    (if op
      (with-paren (princ op))
      (princ expr))))

(defmethod %haskell ((expr character))
  (cond
    ((char= expr #\')
      (write-string "'\\''"))
    ((char= expr #\\)
      (write-string "'\\\\'"))
    ((graphic-char-p expr)
      (format t "'~c'" expr))
    (t (format t "'\\x~x'" (char-code expr)))))

(defmethod %haskell ((expr cons))
  (if (keytypep (car expr) 'pattern)
    (%haskell-top expr)
    (with-paren
      (%haskell-top expr))))


(defshadow defhasq (name string)
  `(defmethod/i %haskell ((_ (eql ',name)))
     (write-string ,string)))

(defhasq nil "()")


(load-relative "define.lisp")
(load-relative "macros.lisp")
(load-relative "specials.lisp")
(load-relative "cl-keywords.lisp")
(load-relative "functions.lisp")

;; Local Variables:
;; eval: (add-cl-indent-rule (quote mv-bind) (quote (&lambda 4 &body)))
;; eval: (add-cl-indent-rule (quote with-paren) (quote (&body)))
;; End:
