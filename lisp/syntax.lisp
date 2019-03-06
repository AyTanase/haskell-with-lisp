(in-package :hs)

(defmacro def-hs-macro (name args &body body)
  (mv-bind (decls rest) (collect-decls body)
    `(defmacro ,name ,args
       ,@decls
       `(progn ,(progn ,@rest) (fresh-line)))))


(defmacro defparen (name open close)
  (with-gensyms (body)
    `(defmacro ,name (&body ,body)
       `(progn
          (princ ,,open)
          ,@,body
          (princ ,,close)))))

(defparen with-paren "(" ")")
(defparen with-square-brackets "[" "]")


(defvar *indent-depth* 0
  "the current indentation depth")

(defun indent (&optional (depth *indent-depth*))
  (fresh-line)
  (loop repeat depth
    do (write-string "  ")))

(definline indent-if (test &optional (depth *indent-depth*))
  (if test
    (indent depth)
    (write-char #\Space)))

(defmacro with-indent (depth &body body)
  `(let ((*indent-depth* (+ *indent-depth* ,depth)))
     ,@body))

(defun map-indent
    (fn xs &key (apply t) (depth *indent-depth*))
  (dolist (x xs)
    (indent depth)
    (if apply
      (apply fn x)
      (funcall fn x))))


(unless (find-package :|hs|)
  (make-package :|haskell| :nicknames '(:|hs|)))

(defun shadow-haskell (x)
  (export (intern (string x) :|hs|) :|hs|))

(defmacro defshadow (macro args &body body)
  (mv-bind (decls rest) (collect-decls body)
    `(defmacro ,macro ,args
       ,@decls
       `(progn
          (shadow-haskell ',,(first args))
          ,(progn ,@rest)))))


(defgeneric apply-macro (spec expr))

(defmethod/i apply-macro (_ expr) expr)

(defshadow def-syntax-macro (name args &body body)
  (with-gensyms (expr)
    `(defmethod/i apply-macro ((_ (eql ',name)) ,expr)
       (hs-macro-expand (ds-bind ,args (cdr ,expr) ,@body)))))

(defmacro |defsynonym| (synonym original)
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
  (if (minusp expr)
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


(defun %map-hs (fn sep expr)
  (if (listp expr)
    (loop for (x . xs) on expr
      do (funcall fn x)
         (when (consp xs) (princ sep)))
    (funcall fn expr)))

(defmacro def-map-hs
    (name fn &optional (default `(error "~S: no separator" ',name)))
  (with-gensyms (expr sep)
    `(defun ,name (,expr &optional (,sep ,default))
       (%map-hs ,fn ,sep ,expr))))

(def-map-hs map-hs #'haskell " ")


(defgeneric apply-syntax (spec expr))

(defmethod/i apply-syntax (_ expr)
  (map-hs expr))

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

(def-map-hs arrange #'haskell-top ", ")


(defvar *keytypes* (make-hash-table :test 'eq))

(defvar *operators* (make-hash-table :test 'eq))

(declaim (inline get-keytype (setf get-keytype)))
(defaccessor get-keytype (key)
  (gethash key *keytypes*))

(definline keytypep (key type)
  (eq (get-keytype key) type))

(defmacro defspecial (name &body body)
  `(progn
     (setf (get-keytype ',name) 'special)
     (defsyntax ,name ,@body)))

(defmacro defpattern (name &body body)
  `(progn
     (setf (get-keytype ',name) 'pattern)
     (defsyntax ,name ,@body)))

(declaim (inline get-operator (setf get-operator)))
(defaccessor get-operator (key)
  (gethash key *operators*))


(defmethod %haskell (expr) (princ expr))

(defmethod %haskell ((expr symbol))
  (let ((op (get-operator expr)))
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
      (format t "'~C'" expr))
    (t (format t "'\\x~X'" (char-code expr)))))

(defmethod %haskell ((expr cons))
  (if (keytypep (car expr) 'pattern)
    (%haskell-top expr)
    (with-paren
      (%haskell-top expr))))


(defshadow defhasq (name expr)
  `(defmethod/i %haskell ((_ (eql ',name)))
     (princ ,expr)))

(defhasq nil "()")
