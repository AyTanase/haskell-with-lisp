(in-package :hs)


(defvar *indent* 0)

(defun indent (&optional (n *indent*))
  (fresh-line)
  (loop repeat n do (format t "  ")))

(defmacro with-indent (n &body body)
  `(let ((*indent* (+ *indent* ,n))) ,@body))

(defun map-indent (fn list &optional (n *indent*))
  (dolist (x list)
    (indent n)
    (apply fn x)))


(defun %type (vars type)
  (rechask vars ", ")
  (format t " :: ")
  (haskell type))

(defkeyword |type| (vars type)
  `(%type ',vars ',type))

(defsyntax |type| (var type)
  (with-paren (%type (list var) type)))


(defun tuple (xs)
  (with-paren (rechask xs ", ")))

(defpattern |tuple| (&rest xs) (tuple xs))


(defun %define-left (var)
  (if (or (atom var) (patternp (car var)))
    (haskell var)
    (rechask var)))

(defun %define-right (val assign)
  (labels ((gendef (value)
             (format t assign)
             (haskell value))
           (guard (condition value)
             (with-indent 1
               (indent)
               (format t "| ")
               (haskell condition)
               (gendef value))))
    (if (consp val)
      (case (car val)
        (|if| (destructuring-bind (x y &optional z) (cdr val)
                (guard x y)
                (if z (guard '|otherwise| z))))
        (|cond| (dolist (args (cdr val))
                  (apply #'guard args)))
        (t (gendef val)))
      (gendef val))))

(defun %define (var val &optional (assign " = "))
  (if (eq var '|type|)
    (apply #'%type val)
    (progn
      (%define-left var)
      (%define-right val assign))))

(defkeyword |define| (var val)
  `(%define ',var ',val))

(defsyntax |where| (defs val)
  (haskell val)
  (format t " where")
  (with-indent 1 (map-indent #'%define defs)))

(defsyntax |let| (defs val)
  (format t "let")
  (with-indent 1
    (map-indent #'%define defs)
    (indent)
    (format t "in ")
    (haskell val)))


(defun %class-derive (derive)
  (flet ((print-derive (derive)
           (with-paren (arrange derive))))
    (when (consp derive)
      (if (symbolp (car derive))
        (progn
          (with-pragma
            (format t "~@:(~a~)" (car derive)))
          (format t " ")
          (print-derive (cdr derive)))
        (print-derive derive))
      (format t " => "))))

(defun %class (key name derive defs)
  (format t "~a " key)
  (%class-derive derive)
  (rechask name)
  (when defs
    (format t " where")
    (with-indent 1
      (map-indent #'%define defs))))

(defkeyword |class| (name &optional derive &rest defs)
  `(%class '|class| ',name ',derive ',defs))

(defkeyword |instance| (name &optional derive &rest defs)
  `(%class '|instance| ',name ',derive ',defs))


(defun module-names (suppliedp names)
  (when suppliedp
    (format t " ")
    (with-paren (arrange names))))

(defun %defmodule (module suppliedp names)
  (format t "module ~a" module)
  (module-names suppliedp names)
  (format t " where"))

(defkeyword |defmodule| (module &optional (names nil suppliedp))
  `(%defmodule ',module ,suppliedp ',names))

(defun %import (module suppliedp names qualifiedp hidingp)
  (format t "import")
  (if qualifiedp (format t " qualified"))
  (format t " ~a" module)
  (if hidingp (format t " hiding"))
  (module-names suppliedp names))

(defmacro defimport (name qualifiedp)
  `(defkeyword ,name (module &optional (names nil suppliedp) hidingp)
     `(%import ',module ,suppliedp ',names ',',qualifiedp ',hidingp)))

(defimport |import| nil)
(defimport |require| t)


(defsyntax => (constraints type)
  (if (consp (car constraints))
    (with-paren (arrange constraints))
    (rechask constraints))
  (format t " => ")
  (haskell type))


(defun %deftype (name type)
  (format t "type ")
  (rechask name)
  (format t " = ")
  (haskell type))

(defkeyword |deftype| (name type) `(%deftype ',name ',type))


(defun %data-body (body)
  (cond
    ((atom body) (haskell body))
    ((and (consp (cdr body))
          (eq (cadr body) :|name|))
     (haskell (car body))
     (format t " { ")
     (%rechask (cddr body)
               #'(lambda (args)
                   (apply #'%type args))
               ", ")
     (format t " }"))
    (t (rechask body))))

(defun %data (name body deriving)
  (format t "data ")
  (rechask name)
  (format t " = ")
  (if (and (consp body) (eq (car body) '|or|))
    (%rechask (cdr body) #'%data-body " | ")
    (%data-body body))
  (when deriving
    (format t " deriving ")
    (with-paren (arrange deriving))))

(defkeyword |data| (name body &optional deriving)
  `(%data ',name ',body ',deriving))


(defsyntax |if| (x y z)
  (with-paren
    (format t "if ")
    (haskell x)
    (format t " then ")
    (haskell y)
    (format t " else ")
    (haskell z)))

(defsyntax |cond| (x &rest xs)
  (haskell (if xs `(|if| ,@x (|cond| ,@xs)) (second x))))


(defsyntax |case| (x &rest xs)
  (format t "case ")
  (haskell x)
  (format t " of")
  (with-indent 1
    (map-indent #'(lambda (x y) (%define x y " -> ")) xs)))


(defkeyword |extension| (&rest args)
  `(with-pragma
     (format t "LANGUAGE ~{~a~^, ~}" ',args)))


(defhasq :|as| "@")


(def-hs-macro |defconstant| (name expr)
  `(defhasq ,name (load-time-value (strhask ',expr) t)))


(def-hs-macro |defun| (name args &body body)
  `(|define| (,name ,@args) ,@body))

(defsyntax |labels| (fns &rest body)
  (haskell `(|where| ,(loop for (name args . values) in fns
                        collect `((,name ,@args) ,@values))
                     ,@body)))

;; Local Variables:
;; eval: (add-cl-indent-rule (quote with-paren) (quote (&body)))
;; eval: (add-cl-indent-rule (quote with-pragma) (quote (&body)))
;; End:
