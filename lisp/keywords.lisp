(in-package :hs)


(defvar *indent* 0)

(defun indent (&optional (n *indent*))
  (fresh-line)
  (loop repeat n
    do (write-string "  ")))

(defmacro with-indent (n &body body)
  `(let ((*indent* (+ *indent* ,n)))
     ,@body))

(defun map-indent (fn list &optional (n *indent*))
  (dolist (x list)
    (indent n)
    (apply fn x)))


(defun %type (vars type)
  (rechask vars ", ")
  (write-string " :: ")
  (haskell type))

(defkeyword |type| (vars type)
  `(%type ',vars ',type))

(defsyntax |type| (var type)
  (with-paren
    (%type (list var) type)))


(defun tuple (xs)
  (with-paren
    (rechask xs ", ")))

(defpattern |tuple| (&rest xs) (tuple xs))


(defun has-guard-p (expr)
  (and (consp expr)
       (case (car expr)
         ((|if| |cond|) t))))

(defun %define-left (var)
  (if (or (atom var)
          (patternp (car var)))
    (haskell var)
    (rechask var)))

(defun %define-right (val assign)
  (labels ((gendef (value)
             (write-string assign)
             (haskell value))
           (guard (condition value)
             (with-indent 1
               (indent)
               (write-string "| ")
               (haskell condition)
               (gendef value))))
    (if (has-guard-p val)
      (case (car val)
        (|if| (destructuring-bind (x y &optional z) (cdr val)
                (guard x y)
                (if z
                  (guard '|otherwise| z))))
        (|cond| (dolist (args (cdr val))
                  (apply #'guard args))))
      (gendef val))))

(defun %define (var val &optional (assign " = "))
  (if (eq var '|type|)
    (%type val assign)
    (progn
      (%define-left var)
      (%define-right val assign))))

(defkeyword |define| (var val)
  `(%define ',var ',val))

(defsyntax |where| (defs val)
  (haskell val)
  (write-string " where")
  (with-indent 1
    (map-indent #'%define defs)))

(defsyntax |let| (defs val)
  (write-string "let")
  (with-indent 1
    (map-indent #'%define defs)
    (indent)
    (write-string "in ")
    (haskell val)))


(defun %class-derive (derive)
  (flet ((print-derive (derive)
           (with-paren
             (arrange derive))))
    (when (consp derive)
      (if (symbolp (car derive))
        (progn
          (with-pragma
            (format t "~@:(~a~)" (car derive)))
          (write-string " ")
          (print-derive (cdr derive)))
        (print-derive derive))
      (write-string " => "))))

(defun %class (key name derive defs)
  (format t "~a " key)
  (%class-derive derive)
  (rechask name)
  (when defs
    (write-string " where")
    (with-indent 1
      (map-indent #'%define defs))))

(defkeyword |class| (name &optional derive &rest defs)
  `(%class '|class| ',name ',derive ',defs))

(defkeyword |instance| (name &optional derive &rest defs)
  `(%class '|instance| ',name ',derive ',defs))

(defkeyword |defdata| (name &rest defs)
  `(%class '|data| ',name nil ',defs))


(defun module-names (suppliedp names)
  (when suppliedp
    (write-string " ")
    (with-paren
      (arrange names))))

(defun %defmodule (module suppliedp names)
  (format t "module ~a" module)
  (module-names suppliedp names)
  (write-string " where"))

(defkeyword |defmodule| (module &optional (names nil suppliedp))
  `(%defmodule ',module ,suppliedp ',names))

(defun %import (module suppliedp names qualifiedp hidingp)
  (write-string "import")
  (if qualifiedp
    (write-string " qualified"))
  (format t " ~a" module)
  (if hidingp
    (write-string " hiding"))
  (module-names suppliedp names))

(defmacro defimport (name qualifiedp)
  `(defkeyword ,name (module &optional (names nil suppliedp) hidingp)
     `(%import ',module ,suppliedp ',names ',',qualifiedp ',hidingp)))

(defimport |import| nil)
(defimport |require| t)

(defhasq :|all| "(..)")


(defsyntax => (constraints type)
  (if (consp (car constraints))
    (with-paren
      (arrange constraints))
    (rechask constraints))
  (write-string " => ")
  (haskell type))


(defun %deftype (name type)
  (write-string "type ")
  (rechask name)
  (write-string " = ")
  (haskell type))

(defkeyword |deftype| (name type)
  `(%deftype ',name ',type))


(defun %data-body (body)
  (cond
    ((atom body) (haskell body))
    ((and (consp (cdr body))
          (eq (cadr body) :|name|))
     (haskell (car body))
     (write-string " { ")
     (%rechask (cddr body) (curry #'apply #'%type) ", ")
     (write-string " }"))
    (t (rechask body))))

(defun %data (name body deriving)
  (write-string "data ")
  (rechask name)
  (write-string " = ")
  (if (and (consp body)
           (eq (car body) '|or|))
    (%rechask (cdr body) #'%data-body " | ")
    (%data-body body))
  (when deriving
    (write-string " deriving ")
    (with-paren
      (arrange deriving))))

(defkeyword |data| (name body &optional deriving)
  `(%data ',name ',body ',deriving))


(defsyntax |if| (x y z)
  (with-paren
    (write-string "if ")
    (haskell x)
    (write-string " then ")
    (haskell y)
    (write-string " else ")
    (haskell z)))

(def-syntax-macro |cond| (x &rest xs)
  (if xs
    `(|if| ,@x (|cond| ,@xs))
    (second x)))


(defsyntax |case| (x &rest xs)
  (write-string "case ")
  (haskell x)
  (write-string " of")
  (with-indent 1
    (map-indent #'(lambda (x y)
                    (%define x y " -> "))
                xs)))


(defkeyword |extension| (&rest args)
  `(with-pragma
     (format t "LANGUAGE ~{~a~^, ~}" ',args)))


(defhasq :|as| "@")


(def-hs-macro |defconstant| (name expr)
  `(defhasq ,name (const (strhask ',expr))))

;; Local Variables:
;; eval: (cl-indent-rules (quote (&body)) (quote with-paren) (quote with-pragma))
;; End:
