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
  (haskells " :: " type))

(defkeyword |type| (vars type)
  `(%type ',vars ',type))

(defsyntax |type| (var type)
  (with-paren
    (%type (list var) type)))


(defun tuple (xs)
  (with-paren
    (rechask xs ", ")))

(defpattern |tuple| (&rest xs) (tuple xs))


(declaim (ftype function %define))

(defun %define-left (var)
  (if (or (atom var)
          (patternp (car var)))
    (haskell var)
    (rechask var)))

(defun if->cond (expr)
  (if (and (consp expr)
           (eq (car expr) '|if|))
    (destructuring-bind (x y &optional z) (cdr expr)
      (if z
        `(|cond| (,x ,y) (|otherwise| ,z))
        `(|cond| (,x ,y))))
    expr))

(defun truep (x)
  (case x ((|True| |otherwise|) t)))

(defun merge-guards (x y)
  (cond
    ((truep x) y)
    ((truep y) x)
    (t `(|and| ,x ,y))))

(defun reduce-cond-1 (f guard value)
  (let ((expr (if->cond value)))
    (if (and (consp expr)
             (eq (car expr) '|cond|))
      (let ((vg (if (or (truep guard)
                        (null (cddr expr)))
                  guard
                  (funcall f guard))))
        (loop for (g v) in (cdr expr)
          nconc (reduce-cond-1 f (merge-guards vg g) v)))
      (list (list guard value)))))

(defun reduce-cond (value)
  (let ((guards nil))
    (flet ((gpush (g)
             (let ((v (genvar)))
               (push (list v g) guards)
               v)))
      (let ((expr (mapcan (curry #'apply #'reduce-cond-1 #'gpush)
                          (cdr value))))
        (values expr (nreverse guards))))))

(defun %define-right (assign value)
  (flet ((print-guard (g v)
           (haskells "| " g assign v)))
    (let ((expr (if->cond value)))
      (if (and (consp expr)
               (eq (car expr) '|cond|))
        (multiple-value-bind (exps gs) (reduce-cond expr)
          (with-indent 1
            (map-indent #'print-guard exps)
            (when gs
              (indent)
              (write-string "where")
              (with-indent 1
                (map-indent #'%define gs)))))
        (haskells assign expr)))))

(defun %define (var val &optional (assign " = "))
  (if (eq var '|type|)
    (%type val assign)
    (progn
      (%define-left var)
      (%define-right assign val))))

(defkeyword |define| (var val)
  `(%define ',var ',val))

(defsyntax |where| (defs val)
  (haskell val)
  (when defs
    (write-string " where")
    (with-indent 1
      (map-indent #'%define defs))))

(defsyntax |let| (defs val)
  (if defs
    (with-indent 1
      (write-string "let")
      (map-indent #'%define defs)
      (indent)
      (haskells "in " val))
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
  (haskells " => " type))


(defun %deftype (name type)
  (write-string "type ")
  (rechask name)
  (haskells " = " type))

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
    (haskells "if " x " then " y " else " z)))

(def-syntax-macro |cond| (x &rest xs)
  (if xs
    `(|if| ,@x (|cond| ,@xs))
    (second x)))


(defsyntax |case| (x &rest xs)
  (flet ((case-val (x y)
           (%define x y " -> ")))
    (haskells "case " x " of")
    (with-indent 1
      (map-indent #'case-val xs))))


(defkeyword |extension| (&rest args)
  `(with-pragma
     (format t "LANGUAGE ~{~a~^, ~}" ',args)))


(defhasq :|as| "@")


(def-hs-macro |defconstant| (name expr)
  `(defhasq ,name (const (strhask ',expr))))

;; Local Variables:
;; eval: (cl-indent-rules (quote (&body)) (quote with-paren) (quote with-pragma))
;; End:
