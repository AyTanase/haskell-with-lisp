(in-package :hs)


(defun %type (vars type)
  (rechask vars ", ")
  (format t " :: ")
  (haskell type))

(defmacro |type| (vars type)
  `(progn (%type ',vars ',type) (fresh-line)))

(defsyntax |type| (var type)
  (with-paren (%type var type)))


(defun tuple (xs)
  (with-paren (rechask xs ", ")))

(defsyntax |tuple| (&rest xs) (tuple xs))


(defun %define-left (var)
  (if (or (atom var) (gethash (car var) *syntax*))
    (haskell var)
    (rechask var)))

(defun %define (var val)
  (%define-left var)
  (format t " = ")
  (haskell val))

(defmacro |define| (var val)
  `(progn (%define ',var ',val) (fresh-line)))

(defun local-definitions (defs)
  (loop for def in defs
    do (apply #'%define def)
       (format t "; ")))

(defsyntax |where| (defs val)
  (haskell val)
  (format t " where { ")
  (local-definitions defs)
  (format t "}"))

(defsyntax |let| (defs val)
  (format t "let { ")
  (local-definitions defs)
  (format t "} in ")
  (haskell val))


(defun %class-declare (fn lst)
  (dolist (args lst)
    (format t "~%  ")
    (apply fn args)))

(defun %class (key name derive svar decs defs)
  (format t "~a " key)
  (when derive
    (with-paren (arrange derive))
    (format t " => "))
  (rechask name)
  (when svar
    (format t " where")
    (%class-declare #'%type decs)
    (%class-declare #'%define defs))
  (fresh-line))

(defmacro |class| (name &optional derive (decs nil svar) defs)
  `(%class '|class| ',name ',derive ,svar ',decs ',defs))

(defmacro |instance| (name &optional derive (defs nil svar))
  `(%class '|instance| ',name ',derive ,svar nil ',defs))


(defun module-names (suppliedp names)
  (when suppliedp
    (format t " ")
    (with-paren (arrange names))))

(defun %defmodule (module suppliedp names)
  (format t "module ~a" module)
  (module-names suppliedp names)
  (format t " where")
  (fresh-line))

(defmacro |defmodule| (module &optional (names nil suppliedp))
  `(%defmodule ',module ,suppliedp ',names))

(defun %import (module suppliedp names qualifiedp hidingp)
  (format t "import")
  (if qualifiedp (format t " qualified"))
  (format t " ~a" module)
  (if hidingp (format t " hiding"))
  (module-names suppliedp names)
  (fresh-line))

(defmacro defimport (name qualifiedp)
  `(defmacro ,name (module &optional (names nil suppliedp) hidingp)
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
  (haskell type)
  (fresh-line))

(defmacro |deftype| (name type) `(%deftype ',name ',type))


(defun %data (name body deriving)
  (format t "data ")
  (rechask name)
  (format t " = ")
  (if (and (consp body) (eq (car body) '|or|))
    (arrange (cdr body) " | ")
    (rechask body))
  (when deriving
    (format t " deriving ")
    (with-paren (arrange deriving)))
  (fresh-line))

(defmacro |data| (name body &optional deriving)
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


(defmacro |extension| (&rest args)
  `(progn (format t "{-# LANGUAGE ~{~a~^, ~} #-}" ',args)
          (fresh-line)))


;; Local Variables:
;; eval: (add-cl-indent-rule (quote with-paren) (quote (&body)))
;; End:
