(in-package :hs)


(defun tuple (xs)
  (if (listp xs)
    (with-paren (arrange xs))
    (haskell-top xs)))

(defpattern |tuple| (&rest xs)
  (tuple xs))


(defun =>-left (args)
  (when (consp args)
    (cond
      ((atom (car args))
        (haskell-top args))
      ((atom (cdr args))
        (haskell-top (car args)))
      (t (tuple args)))
    (write-string " => ")))

(defsyntax => (args type)
  (=>-left args)
  (haskell-top type))


(defun %class-derive (args)
  (when (consp args)
    (ds-bind (key . rest) args
      (cond
        ((keywordp key)
          (format t "{-# ~@:(~A~) #-} " key)
          (=>-left rest))
        (t (=>-left args))))))

(defun %class (key name derive defs)
  (format t "~A " key)
  (%class-derive derive)
  (haskell-top name)
  (when defs
    (write-string " where")
    (with-indent 1
      (map-indent #'%define defs))))

(defmacro def-class-macro (key)
  `(def-hs-macro ,key (name &optional derive &body defs)
     `(%class ',',key ',name ',derive ',defs)))

(def-class-macro |class|)
(def-class-macro |instance|)


(defun module-names (names)
  (write-char #\Space)
  (with-paren
    (%map-hs (curry #'%map-hs #'tuple " ")
             ", " names)))

(defun %defmodule (module svar names)
  (haskell-tops "module " module)
  (when svar
    (module-names names))
  (write-string " where"))

(def-hs-macro |defmodule| (module &optional (names nil svar))
  `(%defmodule ',module ,svar ',names))

(defun %import (module svar names)
  (haskell-tops "import " module)
  (when svar
    (cond
      ((callp names :|hide|)
        (write-string " hiding")
        (module-names (cdr names)))
      (t (module-names names)))))

(def-hs-macro |import| (module &optional (names nil svar))
  `(%import ',module ,svar ',names))

(defhasq :|m| "module")
(defhasq :|q| "qualified")
(defhasq :|all| "(..)")


(def-hs-macro |deftype| (name type)
  `(haskell-tops "type " ',name " = " ',type))


(defun %data-body (body)
  (cond
    ((and (consp body)
          (callp (cdr body) :|name|))
      (haskell-tops (car body) " { ")
      (%map-hs (curry #'apply #'%type)
               ", " (cddr body))
      (write-string " }"))
    (t (haskell-top body))))

(defun %data (key name body deriving)
  (format t "~A " key)
  (haskell-tops name " = ")
  (if (callp body '|or|)
    (%map-hs #'%data-body " | " (cdr body))
    (%data-body body))
  (when deriving
    (write-string " deriving ")
    (tuple deriving)))

(defmacro def-data-macro (key)
  `(def-hs-macro ,key (name constr &optional deriving)
     `(%data ',',key ',name ',constr ',deriving)))

(def-data-macro |data|)
(def-data-macro |newtype|)


(def-hs-macro |extension| (&rest args)
  `(format t "{-# LANGUAGE ~{~A~^, ~} #-}" ',args))
