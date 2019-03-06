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
  (if (atom names)
    (haskell names)
    (with-paren
      (%map-hs (curry #'%map-hs #'tuple " ")
               ", " names))))

(defun %import (key args)
  (format t "~A " key)
  (haskell-top (car args))
  (loop for (x . xs) on (cdr args)
    do (write-char #\Space)
       (cond
         ((consp xs) (haskell x))
         (t (module-names x)))))

(def-hs-macro |import| (&rest args)
  `(%import "import" ',args))

(def-hs-macro |defmodule| (&rest args)
  `(progn
     (%import "module" ',args)
     (write-string " where")))

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
