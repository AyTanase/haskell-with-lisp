(in-package :cl-user)

(defpackage :haskell
  (:nicknames :hs)
  (:documentation
   "macros and keywords for generating Haskell code")
  (:use :cl :alexandria)
  (:shadow :compile)
  (:export :compile :compile-all :lazy-compile))
