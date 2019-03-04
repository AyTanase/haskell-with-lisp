(in-package :cl-user)

(defpackage :haskell
  (:nicknames :hs)
  (:documentation
   "macros and keywords for generating Haskell code")
  (:use :cl :alexandria)
  (:shadow :compile)
  (:export
   ;; reader.lisp
   :*cl-readtable* :*hs-readtable* :*hs-toplevel*
   :compile :compile-all :lazy-compile
   ;; syntax.lisp
   :def-hs-macro
   :defparen :with-paren :with-square-brackets
   :indent :indent-if :with-indent :map-indent
   :shadow-haskell :defshadow
   :def-syntax-macro :|defsynonym|
   :|define-symbol-macro|
   :hs-macro-expand
   :haskell :haskells
   :%map-hs :def-map-hs :map-hs
   :defsyntax
   :haskell-top :haskell-tops :arrange
   :defspecial :defpattern :keytypep :may-op
   :defhasq
   ;; define.lisp
   :|type|
   :%define-expand :%define-print
   :%define-right
   :%define-left
   :%define :|define|
   ;; macros.lisp
   :tuple :|tuple|
   :=>
   :|class| :|instance|
   :|defmodule| :|import|
   :|deftype|
   :|data| :|newtype|
   :|extension|
   ;; specials.lisp
   :|let| :|where|
   :|if|
   :|case|
   :|bind| :|do|
   :|lambda|
   :|as|
   :|list| :|enum-from|
   ;; cl-keywords
   :|cond|
   :|defun| :|labels|
   :|defpackage|
   ;; functions.lisp
   :op-print-1 :map-op-1 :print-infix
   :def-op-macro
   :defoperator
   :defbinop
   :|and| :|or|
   :|append|
   :|compose|
   :->
   :|pair|
   :simplep
   :|funcall|
   :|flip|
   :|nil| :|cons| :|list*|))

(defpackage :haskell-user
  (:nicknames :hs-user)
  (:use :cl :hs)
  (:shadowing-import-from :hs :compile))
