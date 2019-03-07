(in-package :cl-user)

(defpackage :hs-utils
  (:export
   ;; utils.lisp
   :definline
   :defaccessor
   :get-truename :true-path
   :call-when
   :call-car :call-cdr
   :callp
   :ds-bind :mv-bind
   :subst-wild-cards
   :%def*/i :defun/i :defmethod/i
   :collect-decls
   :defmapc
   ;; reader.lisp
   :get-macro-char :set-macro-char
   :drop-char :read-not-cr
   :eol-p
   :change-type))

(defpackage :haskell
  (:nicknames :hs)
  (:documentation
   "macros and keywords for generating Haskell code, and utilities to define them")
  (:use :cl :alexandria :hs-utils)
  (:shadow :compile)
  (:export
   ;; utils.lisp
   :genvar :make-genvar-list
   ;; reader.lisp
   :*cl-readtable* :*hs-readtable* :*hs-toplevel*
   :compile :compile-all :lazy-compile
   ;; syntax.lisp
   :def-hs-macro
   :defparen :with-paren :with-square-brackets
   :indent :indent-if :with-indent
   :map-indent :do-indent
   :shadow-haskell :defshadow
   :def-syntax-macro :|defsynonym|
   :|define-symbol-macro|
   :hs-macro-expand
   :haskell :haskells
   :%map-hs :do-haskell
   :def-map-hs :map-hs
   :defsyntax
   :haskell-top :haskell-tops :arrange
   :get-keytype :keytypep
   :defspecial :defpattern
   :get-operator
   :defhasq
   ;; define.lisp
   :|type|
   :%define-expand :%define-print
   :%define-right :|True| :|otherwise|
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
   :def-op-macro :expr :args
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
   :|nil| :|cons| :|list*|
   :|apply| :|progn| :|prog1|
   :|alt|
   :>>= :=<< :>>))

(defpackage :haskell-user
  (:nicknames :hs-user)
  (:use :cl :hs)
  (:shadowing-import-from :hs :compile))
