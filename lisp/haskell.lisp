(in-package :cl-user)

(defpackage :haskell
  (:nicknames :hs)
  (:documentation
   "macros and keywords for generating Haskell code")
  (:use :cl)
  (:shadow :compile)
  (:export :compile :compile-all :lazy-compile :repl))

(in-package :haskell)

(defmacro definline (name &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,@body)))

(definline get-truename ()
  (or *compile-file-truename*
      *load-truename*
      *default-pathname-defaults*))

(definline true-path (path)
  (merge-pathnames path (get-truename)))

(defun load-relative (file) (load (true-path file)))

(load-relative "util.lisp")
(load-relative "reader.lisp")
(load-relative "syntax.lisp")

(provide :haskell)
