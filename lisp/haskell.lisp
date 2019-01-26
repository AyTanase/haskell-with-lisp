(in-package :cl-user)

(defpackage :haskell
  (:nicknames :hs)
  (:use :cl)
  (:shadow :compile)
  (:export :compile :compile-all :lazy-compile))

(in-package :haskell)

(defun true-path (path)
  (merge-pathnames path *load-truename*))

(defun load-relative (file)
  (load (true-path file)))

(load-relative "util.lisp")
(load-relative "syntax.lisp")

(provide :haskell)
