(in-package :hs)

(defspecial |curry| (&rest args)
  (rechask args))

(defsynonym |setf| |bind|)

(def-syntax-macro |if-bind| (args &rest rest)
  `(|if| (|bind| ,@args) ,@rest))
