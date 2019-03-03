(in-package :asdf-user)

(defsystem "haskell-with-lisp"
  :author "Ayumu Tanase"
  :licence "MIT"
  :description "Common Lisp macros for generating Haskell code"
  :depends-on (:alexandria)
  :components ((:module "lisp"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "reader")
                             (:file "syntax")
                             (:file "define")
                             (:file "macros")
                             (:file "specials")
                             (:file "cl-keywords")
                             (:file "functions")))))
