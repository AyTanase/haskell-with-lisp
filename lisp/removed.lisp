(in-package :hs-user)

(defmacro defsynonym (&rest args)
  `(|defsynonym| ,@args))

(declaim (inline %rechask))
(defun %rechask (expr fn sep)
  (%map-hs fn sep expr))

(defmacro defrechask (&rest args)
  `(def-map-hs ,@args))

(defrechask rechask #'haskell " ")
(defrechask rec-op-1 #'op-print-1)

(declaim (inline may-op))
(defun may-op (symbol)
  (or (get-operator symbol) symbol))

(shadow "import" :hs-user)
(defmacro |import| (&rest args)
  (let ((last (car (last args))))
    (if (and (consp last)
             (eq (car last) :|hide|))
      `(hs:|import| ,@(butlast args) :|hiding| ,(cdr last))
      `(hs:|import| ,@args))))


(defspecial |curry| (&rest args)
  (rechask args))

#!(defsynonym setf bind)

(def-syntax-macro |if-bind| (args &rest rest)
  `(|if| (|bind| ,@args) ,@rest))

#!(defsynonym apply-left funcall)
#!(defsynonym apl funcall)

#!(defsynonym apply-right flip)
#!(defsynonym apr flip)
