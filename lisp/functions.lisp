(in-package :hs)


(defhasq |compose| "(.)")
(defhasq |nil| "[]")
(defhasq 1+ "((+) 1)")
(defhasq 1- "((+) (negate 1))")
(defhasq = "(==)")
(defhasq |cons| "(:)")
(defhasq |pair| "(,)")

(defpattern |list*| (&rest args)
  (with-paren (rechask args ":")))

(defpattern |list| (&rest args)
  (format t "[")
  (rechask args ", ")
  (format t "]"))


(defmacro def-binop-as (name op &key zero one many)
  (let ((printed (format nil "(~a)" op)))
    `(progn
       (defsyntax ,name (&rest args)
         (cond
           ((null args)
            ,(or zero `(format t ,printed)))
           ((null (cdr args))
            ,(or one `(with-paren (rechask (cons ',name args)))))
           (t ,(or many `(with-paren (rechask args ,(format nil " ~a " op)))))))
       (defhasq ,name ,printed))))

(defmacro defbinop (op &rest args) `(def-binop-as ,op ,op ,@args))

(defbinop ->)
(defbinop + :one (haskell (car args)))
(defbinop - :one (with-paren (format t "negate ") (haskell (car args))))


(defmacro shadow-binop (name op &rest args)
  `(progn
     (def-binop-as ,name ,op ,@args)
     (shadow-haskell ',name)))

(shadow-binop |and| &&)
(shadow-binop |or| "||")
(shadow-binop |concat| ++)
