(in-package :hs)

(defsyntax function (x)
  (if (atom x)
    (haskell x)
    (with-paren (rechask x))))

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
           ((null args) (format t ,(or zero printed)))
           ((null (cdr args))
            ,(or one '(haskell (car args))))
           (t ,(or many `(with-paren (rechask args ,(format nil " ~a " op)))))))
       (defhasq ,name ,printed))))

(defmacro defbinop (op &rest args) `(def-binop-as ,op ,op ,@args))

(defbinop ->)
(defbinop + :zero "0")
(defbinop - :one (with-paren (format t "negate ") (haskell (car args))))
(defbinop * :zero "1")
(defbinop / :one (with-paren (format t "recip ") (haskell (car args))))


(defmacro shadow-binop (name &rest args)
  `(progn
     (def-binop-as ,name ,@args)
     (shadow-haskell ',name)))

(shadow-binop |and| && :zero "True")
(shadow-binop |or| "||" :zero "False")
(shadow-binop |append| ++ :zero "[]")
