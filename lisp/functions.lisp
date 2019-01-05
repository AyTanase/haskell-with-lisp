(in-package :hs)


(defmacro defhasq (name body)
  `(defmethod haskell ((x (eql ',name))) (format t ,body)))

(defhasq |compose| "(.)")
(defhasq |nil| "[]")
(defhasq 1+ "((+) 1)")
(defhasq 1- "((+) (negate 1))")
(defhasq = "(==)")
(defhasq |cons| "(:)")
(defhasq |pair| "(,)")

(defsyntax |list*| (&rest args)
  (with-paren (rechask args ":")))

(defsyntax |list| (&rest args)
  (format t "[")
  (rechask args ", ")
  (format t "]"))


(defmacro defbinop (op &key zero one many)
  (let ((printed (format nil "(~a)" op)))
    `(progn
       (defsyntax ,op (&rest args)
         (cond
           ((null args)
            ,(or zero `(format t ,printed)))
           ((null (cdr args))
            ,(or one `(with-paren (rechask `(,',op ,@args)))))
           (t ,(or many `(with-paren (rechask args ,(format nil " ~a " op)))))))
       (defhasq ,op ,printed))))

(defbinop ->)
(defbinop + :one (haskell (car args)))
(defbinop - :one (with-paren (format t "negate ") (haskell (car args))))
