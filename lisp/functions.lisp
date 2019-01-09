(in-package :hs)

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

(defbinop -> :zero "()")
(defbinop + :zero "0")
(defbinop - :one (with-paren (format t "negate ") (haskell (car args))))
(defbinop * :zero "1")
(defbinop / :one (with-paren (format t "recip ") (haskell (car args))))

(def-binop-as |and| && :zero "True")
(def-binop-as |or| "||" :zero "False")
(def-binop-as |append| ++ :zero "[]")
(def-binop-as |compose| |.| :zero "id")


(defsyntax function (x)
  (if (atom x)
    (haskell x)
    (with-paren
      (haskell (car x))
      (format t " ")
      (haskell x))))

(defhasq = "(==)")

#!(defconstant 1+ #'(+ 1))
(defsyntax 1+ (x)
  (with-paren
    (haskell x)
    (format t " + 1")))

#!(defconstant 1- #'(+ (- 1)))
(defsyntax 1- (x)
  (with-paren
    (haskell x)
    (format t " - 1")))

(defhasq |pair| "(,)")

(defhasq |nil| "[]")
(defhasq |cons| "(:)")

(defpattern |list*| (&rest args)
  (with-paren (rechask args ":")))

(defpattern |list| (&rest args)
  (format t "[")
  (rechask args ", ")
  (format t "]"))

;; Local Variables:
;; eval: (add-cl-indent-rule (quote with-paren) (quote (&body)))
;; End:
