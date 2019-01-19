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


(defmacro defoperator (name &optional (op name))
  `(def-binop-as ,name ,op
     :one (haskell `(,,(format nil "(~a)" op) ,@args))
     :many (destructuring-bind (x y) args
             (with-paren
               (haskell x)
               (format t ,(format nil " ~a " op))
               (haskell y)))))

(defoperator = ==)
(defoperator /=)
(defoperator <=)
(defoperator >=)
(defoperator <)
(defoperator >)


(defsyntax function (x)
  (cond
    ((atom x) (haskell x))
    ((atom (cdr x)) (haskell (car x)))
    (t (with-paren
         (haskell (car x))
         (format t " ")
         (haskell (if (atom (cddr x)) (cadr x) x))))))

(defhasq |strict| "($!)")

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

(defhasq |cons| "(:)")

(defpattern |list*| (&rest args)
  (with-paren (rechask args ":")))


(defmacro with-square-brackets (&body body)
  `(progn
     (format t "[")
     ,@body
     (format t "]")))

(defpattern |list| (&rest args)
  (with-square-brackets
    (rechask args ", ")))

#!(defconstant nil (list))

(defsyntax |enum-from| (x &rest xs)
  (labels ((rec (xs)
             (cond
               ((atom xs) (format t ".."))
               ((eq (car xs) :|to|)
                (format t "..")
                (if (consp (cdr xs))
                  (haskell (cadr xs))))
               (t (format t ",")
                  (haskell (car xs))
                  (rec (cdr xs))))))
    (with-square-brackets
      (haskell x)
      (rec xs))))

;; Local Variables:
;; eval: (add-cl-indent-rule (quote with-paren) (quote (&body)))
;; eval: (add-cl-indent-rule (quote with-square-brackets) (quote (&body)))
;; End:
