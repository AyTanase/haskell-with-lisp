(in-package :hs)

(defmacro def-binop-as (name op &key zero one many)
  (let ((printed (format nil "(~a)" op)))
    `(progn
       (defsyntax ,name (&rest args)
         (cond
           ((null args) (write-string ,(or zero printed)))
           ((null (cdr args)) (haskell ,(or one '(car args))))
           (t ,(or many `(with-paren
                           (rechask args ,(format nil " ~a " op)))))))
       (defhasq ,name ,printed))))

(defmacro defbinop (op &rest args) `(def-binop-as ,op ,op ,@args))

(defbinop -> :zero "()")
(defbinop + :zero "0")
(defbinop - :one `(|negate| ,(car args)))
(defbinop * :zero "1")
(defbinop / :one `(|recip| ,(car args)))

(def-binop-as |and| && :zero "True")
(def-binop-as |or| "||" :zero "False")
(def-binop-as |append| ++ :zero "[]")
(def-binop-as |compose| |.| :zero "id")


(defun print-infix (op x y)
  (with-paren
    (haskell x)
    (format t " ~a " op)
    (haskell y)))

(defmacro defoperator (name &optional (op name))
  `(def-binop-as ,name ,op
     :one `#'(,',name ,(car args))
     :many (apply #'print-infix ',op args)))


(defun expand-ord-op (op args)
  (let ((vs (genvars (- (length args) 2))))
    `#!(let ,#?(mapcar #'list vs (cdr args))
         (and ,@#?(loop for (x . xs) on (cons (car args) vs)
                    collect (list op x (car (or xs (last args)))))))))

(defmacro def-ord-op (name &optional (op name))
  (with-gensyms (g)
    `(progn
       (defsyntax ,g (x y)
         (print-infix ',op x y))
       (def-binop-as ,name ,op
         :one `#'(,',name ,(car args))
         :many (haskell (expand-ord-op ',g args))))))

(def-ord-op = ==)
(def-ord-op <=)
(def-ord-op >=)
(def-ord-op <)
(def-ord-op >)


(let ((g '#:/=))
  (labels ((expand-1 (xs)
             (mapcar (curry #'list g (car xs)) (cdr xs)))
           (expand-/= (args)
             (let ((vs (genvars (length args))))
               `#!(let ,#?(mapcar #'list vs args)
                    (and ,@#?(mapcon #'expand-1 vs))))))
    (eval `(defsyntax ,g (x y)
             (print-infix '/= x y)))
    (defbinop /=
      :one `#'(/= ,(car args))
      :many (haskell (if (cddr args)
                       (expand-/= args)
                       (cons g args))))))


(defsyntax function (x)
  (cond
    ((atom x) (haskell x))
    ((atom (cdr x)) (haskell (car x)))
    (t (with-paren
         (rechask `(,(car x) ,(if (cddr x) x (cadr x))))))))

(defhasq |strict| "($!)")

#!(defconstant 1+ #'(+ 1))
(def-syntax-macro 1+ (x) `(+ ,x 1))

#!(defconstant 1- #'(+ (- 1)))
(def-syntax-macro 1- (x) `(- ,x 1))

(defhasq |pair| "(,)")


(def-binop-as |cons| |:|
  :one `#'(|cons| ,@args)
  :many (destructuring-bind (x y) args
          (with-paren
            (haskell x)
            (write-string (if (and (atom x) (atom y)) ":" " : "))
            (haskell y))))

(defpattern |list*| (&rest args)
  (with-paren
    (rechask args ":")))


(defpattern |list| (&rest args)
  (with-square-brackets
    (rechask args ", ")))

#!(defconstant nil (list))

#!(defconstant list pure)

(defsyntax |enum-from| (x &rest xs)
  (labels ((rec (xs)
             (cond
               ((atom xs) (write-string ".."))
               ((eq (car xs) :|to|)
                (write-string "..")
                (if (consp (cdr xs))
                  (haskell (cadr xs))))
               (t (haskells "," (car xs))
                  (rec (cdr xs))))))
    (with-square-brackets
      (haskell x)
      (rec xs))))

;; Local Variables:
;; eval: (cl-indent-rules (quote (&body)) (quote with-paren) (quote with-square-brackets))
;; eval: (add-cl-indent-rule (quote defbinop) (quote (2 &body)))
;; End:
