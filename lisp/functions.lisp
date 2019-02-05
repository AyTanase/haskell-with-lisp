(in-package :hs)

(defmacro def-binop-as (name op &key zero one many)
  (let ((printed (format nil "(~a)" op)))
    `(progn
       (defsyntax ,name (&rest args)
         (cond
           ((null args)
             (write-string ,(or zero printed)))
           ((null (cdr args))
             (haskell-top ,(or one '(car args))))
           (t ,(or many `(rechask args ,(format nil " ~a " op))))))
       (defhasq ,name ,printed))))

(defmacro defbinop (op &rest args)
  `(def-binop-as ,op ,op ,@args))

(defbinop -> :zero "()")
(defbinop + :zero "0")
(defbinop - :one `(|negate| ,(car args)))
(defbinop * :zero "1")
(defbinop / :one `(|recip| ,(car args)))

(def-binop-as |and| && :zero "True")
(def-binop-as |or| "||" :zero "False")
(def-binop-as |append| ++ :zero "[]")
(def-binop-as |compose| |.| :zero "id")


(defun %-> (expr)
  (if (callp expr '->)
    (haskell expr)
    (haskell-top expr)))

(defbinop ->
  :zero "()"
  :many (%rechask args #'%-> " -> "))


(defun print-infix (op x y)
  (haskell x)
  (format t " ~a " op)
  (haskell y))

(defmacro defoperator (name &key (op name) many)
  `(def-binop-as ,name ,op
     :one `#'(,',name ,(car args))
     :many (if (cddr args)
             ,(or many `(rechask `(,',name ,@args)))
             (apply #'print-infix ',op args))))


(defun expand-ord-op (op args)
  (flet ((expand-1 (vs)
           (list op (car vs) (car (or (cdr vs) (last args))))))
    (let ((vs (genvars (- (length args) 2))))
      `#!(let ,#?(mapcar #'list vs (cdr args))
           (and ,@#?(maplist #'expand-1 (cons (car args) vs)))))))

(defmacro def-ord-op (name &optional (op name))
  `(defoperator ,name :op ,op
     :many (haskell-top (expand-ord-op ',name args))))

(def-ord-op = ==)
(def-ord-op <=)
(def-ord-op >=)
(def-ord-op <)
(def-ord-op >)


(defun expand-/= (args)
  (let ((vs (genvars (length args))))
    (flet ((expand-1 (v)
             (loop for w in vs
               until (eq v w)
               collect `(/= ,w ,v))))
    `#!(let ,#?(mapcar #'list vs args)
         (and ,@#?(mapcan #'expand-1 vs))))))

(defoperator /= :many (haskell-top (expand-/= args)))


(defsyntax function (x)
  (cond
    ((atom x)
      (haskell-top x))
    ((atom (cdr x))
      (haskell-top (car x)))
    (t (rechask `(,(car x) ,(if (cddr x) x (cadr x)))))))

#!(defconstant 1+ #'(+ 1))
(def-syntax-macro 1+ (x) `(+ ,x 1))

#!(defconstant 1- #'(+ (- 1)))
(def-syntax-macro 1- (x) `(- ,x 1))

(defhasq |pair| "(,)")


(def-binop-as |cons| |:|
  :one `#'(|cons| ,(car args))
  :many (ds-bind (x y) args
          (haskell x)
          (if (and (atom x) (atom y))
            (write-string ":")
            (write-string " : "))
          (haskell y)))

(defsyntax |list*| (&rest args)
  (if (find-if #'consp args)
    (rechask args " : ")
    (rechask args ":")))


(defpattern |list| (&rest args)
  (with-square-brackets
    (arrange args)))

#!(defconstant nil (list))

#!(defconstant list pure)

(defpattern |enum-from| (expr &rest args)
  (with-square-brackets
    (haskell-top expr)
    (loop
      for xs on args
      for x = (car xs)
      until (eq x :|to|)
      do (haskell-tops "," x)
      finally
         (write-string "..")
         (if (consp (cdr xs))
           (haskell-top (cadr xs))))))

;; Local Variables:
;; eval: (cl-indent-rules (quote (&body)) (quote with-paren) (quote with-square-brackets))
;; eval: (add-cl-indent-rule (quote defbinop) (quote (4 &body)))
;; eval: (add-cl-indent-rule (quote defoperator) (quote (4 2 2 &body)))
;; eval: (add-cl-indent-rule (quote ds-bind) (quote (&lambda 4 &body)))
;; End:
