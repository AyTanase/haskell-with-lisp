(in-package :hs)

(defmacro def-binop-as (name op &key zero one many)
  (let ((printed (format nil "(~a)" op)))
    `(progn
       (defsyntax ,name (&rest args)
         (cond
           ((null args)
             (write-string ,(or zero printed)))
           ((null (cdr args))
             ,(or one '(haskell-top (car args))))
           (t ,(or many `(rechask args ,(format nil " ~a " op))))))
       (defhasq ,name ,printed))))

(defmacro defbinop (op &rest args)
  `(def-binop-as ,op ,op ,@args))

(defbinop + :zero "0")
(defbinop - :one (haskell-top `(|negate| ,(car args))))
(defbinop * :zero "1")
(defbinop / :one (haskell-top `(|recip| ,(car args))))

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

(defmacro defoperator (name &key (op name) one many)
  `(def-binop-as ,name ,op
     :one ,(or one '(call-nextmethod))
     :many (if (cddr args)
             ,(or many '(call-next-method))
             (apply #'print-infix ',op args))))

(defmacro defrelation (name &rest args)
  `(defoperator ,name :one (write-string "True") ,@args))


(defun expand-= (args)
  (let ((v (genvar)))
    (flet ((expand-1 (w) `(= ,w ,v)))
      `#!(let #?((,v ,(car args)))
           (and ,@#?(mapcar #'expand-1 (cdr args)))))))

(defrelation = :op ==
  :many (haskell-top (expand-= args)))

(defun expand-/= (args)
  (let ((vs (genvars (length args))))
    (flet ((expand-1 (v)
             (loop for w in vs
               until (eq v w)
               collect `(/= ,w ,v))))
    `#!(let ,#?(mapcar #'list vs args)
         (and ,@#?(mapcan #'expand-1 vs))))))

(defrelation /= :many (haskell-top (expand-/= args)))

(defun expand-ord-op (op args)
  (let ((var (genvar)))
    `#!(let #?((,var ,(cadr args)))
         (and #?(,op ,(car args) ,var)
              #?(,op ,var ,@(cddr args))))))

(defmacro def-ord-op (op)
  `(defrelation ,op
     :many (haskell-top (expand-ord-op ',op args))))

(def-ord-op <=)
(def-ord-op >=)
(def-ord-op <)
(def-ord-op >)



#!(defconstant 1+ (curry + 1))
(def-syntax-macro 1+ (x) `(+ ,x 1))

#!(defconstant 1- (curry + -1))
(def-syntax-macro 1- (x) `(- ,x 1))

(defhasq |pair| "(,)")


(def-binop-as |cons| |:|
  :one (call-next-method)
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
;; eval: (add-cl-indent-rule (quote defrelation) (quote (4 2 2 &body)))
;; eval: (add-cl-indent-rule (quote ds-bind) (quote (&lambda 4 &body)))
;; End:
