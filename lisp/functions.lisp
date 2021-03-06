(in-package :hs)

(defun %op-print-1 (expr)
  (if (atom expr)
    (%haskell expr)
    (ds-bind (spec . args) expr
      (cond
        ((eq spec '|funcall|)
          (%map-hs (compose #'%haskell #'%define-expand)
                   " " args))
        ((get-keytype spec)
          (%haskell expr))
        (t (%haskell-top expr))))))

(definline op-print-1 (expr)
  (%op-print-1 (hs-macro-expand expr)))

(def-map-hs map-op-1 #'op-print-1)

(defun print-infix (op x y)
  (op-print-1 x)
  (format t " ~A " op)
  (op-print-1 y))


(defmacro def-op-macro
    (name &key (op name) (zero `',name) (one '`(|funcall| ,@expr)) (many 'expr))
  `(progn
     (setf (get-keytype ',name) :operator)
     (setf (get-operator ',name) ',op)
     (defmethod/i apply-macro ((_ (eql ',name)) expr)
       (let ((args (cdr expr)))
         (cond
           ((atom args) ,zero)
           ((atom (cdr args))
             (hs-macro-expand ,one))
           (t ,many))))))


(defmacro defoperator (name &optional (op name))
  `(progn
     (def-op-macro ,name
       :op ,op
       :many (ds-bind (x y . rest) args
               (if rest
                 `((,',name ,x ,y) ,@rest)
                 expr)))
     (defsyntax ,name (x y)
       (print-infix ',op x y))))


(defmacro defbinop
    (name &key (op name) (zero `',name) (one '(car args)) many)
  `(progn
     (def-op-macro ,name :op ,op :zero ,zero :one ,one)
     (defsyntax ,name (&rest args)
       ,(or many `(map-op-1 args ,(format nil " ~A " op))))))

(defbinop + :zero 0)
(defbinop - :one `(|negate| ,(car args)))
(defbinop * :zero 1)
(defbinop / :one `(|recip| ,(car args)))

(defbinop |and|     :op "&&" :zero '|True|)
(defbinop |or|      :op "||" :zero '|False|)
(defbinop |append|  :op "++" :zero '|nil|)
(defbinop |compose| :op "."  :zero '|id|)

(defbinop -> :many
  (do-haskell (x args " -> ")
    (let ((expr (hs-macro-expand x)))
      (if (callp expr '->)
        (%haskell expr)
        (%haskell-top expr)))))


(defmacro defrelation (name many &optional (op name))
  `(progn
     (def-op-macro ,name :op ,op
       :one '|True|
       :many (if (cddr args)
               (hs-macro-expand ,many)
               expr))
     (defsyntax ,name (x y)
       (print-infix ',op x y))))

(defun expand-= (args)
  (let ((v (genvar)))
    (flet ((expand-1 (w) `(= ,w ,v)))
      `(|let| ((,v ,(car args)))
         (|and| ,@(mapcar #'expand-1 (cdr args)))))))

(defrelation = (expand-= args) ==)

(defun expand-/= (args)
  (let ((vs (make-genvar-list (length args))))
    (flet ((expand-1 (v)
             (loop for w in vs
               until (eq v w)
               collect `(/= ,w ,v))))
      `(|let| ,(mapcar #'list vs args)
         (|and| ,@(mapcan #'expand-1 vs))))))

(defrelation /= (expand-/= args))

(defun expand-ord-op (op args)
  (let ((var (genvar)))
    (ds-bind (x y . ys) args
      `(|let| ((,var ,y))
         (|and| (,op ,x ,var) (,op ,var ,@ys))))))

(defmacro def-ord-op (op)
  `(defrelation ,op (expand-ord-op ',op args)))

(def-ord-op <=)
(def-ord-op >=)
(def-ord-op <)
(def-ord-op >)



#!(define-symbol-macro 1+ (flip + 1))
(def-syntax-macro 1+ (x) `(+ ,x 1))

#!(define-symbol-macro 1- (subtract 1))
(def-syntax-macro 1- (x) `(- ,x 1))

(defhasq |pair| "(,)")


(definline simplep (expr)
  (or (atom expr)
      (keytypep (car expr) :pattern)))

(declaim (ftype function %funcall))
(defun funcall-last (expr)
  (if (eq (car expr) '|funcall|)
    (let ((args (mapcar #'%define-expand (cdr expr))))
      (ds-bind (f x . xs) args
        (cond
          ((or xs (simplep x))
            (write-string " $ ")
            (%funcall args))
          (t (write-string " . ")
             (%op-print-1 f)
             (funcall-last x)))))
    (%haskell-tops " $ " expr)))

(defun %funcall-1 (args paren?)
  (when args
    (ds-bind (x . xs) args
      (flet ((recurse (p)
               (%haskells " " x)
               (%funcall-1 xs p)))
        (cond
          ((simplep x)
            (recurse paren?))
          ((or (consp xs)
               (and paren? (every #'simplep x)))
            (recurse t))
          (t (funcall-last x)))))))

(defun %funcall (args)
  (%haskell (car args))
  (%funcall-1 (cdr args) nil))

(progn
  (def-op-macro |funcall| :op "$"
    :many (if (and (keytypep (car args) :operator)
                   (atom (cddr args)))
            `(#1=#:|funcall| ,@args)
            expr))
  (defpattern #1# (op x)
    (with-paren
      (op-print-1 x)
      (write-char #\Space)
      (princ (get-operator op)))))

(defspecial |funcall| (&rest args)
  (let ((xs (mapcar #'%define-expand args)))
    (ds-bind (x y . ys) xs
      (if (and (call-car (rcurry #'keytypep :operator) x)
               (atom ys))
        (haskell-tops x " $ " y)
        (%funcall xs)))))


(progn
  (defmethod/i apply-macro ((_ (eql '|flip|)) expr)
    (let ((args (cdr expr)))
      (if (ds-bind (x . xs) args
            (and (keytypep x :operator)
                 (call-cdr #'atom xs)))
        `(#1=#:|flip| ,@args)
        expr)))
  (defpattern #1# (op x)
    (with-paren
      (princ (get-operator op))
      (write-char #\Space)
      (op-print-1 x))))


(defhasq |nil| "[]")

(def-op-macro |cons| :op ":")
(defsyntax |cons| (x y)
  (op-print-1 x)
  (if (and (simplep x) (simplep y))
    (write-string ":")
    (write-string " : "))
  (op-print-1 y))

(defbinop |list*| :op ":"
  :many (if (every #'simplep args)
          (map-op-1 args ":")
          (map-op-1 args " : ")))


(defoperator |apply| "<*>")
(defbinop |progn| :op "*>")
(defbinop |prog1| :op "<*")

(defbinop |alt| :op "<|>" :zero '|empty|)

(defbinop >>=)
(defbinop =<<)
(defbinop >>)
