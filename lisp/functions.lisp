(in-package :hs)

(defun %op-print-1 (expr)
  (if (atom expr)
    (%haskell expr)
    (let ((spec (car expr)))
      (cond
        ((eq spec '|funcall|)
          (rechask (cdr expr)))
        ((keytypep spec 'operator)
          (%haskell expr))
        (t (%haskell-top expr))))))

(declaim (inline op-print-1))
(defun op-print-1 (expr)
  (%op-print-1 (hs-macro-expand expr)))

(defrechask rec-op-1 #'op-print-1 "")

(defun print-infix (op x y)
  (op-print-1 x)
  (format t " ~a " op)
  (op-print-1 y))


(defmacro def-op-macro
    (name &key (op name) (zero `',name) (one '`(|curry| ,@expr)) (many 'expr))
  (with-gensyms (spec)
    `(progn
       (setf (gethash ',name *specials*) 'operator)
       (defmethod apply-macro ((,spec (eql ',name)) expr)
         (declare (ignore ,spec))
         (let ((args (cdr expr)))
           (cond
             ((atom args) ,zero)
             ((atom (cdr args)) ,one)
             (t ,many))))
       (defhasq ,name ,(format nil "(~a)" op)))))


(defmacro defoperator (name &optional (op name))
  `(progn
     (def-op-macro ,name :op ,op)
     (defsyntax ,name (x y &rest rest)
       (if rest
         (call-next-method)
         (print-infix ',op x y)))))


(defmacro defbinop
    (name &key (op name) (zero `',name) one many)
  `(progn
     (def-op-macro ,name :op ,op
       :zero ,zero
       :one ,(or one '(hs-macro-expand (car args))))
     (defsyntax ,name (&rest args)
       ,(or many `(rec-op-1 args ,(format nil " ~a " op))))))

(defbinop + :zero 0)
(defbinop - :one `(|negate| ,(car args)))
(defbinop * :zero 1)
(defbinop / :one `(|recip| ,(car args)))

(defbinop |and|     :op &&   :zero '|True|)
(defbinop |or|      :op "||" :zero '|False|)
(defbinop |append|  :op ++   :zero '|nil|)
(defbinop |compose| :op |.|  :zero '|id|)


(defun ->-print-1 (x)
  (let ((expr (hs-macro-expand x)))
    (if (callp expr '->)
      (%haskell expr)
      (%haskell-top expr))))

(defbinop -> :many (%rechask args #'->-print-1 " -> "))


(defsynonym let |let|)
(defsynonym and |and|)

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
      `(let ((,v ,(car args)))
         (and ,@(mapcar #'expand-1 (cdr args)))))))

(defrelation = (expand-= args) ==)

(defun expand-/= (args)
  (let ((vs (genvars (length args))))
    (flet ((expand-1 (v)
             (loop for w in vs
               until (eq v w)
               collect `(/= ,w ,v))))
      `(let ,(mapcar #'list vs args)
         (and ,@(mapcan #'expand-1 vs))))))

(defrelation /= (expand-/= args))

(defun expand-ord-op (op args)
  (let ((var (genvar)))
    `(let ((,var ,(cadr args)))
       (and (,op ,(car args) ,var)
            (,op ,var ,@(cddr args))))))

(defmacro def-ord-op (op)
  `(defrelation ,op (expand-ord-op ',op args)))

(def-ord-op <=)
(def-ord-op >=)
(def-ord-op <)
(def-ord-op >)



#!(define-symbol-macro 1+ (funcall + 1))
(def-syntax-macro 1+ (x) `(+ ,x 1))

#!(define-symbol-macro 1- (funcall + -1))
(def-syntax-macro 1- (x) `(- ,x 1))

(defhasq |pair| "(,)")


(declaim (inline simplep))
(defun simplep (expr)
  (or (atom expr)
      (keytypep (car expr) 'pattern)))

(declaim (ftype function %funcall))
(defun funcall-last (expr)
  (if (eq (car expr) '|funcall|)
    (let ((args (mapcar #'%define-expand (cdr expr))))
      (ds-bind (f x &rest xs) args
        (cond
          ((or xs (simplep x))
            (write-string "$ ")
            (%funcall args))
          (t (write-string ". ")
             (%op-print-1 f)
             (write-string " ")
             (funcall-last x)))))
    (progn
      (write-string "$ ")
      (%haskell-top expr))))

(defun funcall-many (args)
  (%haskell (car args))
  (loop
    for xs on (cdr args)
    for x = (car xs)
    do (write-string " ")
       (cond
         ((simplep x) (%haskell x))
         ((atom (cdr xs))
           (funcall-last x))
         (t (return (rec%hask xs))))))

(defun %funcall (args)
  (ds-bind (x y &rest rest) args
    (if (and (atom rest)
             (if (consp x)
               (keytypep (car x) 'special)
               (simplep y)))
      (rec%hask args)
      (funcall-many args))))

(defbinop |funcall| :op $
  :many (%funcall (mapcar #'%define-expand args)))


(defhasq |nil| "[]")

(def-op-macro |cons| :op |:|)
(defsyntax |cons| (x &optional (y nil svar))
  (if svar
    (progn
      (op-print-1 x)
      (if (and (atom x) (atom y))
        (write-string ":")
        (write-string " : "))
      (op-print-1 y))
    (call-next-method)))


(defbinop |list*| :op |:|
  :many (if (find-if #'consp args)
          (rec-op-1 args " : ")
          (rec-op-1 args ":")))


(defun read-at-mark (stream &rest args)
  (declare (ignore args))
  `(destruct ,(read stream t nil t)))

(set-macro-char #\@ #'read-at-mark t *hs-readtable*)

(def-syntax-macro destruct (expr)
  (cond
    ((not (listp expr)) expr)
    ((last expr 0)
      `(|list*|
        ,@(loop for (x . xs) on expr
            nconc (if (consp xs)
                    (list x)
                    (list x xs)))))
    (t `(|list| ,@expr))))

;; Local Variables:
;; eval: (add-cl-indent-rule (quote ds-bind) (quote (&lambda 4 &body)))
;; eval: (cl-indent-rules (quote (4 2 2 &body)) (quote def-op-macro) (quote defbinop))
;; End:
