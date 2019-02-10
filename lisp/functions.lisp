(in-package :hs)

(defun %op-print-1 (expr)
  (if (atom expr)
    (haskell expr)
    (let ((spec (car expr)))
      (cond
        ((eq spec '|funcall|)
          (rechask (cdr expr)))
        ((keytypep spec 'operator)
          (haskell expr))
        (t (haskell-top expr))))))

(defun op-print-1 (expr)
  (%op-print-1 (hs-macro-expand expr)))

(defun print-infix (op x y)
  (op-print-1 x)
  (format t " ~a " op)
  (op-print-1 y))

(defun %operator (op expr)
  (let ((args (cdr expr)))
    (cond
      ((atom args)
        (format nil "(~a)" op))
      ((and (consp (cdr args))
            (atom (cddr args)))
        (apply #'print-infix op args))
      (t (rechask expr)))))

(defmacro defoperator (name &optional (op name))
  `(progn
     (setf (gethash ',name *specials*) 'operator)
     (defmethod apply-syntax ((spec (eql ',name)) expr)
       (declare (ignore spec))
       (%operator ',op expr))
     (defhasq ,name ,(format nil "(~a)" op))))


(defmacro def-op-macro
    (name &key (op name) (zero `',name) (one 'expr) (many 'expr))
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


(defmacro defbinop
    (name &key (op name) (zero `',name) one many)
  `(progn
     (def-op-macro ,name :op ,op
       :zero ,zero
       :one ,(or one '(hs-macro-expand (car args))))
     (defsyntax ,name (&rest args)
       ,(or many `(%rechask args #'op-print-1 ,(format nil " ~a " op))))))

(defbinop + :zero 0)
(defbinop - :one `(|negate| ,(car args)))
(defbinop * :zero 1)
(defbinop / :one `(|recip| ,(car args)))

(defbinop |and|     :op &&   :zero '|True|)
(defbinop |or|      :op "||" :zero '|False|)
(defbinop |append|  :op ++   :zero '|nil|)


(defun compose-print-1 (x)
  (let ((expr (hs-macro-expand x)))
    (if (callp expr '|compose|)
      (haskell-top expr)
      (%op-print-1 expr))))

(defbinop |compose| :op |.|
  :zero '|id|
  :many (%rechask args #'compose-print-1 " . "))


(defun ->-print-1 (x)
  (let ((expr (hs-macro-expand x)))
    (if (callp expr '->)
      (haskell expr)
      (haskell-top expr))))

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



#!(define-symbol-macro 1+ (curry + 1))
(def-syntax-macro 1+ (x) `(+ ,x 1))

#!(define-symbol-macro 1- (curry + -1))
(def-syntax-macro 1- (x) `(- ,x 1))

(defhasq |pair| "(,)")


(defbinop |funcall| :op $
  :many (ds-bind (x y &rest rest) args
          (if (or rest
                  (if (consp x)
                    (keytypep (car x) 'special)
                    (or (atom y)
                        (keytypep (car y) 'pattern))))
            (rechask args)
            (progn
              (if (callp x '|funcall|)
                (rechask (cdr x))
                (haskell-top x))
              (haskell-tops " $ " y)))))


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
  :many (flet ((call (between)
                 (%rechask args #'op-print-1 between)))
          (if (find-if #'consp args)
            (call " : ")
            (call ":"))))

;; Local Variables:
;; eval: (add-cl-indent-rule (quote ds-bind) (quote (&lambda 4 &body)))
;; eval: (cl-indent-rules (quote (4 2 2 &body)) (quote def-op-macro) (quote defbinop) (quote defrelation))
;; eval: (add-cl-indent-rule (quote with-paren) (quote (&body)))
;; End:
