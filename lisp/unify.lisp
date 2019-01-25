(in-package :hs)

(defun format-symbol (&rest args)
  (intern (apply #'format nil args)))

(defun string-tail (s)
  (coerce (cdr (coerce s 'list)) 'string))

(defun symbol-tail (s)
  (intern (string-tail (string s))))

(defun uvarp (x)
  (and (symbolp x) (char= (char (string x) 0) #\?)))

(defmacro with-unifying (args expr context &body body)
  `(multiple-value-bind ,args (unify ,expr ,context)
     ,@body))

(defun unify (expr context)
  (cond
    ((consp expr)
     (with-unifying (eh ch) (car expr) context
       (with-unifying (et ct) (cdr expr) ch
         (values (cons eh et) ct))))
    ((uvarp expr)
     (let ((vs (assoc expr context :test #'eq)))
       (if vs
         (values (format-symbol "~a~a" (second vs) (incf (third vs)))
                 context)
         (let ((v (symbol-tail expr)))
           (values v (cons (list expr v 0) context))))))
    (t (values expr context))))


(defun %udef-guard (context)
  (loop for (_ v n) in context
    nconc (loop for i from 1 to n
            collect `(= ,v ,(format-symbol "~a~a" v i)))))

(defun %udef-nest-guard (expr val)
  (let ((name (format-symbol "_~a" (car expr))))
    `(|let| ((,name ,val)) ,name)))

(defun %udef-guard-body (expr val)
  (if (has-guard-p val)
    (%udef-nest-guard expr val)
    val))

(defun %udef-body (expr guard val)
  (if guard
    `(|if| (|and| ,@guard)
           ,(%udef-guard-body expr val))
    val))

(defun %udef (var val)
  (with-unifying (expr context) var nil
    (list expr (%udef-body expr (%udef-guard context) val))))

(def-hs-macro |udef| (var val)
  `(|define| ,@(%udef var val)))


(defun %ulet (name defs val)
  (list name (mapcar (const (curry #'apply #'%udef)) defs) val))

(defmacro defulet (name)
  `(def-syntax-macro ,name (defs val)
     (%ulet ',(symbol-tail name) defs val)))

(defulet |ulet|)
(defulet |uwhere|)

;; Local Variables:
;; eval: (add-cl-indent-rule (quote with-unifying) (quote (6 4 4 &body)))
;; End:
