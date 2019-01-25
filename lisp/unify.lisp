(in-package :hs)

(defun format-symbol (&rest args)
  (intern (apply #'format nil args)))

(defun string-tail (s)
  (coerce (cdr (coerce s 'list)) 'string))

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
         (let ((v (intern (string-tail (string expr)))))
           (values v (cons (list expr v 0) context))))))
    (t (values expr context))))

;; Local Variables:
;; eval: (add-cl-indent-rule (quote with-unifying) (quote (6 4 4 &body)))
;; End:
