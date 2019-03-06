(in-package :hs)

(defmacro definline (name &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,@body)))

(defmacro defaccessor (name args expr)
  (with-gensyms (value)
    `(progn
       (defun ,name ,args ,expr)
       (defun (setf ,name) (,value ,@args)
         (setf ,expr ,value)))))


(definline get-truename ()
  (or *compile-file-truename*
      *load-truename*
      *default-pathname-defaults*))

(definline true-path (path)
  (merge-pathnames path (get-truename)))


(definline genvar (&optional (prefix "v"))
  (gentemp prefix))

(defun make-genvar-list (length)
  (loop repeat length collect (genvar)))


(definline call-when (test fn item)
  (if (funcall test item)
    (funcall fn item)
    (values nil t)))

(definline call-car (fn item)
  (call-when #'consp (compose fn #'car) item))

(definline call-cdr (fn item)
  (call-when #'consp (compose fn #'cdr) item))

(defmacro with-car ((var item) &body body)
  `(call-car #'(lambda (,var) ,@body) ,item))

(defmacro with-cdr ((var item) &body body)
  `(call-cdr #'(lambda (,var) ,@body) ,item))

(definline callp (expr symbol)
  (call-car (curry #'eq symbol) expr))


(defmacro ds-bind (&body body)
  `(destructuring-bind ,@body))

(defmacro mv-bind (&body body)
  `(multiple-value-bind ,@body))


(defun subst-wild-cards (args)
  (let ((vars nil))
    (labels ((subst-1 (x)
               (cond
                 ((eq x '_)
                   (car (push (gensym) vars)))
                 ((or (atom x) (eq (car x) 'quote))
                   x)
                 (t (mapcar #'subst-1 x)))))
      (let ((new-args (subst-1 args)))
        (values new-args vars)))))

(defun %def*/i (macro name args &rest body)
  (mv-bind (new-args vars) (subst-wild-cards args)
    `(,macro ,name ,new-args
             (declare (ignore ,@vars))
             ,@body)))

(defmacro defun/i (&rest args)
  (apply #'%def*/i 'defun args))

(defmacro defmethod/i (&rest args)
  (apply #'%def*/i 'defmethod args))


(defun collect-decls (body)
  (mv-bind (rest decls doc)
      (parse-body body :documentation t)
    (values (if doc (cons doc decls) decls)
            rest)))

(defmacro defmapc (name fn)
  (with-gensyms (args)
    `(defun ,name (&rest ,args) (mapc ,fn ,args))))
