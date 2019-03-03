(in-package :hs)

(defmacro definline (name &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,@body)))


(definline get-truename ()
  (or *compile-file-truename*
      *load-truename*
      *default-pathname-defaults*))

(definline true-path (path)
  (merge-pathnames path (get-truename)))


(definline genvar () (gentemp "v"))

(defun genvars (n)
  (loop repeat n collect (genvar)))


(definline callp (expr symbol)
  (and (consp expr) (eq (car expr) symbol)))

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


(defmacro defmapc (name fn)
  (with-gensyms (args)
    `(defun ,name (&rest ,args) (mapc ,fn ,args))))

;; Local Variables:
;; eval: (add-cl-indent-rule (quote mv-bind) (quote (&lambda 4 &body)))
;; End:
