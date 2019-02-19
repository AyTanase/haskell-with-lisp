(in-package :hs)


(defmacro with-gensyms (args &body body)
  `(let ,(loop for x in args
           collect `(,x (gensym)))
     ,@body))

(defmacro definline (name &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,@body)))


(defun curry (f &rest xs)
  #'(lambda (&rest ys) (apply f (append xs ys))))

(define-compiler-macro curry (f &rest xs)
  (with-gensyms (ys)
    `#'(lambda (&rest ,ys) (apply ,f ,@xs ,ys))))

(definline compose (f g)
  #'(lambda (x) (funcall f (funcall g x))))


(definline %partition (test xs)
  (loop for x in xs
    if (funcall test x) collect x into ys
    else collect x into ns
    finally (return (values ys ns))))

(definline partition (test xs &key (key #'identity))
  (%partition (compose test key) xs))


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
               (if (atom x)
                 (subst-atom x)
                 (subst-cons x)))
             (subst-atom (x)
               (if (eq x '_)
                 (let ((var (gensym)))
                   (push var vars)
                   var)
                 x))
             (subst-cons (x)
               (if (eq (car x) 'quote)
                 x
                 (mapcar #'subst-1 x))))
      (declare (inline subst-atom subst-cons))
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

;; Local Variables:
;; eval: (add-cl-indent-rule (quote mv-bind) (quote (&lambda 4 &body)))
;; End:
