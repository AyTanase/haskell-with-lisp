(in-package :hs)


(defmacro with-gensyms (args &body body)
  `(let ,(loop for x in args
           collect `(,x (gensym)))
     ,@body))

(defun curry (f &rest xs)
  #'(lambda (&rest ys) (apply f (append xs ys))))

(define-compiler-macro curry (f &rest xs)
  (with-gensyms (ys)
    `#'(lambda (&rest ,ys) (apply ,f ,@xs ,ys))))

(declaim (inline compose))
(defun compose (f g)
  #'(lambda (x) (funcall f (funcall g x))))

(declaim (inline %partition partition))

(defun %partition (test xs)
  (loop for x in xs
    if (funcall test x) collect x into ys
    else collect x into ns
    finally (return (values ys ns))))

(defun partition (test xs &key (key #'identity))
  (%partition (compose test key) xs))

(declaim (inline format-symbol))
(defun format-symbol (&rest args)
  (intern (apply #'format nil args)))

(let ((i 0))
  (defun genvar ()
    (format-symbol "_v~d" (incf i))))

(defun genvars (n)
  (loop repeat n collect (genvar)))

(defun map-tree (fn tree)
  (if (atom tree)
    (funcall fn tree)
    (mapcar (curry #'map-tree fn) tree)))

(declaim (inline callp))
(defun callp (expr symbol)
  (and (consp expr) (eq (car expr) symbol)))

(defmacro ds-bind (&body body)
  `(destructuring-bind ,@body))

(defmacro mv-bind (&body body)
  `(multiple-value-bind ,@body))
