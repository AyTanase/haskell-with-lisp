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

(defmacro const (x)
  `(load-time-value ,x t))

(declaim (inline filter))
(defun filter (item sequence &key (test #'eql) (key #'identity))
  (remove-if (complement (curry test item)) sequence :key key))

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
    (cons (map-tree fn (car tree))
          (map-tree fn (cdr tree)))))


(defvar *cl-readtable* *readtable*)
(defvar *hs-readtable*)

(defmacro read-by (readtable)
  (with-gensyms (stream args)
    `#'(lambda (,stream &rest ,args)
         (declare (ignore ,args))
         (let ((*readtable* ,readtable))
           (read ,stream t nil t)))))

(defun read-hs-string (stream &rest args)
  (declare (ignore args))
  (labels ((read-1 ()
             (let ((c (read-char stream t nil t)))
               (if (char= c #\Return) (read-1) c)))
           (recread ()
             (let ((c (read-1)))
               (write-char c)
               (unless (char= c #\")
                 (if (char= c #\\)
                   (write-char (read-1)))
                 (recread)))))
    (with-output-to-string (*standard-output*)
      (write-char #\")
      (recread))))

(set-dispatch-macro-character #\# #\? (read-by *cl-readtable*))
(set-dispatch-macro-character #\# #\! (read-by *hs-readtable*))

(setf *hs-readtable* (copy-readtable *cl-readtable*))

(let ((*readtable* *hs-readtable*))
  (set-macro-character #\' (get-macro-character #\') t)
  (set-macro-character #\" #'read-hs-string))

(setf (readtable-case *hs-readtable*) :preserve)


(defun default-out (src)
  (make-pathname :type "hs" :defaults src))

(defun compile (src &optional (out (default-out src)))
  (let ((*package* (find-package :hs))
        (*readtable* *hs-readtable*)
        (*print-right-margin* most-positive-fixnum))
    (with-open-file (*standard-output* out
                     :direction :output
                     :if-exists :supersede)
      (load src))))

(defun compile-all (&rest args)
  (mapc #'compile args))

(defun lazy-compile (src &optional (out (default-out src)))
  (if (or (not (probe-file out))
          (< (file-write-date out)
             (file-write-date src)))
    (compile src out)))
