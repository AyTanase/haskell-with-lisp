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
    (mapcar (curry #'map-tree fn) tree)))


(defun get-macro-char (char &rest args)
  (apply (if (char= char #\#)
           #'get-dispatch-macro-character
           #'get-macro-character)
         char args))

(defun set-macro-char (char &rest args)
  (apply (if (char= char #\#)
           #'set-dispatch-macro-character
           #'set-macro-character)
         char args))

(defvar *cl-readtable* *readtable*)
(defvar *hs-readtable*)
(defvar *hs-toplevel*)

(defun read-as-is (stream &rest args)
  (declare (ignore args))
  (read stream t nil t))

(defun make-hs-reader (reader)
  #'(lambda (&rest args)
      (let ((*readtable* *hs-readtable*))
        (apply reader args))))

(set-macro-char #\# #\? #'read-as-is)
(set-macro-char #\# #\! (make-hs-reader #'read-as-is))


(setf *hs-readtable* (copy-readtable))
(setf (readtable-case *hs-readtable*) :preserve)

(defun make-cl-reader (reader)
  #'(lambda (stream &rest args)
      (let ((*readtable* *cl-readtable*))
        (prog1 (apply reader stream args)
          (peek-char t stream nil nil t)))))

(defmacro cl-macro-char (&rest args)
  `(set-macro-char ,@args (make-cl-reader (get-macro-char ,@args))))

(defun read-hs-string (stream &rest args)
  (declare (ignore args))
  (labels ((read-1 ()
             (let ((char (read-char stream t nil t)))
               (if (char= char #\Return) (read-1) char))))
    (with-output-to-string (*standard-output*)
      (write-char #\")
      (loop
        (let ((char (read-1)))
          (write-char char)
          (case char
            (#\\ (write-char (read-1)))
            (#\" (return))))))))

(let ((*readtable* *hs-readtable*))
  (cl-macro-char #\# #\?)
  (set-macro-char #\' (get-macro-char #\') t)
  (set-macro-char #\" #'read-hs-string))


(setf *hs-toplevel* (copy-readtable *hs-readtable*))

(defun read-hs-lf (stream &rest args)
  (declare (ignore args))
  (if (case (peek-char nil stream nil nil t)
        ((#\Newline #\Return) t))
    '(terpri)
    (read stream nil nil t)))

(defun read-hs-cr (stream &rest args)
  (declare (ignore args))
  (if (eql (peek-char nil stream nil nil t)
           #\Newline)
    (read-char stream t nil t))
  (read-hs-lf stream))

(let ((*readtable* *hs-toplevel*))
  (set-macro-char #\Newline #'read-hs-lf)
  (set-macro-char #\Return #'read-hs-cr)
  (set-macro-char #\( (make-hs-reader (get-macro-char #\()))
  (cl-macro-char #\# #\|)
  (cl-macro-char #\;))


(defun default-out (src)
  (make-pathname :type "hs" :defaults src))

(defun compile (src &optional (out (default-out src)))
  (let ((*package* (find-package :hs))
        (*readtable* *hs-toplevel*)
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
