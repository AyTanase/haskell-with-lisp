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


(declaim (inline get-macro-char set-macro-char))

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

(defun convert-cr (stream)
  (if (eql (peek-char nil stream nil nil t)
           #\Newline)
    (read-char stream t nil t)
    #\Newline))

(defun read-not-cr (stream)
  (let ((char (read-char stream t nil t)))
    (if (char= char #\Return)
      (convert-cr stream)
      char)))


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
  (with-output-to-string (s)
    (write-char #\" s)
    (loop
      (let ((char (read-not-cr stream)))
        (write-char char s)
        (case char
          (#\\ (write-char (read-not-cr stream) s))
          (#\" (return)))))))

(defun read-hs-comment-1 (ins outs)
  (flet ((read-1 ()
           (read-not-cr ins))
         (write-1 (char)
           (write-char char outs)))
    (write-string "{-" outs)
    (loop
      (let ((c (read-1)))
        (case c
          (#\\ (write-1 (read-1)))
          (#\# (let ((d (read-1)))
                 (if (char= d #\{)
                   (read-hs-comment-1 ins outs)
                   (progn (write-1 c)
                          (write-1 d)))))
          (#\} (return))
          (t (write-1 c)))))
    (write-string "-}" outs)))

(defun read-hs-comment (ins &rest args)
  (declare (ignore args))
  `(progn
     (write-string
      ,(with-output-to-string (outs)
         (read-hs-comment-1 ins outs)))
     (fresh-line)))

(let ((*readtable* *hs-readtable*))
  (cl-macro-char #\# #\?)
  (set-macro-char #\' (get-macro-char #\') t)
  (set-macro-char #\" #'read-hs-string)
  (set-macro-char #\# #\{ #'read-hs-comment))


(setf *hs-toplevel* (copy-readtable *hs-readtable*))

(defun read-hs-lf (stream &rest args)
  (declare (ignore args))
  (if (case (peek-char nil stream nil nil t)
        ((#\Newline #\Return) t))
    '(terpri)
    (read stream nil nil t)))

(defun read-hs-cr (stream &rest args)
  (declare (ignore args))
  (convert-cr stream)
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
