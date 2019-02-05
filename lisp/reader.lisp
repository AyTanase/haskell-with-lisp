(in-package :hs)


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


(defvar *cl-readtable* *readtable*
  "for reading Common Lisp parts")

(defvar *hs-readtable* nil
  "for reading Haskell parts")

(defvar *hs-toplevel* nil
  "for preserving the code style")


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

(defun read-hs-string (ins &rest args)
  (declare (ignore args))
  (with-output-to-string (outs)
    (flet ((read-1 ()
             (read-not-cr ins))
           (write-1 (char)
             (write-char char outs)))
      (write-1 #\")
      (loop
        (let ((char (read-1)))
          (write-1 char)
          (case char
            (#\\ (write-1 (read-1)))
            (#\" (return))))))))

(let ((*readtable* *hs-readtable*))
  (cl-macro-char #\# #\?)
  (set-macro-char #\' (get-macro-char #\') t)
  (set-macro-char #\" #'read-hs-string))


(setf *hs-toplevel* (copy-readtable *hs-readtable*))

(declaim (ftype function read-hs-comment-1))
(defun parse-hs-comment (c ins outs)
  (flet ((read-1 ()
           (read-not-cr ins))
         (write-1 (char)
           (write-char char outs))
         (recurse (char)
           (parse-hs-comment char ins outs)))
    (case c
      (#\# (let ((d (read-1)))
             (cond
               ((char= d #\{)
                 (read-hs-comment-1 ins outs)
                 (recurse (read-1)))
               (t (write-1 c)
                  (recurse d)))))
      (#\} (let ((d (read-1)))
             (unless (char= d #\#)
               (write-1 c)
               (recurse d))))
      (t (write-1 c)
         (recurse (read-1))))))

(defun read-hs-comment-1 (ins outs)
  (write-string "{-" outs)
  (parse-hs-comment (read-not-cr ins) ins outs)
  (write-string "-}" outs))

(defun read-hs-comment (ins &rest args)
  (declare (ignore args))
  `(progn
     (write-string
      ,(with-output-to-string (outs)
         (read-hs-comment-1 ins outs)))
     (fresh-line)))

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
  (set-macro-char #\# #\{ #'read-hs-comment)
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

