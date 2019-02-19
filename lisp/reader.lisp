(in-package :hs)


(definline get-macro-char (char &rest args)
  (apply (if (char= char #\#)
           #'get-dispatch-macro-character
           #'get-macro-character)
         char args))

(definline set-macro-char (char &rest args)
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


(defun/i read-as-is (stream &rest _)
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

(defun/i read-hs-string (ins &rest _)
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
         (recurse (char)
           (parse-hs-comment char ins outs)))
    (let ((d (read-1)))
      (cond
        ((and (char= c #\#) (char= d #\{))
          (read-hs-comment-1 ins outs)
          (recurse (read-1)))
        ((char/= c #\})
          (write-char c outs)
          (recurse d))))))

(defun read-hs-comment-1 (ins outs)
  (write-string "{-" outs)
  (parse-hs-comment (read-not-cr ins) ins outs)
  (write-string "-}" outs))

(defun/i read-hs-comment (ins &rest _)
  `(write-line
    ,(with-output-to-string (outs)
       (read-hs-comment-1 ins outs))))

(defun/i read-hs-lf (stream &rest _)
  (if (case (peek-char nil stream nil nil t)
        ((#\Newline #\Return) t))
    '(terpri)
    (read stream nil nil t)))

(defun/i read-hs-cr (stream &rest _)
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
        (*readtable* *hs-toplevel*))
    (with-open-file (*standard-output* out
                     :direction :output
                     :if-exists :supersede)
      (load src))))

(defmapc compile-all #'compile)

(defun lazy-compile (src &optional (out (default-out src)))
  (if (or (not (probe-file out))
          (< (file-write-date out)
             (file-write-date src)))
    (compile src out)))
