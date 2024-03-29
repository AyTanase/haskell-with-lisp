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


(defun drop-char (char stream)
  (when (eql char (peek-char nil stream nil nil))
    (read-char stream)))

(defun read-not-cr (stream)
  (let ((char (read-char stream t nil t)))
    (cond
      ((char= char #\Return)
        (drop-char #\Newline stream)
        #\Newline)
      (t char))))


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

(defun/i read-as-cl (stream &rest _)
  (let ((*readtable* *cl-readtable*))
    (read stream t nil t)))

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
  (set-macro-char #\# #\? #'read-as-cl)
  (set-macro-char #\' (get-macro-char #\') t)
  (set-macro-char #\" #'read-hs-string))


(setf *hs-toplevel* (copy-readtable *hs-readtable*))

(declaim (ftype function read-hs-comment-1))
(defun parse-hs-comment (char ins outs)
  (flet ((read-1 ()
           (read-not-cr ins))
         (recurse (c)
           (parse-hs-comment c ins outs)))
    (let ((next-char (read-1)))
      (cond
        ((and (char= char #\#)
              (char= next-char #\{))
          (read-hs-comment-1 ins outs)
          (recurse (read-1)))
        ((char/= char #\})
          (write-char char outs)
          (recurse next-char))))))

(defun read-hs-comment-1 (ins outs)
  (write-string "{-" outs)
  (parse-hs-comment (read-not-cr ins) ins outs)
  (write-string "-}" outs))

(defun/i read-hs-comment (ins &rest _)
  `(write-line
    ,(with-output-to-string (outs)
       (read-hs-comment-1 ins outs))))

(definline eol-p (char)
  (or (eql char #\Newline)
      (eql char #\Return)))

(defun/i read-hs-lf (stream &rest _)
  (if (eol-p (peek-char nil stream nil nil t))
    '(terpri)
    (values)))

(defun/i read-hs-cr (stream &rest _)
  (drop-char #\Newline stream)
  (read-hs-lf stream))

(defun make-cl-reader (reader)
  #'(lambda (stream &rest args)
      (let ((*readtable* *cl-readtable*))
        (prog1 (apply reader stream args)
          (peek-char t stream nil nil t)))))

(defun make-cl-comment-reader (reader)
  #'(lambda (stream &rest args)
      (let ((*readtable* *cl-readtable*))
        (apply reader stream args)
        (peek-char t stream nil nil t)
        (values))))

(defmacro lift-cl-comment-reader (&rest args)
  `(set-macro-char ,@args (make-cl-comment-reader (get-macro-char ,@args))))

(let ((*readtable* *hs-toplevel*))
  (set-macro-char #\# #\? (make-cl-reader #'read-as-is))
  (set-macro-char #\# #\{ #'read-hs-comment)
  (set-macro-char #\Newline #'read-hs-lf)
  (set-macro-char #\Return #'read-hs-cr)
  (set-macro-char #\( (make-hs-reader (get-macro-char #\()))
  (lift-cl-comment-reader #\# #\|)
  (lift-cl-comment-reader #\;))


(defparameter *external-format* :utf-8)

(definline change-type (src type)
  (make-pathname :type type :defaults src))

(definline call-compile (fn src out)
  (let ((src-path (true-path src)))
    (funcall fn
             (if (pathname-type src-path)
               src-path
               (change-type src-path "hl"))
             (if out
               (true-path out)
               (change-type src-path "hs")))))

(defun %compile (src out)
  (let ((*package* (find-package :hs-user))
        (*readtable* *hs-toplevel*))
    (with-open-file (*standard-output* out
                     :direction :output
                     :if-exists :supersede
                     :external-format *external-format*)
      (load src :external-format *external-format*))))

(defun compile (src &optional out)
  (call-compile #'%compile src out))

(defmapc compile-all #'compile)

(defun %lazy-compile (src out)
  (if (or (not (probe-file out))
          (< (file-write-date out)
             (file-write-date src)))
    (%compile src out)))

(defun lazy-compile (src &optional out)
  (call-compile #'%lazy-compile src out))
