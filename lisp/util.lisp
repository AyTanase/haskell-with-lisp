(in-package :hs)


(defvar *cl-readtable* *readtable*)
(defvar *hs-readtable*)

(defmacro read-by (readtable)
  `#'(lambda (stream &rest args)
       (declare (ignore args))
       (let ((*readtable* ,readtable))
         (read stream t nil t))))

(set-dispatch-macro-character #\# #\? (read-by *cl-readtable*))
(set-dispatch-macro-character #\# #\! (read-by *hs-readtable*))

(setf *hs-readtable* (copy-readtable *cl-readtable*))
(setf (readtable-case *hs-readtable*) :preserve)
(set-macro-character #\' (get-macro-character #\') t *hs-readtable*)


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

(defun lazy-compile (src &optional (out (default-out src)))
  (if (or (not (probe-file out))
          (< (file-write-date out)
             (file-write-date src)))
    (compile src out)))
