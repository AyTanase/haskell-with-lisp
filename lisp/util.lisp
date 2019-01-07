(in-package :hs)


(defvar *cl-readtable* *readtable*)
(defvar *hs-readtable*)

(defmacro read-by (readtable)
  `#'(lambda (stream &rest args)
       (declare (ignore args))
       (let ((*readtable* ,readtable))
         (read stream t nil t))))

(defun make-fillptr-string (&optional initial-contents)
  (make-array (length initial-contents)
              :element-type 'character
              :initial-contents initial-contents
              :fill-pointer t))

(defun read-hs-string (stream &rest args)
  (declare (ignore args))
  (let ((str (make-fillptr-string '(#\"))))
    (labels ((read-1 ()
               (let ((c (read-char stream t nil t)))
                 (if (char= c #\Return) (read-1) c)))
             (vpush (c) (vector-push-extend c str))
             (recread ()
               (let ((c (read-1)))
                 (vpush c)
                 (unless (char= c #\")
                   (if (char= c #\\) (vpush (read-1)))
                   (recread)))))
      (recread))
    str))

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

(defun compile-all (&rest args) (mapc #'compile args))

(defun lazy-compile (src &optional (out (default-out src)))
  (if (or (not (probe-file out))
          (< (file-write-date out)
             (file-write-date src)))
    (compile src out)))
