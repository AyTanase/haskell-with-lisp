(in-package :hs)


(defvar *hs-readtable* (copy-readtable *readtable*))

(defun set-haskell-case (case-name)
  (setf (readtable-case *hs-readtable*) case-name))

(defun read-as-lisp (stream &rest _)
  (declare (ignore _))
  (unwind-protect
       (progn
         (set-haskell-case :upcase)
         (read stream t nil t))
    (set-haskell-case :preserve)))

(set-haskell-case :preserve)
(set-macro-character #\' (get-macro-character #\') t *hs-readtable*)
(set-dispatch-macro-character #\# #\? #'read-as-lisp *hs-readtable*)


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
  (if (and (not (probe-file out))
           (< (file-write-date out)
              (file-write-date src)))
    (compile src out)))
