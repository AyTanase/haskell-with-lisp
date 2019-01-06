(in-package :hs)


(defvar *hs-readtable* (copy-readtable *readtable*))

(define-symbol-macro *haskell-case* (readtable-case *hs-readtable*))

(defmacro def-read-as (name case-name)
  `(defun ,name (stream &rest args)
     (declare (ignore args))
     (let ((default *haskell-case*))
       (unwind-protect
            (progn
              (setf *haskell-case* ,case-name)
              (read stream t nil t))
         (setf *haskell-case* default)))))

(def-read-as read-as-lisp :upcase)
(def-read-as read-as-haskell :preserve)

(let ((*readtable* *hs-readtable*))
  (set-macro-character #\' (get-macro-character #\') t)
  (set-dispatch-macro-character #\# #\? #'read-as-lisp)
  (set-dispatch-macro-character #\# #\! #'read-as-haskell))

(setf *haskell-case* :preserve)


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
