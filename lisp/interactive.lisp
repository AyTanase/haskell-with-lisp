(in-package :cl)

(load (merge-pathnames "haskell" *load-truename*))

(in-package :hs)

(defmacro |progn| (&body body) `(progn ,@body))


(setf *debug-io* (make-two-way-stream *standard-input* *error-output*))

(defun %repl ()
  (loop for item = (read)
    until (equal item '(|exit|))
    do (write-line ":{")
       (unwind-protect
           (if (and (consp item)
                    (keywordp (car item)))
             (progn
               (prin1 (car item))
               (mapc (curry #'haskell-tops " ") (cdr item)))
             (handler-case (eval item)
               ((or unbound-variable
                    undefined-function
                    warning)
                   () (%define-print item))))
         (fresh-line)
         (write-line ":}"))))

(defun repl ()
  (let ((*package* (find-package :hs))
        (*readtable* *hs-readtable*))
    (restart-case (%repl)
      (abort ()
        :report "Exit debugger, returning to GHCi."
        (fresh-line)
        (repl)))))
