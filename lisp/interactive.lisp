(in-package :cl)

(load (merge-pathnames "haskell" *load-truename*))

(in-package :hs)

(def-hs-macro |eval| (expr) `(%define-print ',expr))

(defmacro |progn| (&body body) `(progn ,@body))


(setf *debug-io* (make-two-way-stream *standard-input* *error-output*))

(defun %repl ()
  (loop for item = (read)
    until (equal item '(|exit|))
    do (if (and (consp item)
                (keywordp (car item)))
         (format t ":~{~a~^ ~}~%" item)
         (unwind-protect
             (progn
               (write-line ":{")
               (eval item))
           (fresh-line)
           (write-line ":}")))))

(defun repl ()
  (let ((*package* (find-package :hs))
        (*readtable* *hs-readtable*))
    (restart-case (%repl)
      (abort ()
        :report "Exit debugger, returning to GHCi."
        (fresh-line)
        (repl)))))
