(in-package :cl)
(load (merge-pathnames "haskell" *load-truename*))

(in-package :hs)
(export 'repl)

(setf *debug-io* (make-two-way-stream *standard-input* *error-output*))


(defun ghci-print (expr)
  (write-line ":{")
  (unwind-protect
      (if (and (consp expr)
               (keywordp (car expr)))
        (progn
          (prin1 (car expr))
          (mapc (curry #'haskell-tops " ") (cdr expr)))
        (handler-case
            (let ((*error-output* #.(make-broadcast-stream)))
              (eval expr))
          (error () (%define-print expr))))
    (fresh-line)
    (write-line ":}")))

(defun repl ()
  (let ((*package* (find-package :hs))
        (*readtable* *hs-readtable*))
    (loop
      (with-simple-restart
          (abort "Exit debugger, returning to GHCi.")
        (let ((expr (read)))
          (if (equal expr '(|exit|))
            (return)
            (ghci-print expr)))))))


(defmacro |progn| (&body body) `(progn ,@body))

(defmacro |load| (file)
  `(progn
     (lazy-compile ,(string file))
     (format t ":load ~a~%" ',file)))
