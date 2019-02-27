(in-package :cl)
(load (merge-pathnames "haskell" *load-truename*))

(in-package :hs)
(export 'repl)

(setf *debug-io* (make-two-way-stream *standard-input* *error-output*))


(defun ghci-eval (expr)
  (let ((form (if (consp expr) (car expr))))
    (if (keywordp form)
      (progn
        (prin1 form)
        (mapc (curry #'haskell-tops " ") (cdr expr)))
      (handler-case
          (let ((*error-output* #.(make-broadcast-stream)))
            (eval expr))
        (error () (%define-print expr))))))

(defun ghci-print (expr)
  (write-line ":{")
  (unwind-protect (ghci-eval expr)
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
     (ghci-eval `(:|load| ,',file))))
