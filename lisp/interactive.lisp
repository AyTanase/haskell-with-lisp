(in-package :cl-user)
(load (merge-pathnames "../haskell-with-lisp.asd" *load-truename*))
(require "haskell-with-lisp")

(in-package :hs)
(export '(repl |exit| |require|))

(setf *debug-io* (make-two-way-stream *standard-input* *error-output*))


(defun ghci-eval (expr)
  (let ((form (call-car #'identity expr)))
    (cond
      ((keywordp form)
        (when (eq form :|cd|)
          (setf *default-pathname-defaults*
                (first (directory (string (second expr))))))
        (prin1 form)
        (mapc (curry #'haskell-tops " ") (cdr expr)))
      (t (handler-case
             (let ((*error-output*
                     (load-time-value (make-broadcast-stream) t)))
               (format *debug-io* "~&~S~&" (eval expr)))
           ((or program-error cell-error) ()
             (%define-print expr))
           (error (condition)
             (error condition)))))))

(defun ghci-print (expr)
  (write-line ":{")
  (unwind-protect (ghci-eval expr)
    (fresh-line)
    (write-line ":}")))

(progn
  (defun ghci-read ()
    (restart-case (read)
      (abort ()
        :report #1="Exit debugger, returning to GHCi."
        (terpri)
        (ghci-read))))
  (defun repl ()
    (let ((*package* (find-package :hs-user))
          (*readtable* *hs-readtable*))
      (loop for expr = (ghci-read)
        until (equal expr '(|exit|))
        do (with-simple-restart (abort #1#)
             (ghci-print expr))))))


(defun |require| (file)
  (lazy-compile (string file))
  (ghci-eval `(:|load| ,file)))
