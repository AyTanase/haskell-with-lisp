(in-package :hs)


(defvar *syntax* (make-hash-table :test 'eq))

(defmacro defsyntax (name &body body)
  `(setf (gethash ',name *syntax*) #'(lambda ,@body)))

(defgeneric haskell (x)
  (:documentation "Convert to Haskell code"))

(defun strhask (x)
  (with-output-to-string (*standard-output*)
    (haskell x)))


(defmacro with-paren (&body body)
  `(progn
     (format t "(")
     ,@body
     (format t ")")))

(defun %rechask (x fn between)
  (labels ((rec (x xs)
             (funcall fn x)
             (when xs
               (format t between)
               (rec (car xs) (cdr xs)))))
    (typecase x
      (null)
      (cons (rec (car x) (cdr x)))
      (t (haskell x)))))

(defmacro defrechask (name fn default)
  `(defun ,name (x &optional (between ,default))
     (%rechask x ,fn between)))

(defrechask rechask #'haskell " ")
(defrechask arrange #'rechask ", ")


(defmethod haskell (x) (format t "~a" x))

(defmethod haskell ((x character))
  (cond
    ((char= x #\') (format t "'\\''"))
    ((char= x #\\) (format t "'\\\\'"))
    ((graphic-char-p x) (format t "'~c'" x))
    (t (format t "'\\x~x'" (char-code x)))))

(defmethod haskell ((x string))
  (format t "\"")
  (loop for c across x
    do (cond
         ((char= c #\") (format t "\\\""))
         ((char= c #\\) (format t "\\\\"))
         ((graphic-char-p c) (format t "~c" c))
         (t (format t "\\x~x\\&" (char-code c)))))
  (format t "\""))

(defmethod haskell ((x null)) (format t "()"))

(defmethod haskell ((x cons))
  (let ((rule (gethash (car x) *syntax*)))
    (if rule
      (apply rule (cdr x))
      (with-paren (rechask x)))))


(defmacro defhasq (name body)
  `(defmethod haskell ((x (eql ',name))) (format t ,body)))


(defvar *patterns* (make-hash-table :test 'eq))

(defun patternp (x) (gethash x *patterns*))

(defmacro defpattern (name &body body)
  `(progn
     (defsyntax ,name ,@body)
     (setf (gethash ',name *patterns*) t)))


(load-relative "keywords.lisp")
(load-relative "functions.lisp")
