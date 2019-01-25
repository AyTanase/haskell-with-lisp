(in-package :hs)


(defpackage :|haskell| (:nicknames :|hs|))

(defun shadow-haskell (x)
  (export (intern (string x) :|hs|) :|hs|))


(defmacro def-hs-macro (name &body body)
  `(progn
     (shadow-haskell ',name)
     (defmacro ,name ,@body)))


(defun pick-out-decs-doc (body &optional (doc-allowed t))
  "Pick out declarations and a documentation."
  (labels ((pick-out-1 (decs body doc-allowed)
             (flet ((return-values ()
                      (values (nreverse decs) body))
                    (recurse (doc-allowed)
                      (pick-out-1 (cons (car body) decs)
                                  (cdr body)
                                  doc-allowed)))
               (if (atom body)
                 (return-values)
                 (cond
                   ((and doc-allowed
                         (stringp (car body))
                         (cdr body))
                    (recurse nil))
                   ((and (consp (car body))
                         (eq (caar body) 'declare))
                    (recurse doc-allowed))
                   (t (return-values)))))))
    (pick-out-1 nil body doc-allowed)))

(defmacro with-picking-out (args code &body body)
  `(multiple-value-bind ,args (pick-out-decs-doc ,code)
     ,@body))


(defmacro defkeyword (name args &body body)
  (with-picking-out (decs rest) body
    `(def-hs-macro ,name ,args
       ,@decs
       `(progn ,(progn ,@rest) (fresh-line)))))


(defvar *syntax* (make-hash-table :test 'eq))

(defmacro defsyntax (name &body body)
  `(progn
     (shadow-haskell ',name)
     (setf (gethash ',name *syntax*) #'(lambda ,@body))))

(defmacro def-syntax-macro (name args &body body)
  (with-picking-out (decs rest) body
    `(defsyntax ,name ,args
       ,@decs
       (haskell (progn ,@rest)))))


(defgeneric haskell (x)
  (:documentation "Convert to Haskell code"))

(defun strhask (x)
  (with-output-to-string (*standard-output*)
    (haskell x)))


(defmacro defparen (name open close)
  (let ((body (gensym)))
    `(defmacro ,name (&body ,body)
       `(progn
          (format t ,,open)
          ,@,body
          (format t ,,close)))))

(defparen with-paren "(" ")")
(defparen with-square-brackets "[" "]")
(defparen with-pragma "{-# " " #-}")


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
  (let ((x (gensym))
        (between (gensym)))
    `(defun ,name (,x &optional (,between ,default))
       (%rechask ,x ,fn ,between))))

(defrechask rechask #'haskell " ")
(defrechask arrange #'rechask ", ")


(defmethod haskell (x) (format t "~a" x))

(defmethod haskell ((x character))
  (cond
    ((char= x #\') (format t "'\\''"))
    ((char= x #\\) (format t "'\\\\'"))
    ((graphic-char-p x) (format t "'~c'" x))
    (t (format t "'\\x~x'" (char-code x)))))

(defmethod haskell ((x null)) (format t "()"))

(defmethod haskell ((x cons))
  (let ((rule (gethash (car x) *syntax*)))
    (if rule
      (apply rule (cdr x))
      (with-paren (rechask x)))))


(defvar *patterns* (make-hash-table :test 'eq))

(defun patternp (x) (gethash x *patterns*))

(defmacro defpattern (name &body body)
  `(progn
     (setf (gethash ',name *patterns*) t)
     (defsyntax ,name ,@body)))


(defmacro defhasq (name body)
  `(progn
     (shadow-haskell ',name)
     (defmethod haskell ((x (eql ',name))) (format t ,body))))


(load-relative "keywords.lisp")
(load-relative "unify.lisp")
(load-relative "functions.lisp")

;; Local Variables:
;; eval: (add-cl-indent-rule (quote with-picking-out) (quote (6 4 &body)))
;; End:
