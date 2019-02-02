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


(defmacro defparen (name open close)
  (with-gensyms (body)
    `(defmacro ,name (&body ,body)
       `(progn
          (write-string ,,open)
          ,@,body
          (write-string ,,close)))))

(defparen with-paren "(" ")")
(defparen with-square-brackets "[" "]")
(defparen with-pragma "{-# " " #-}")


(defgeneric haskell (x)
  (:documentation "Convert to Haskell code"))

(defun strhask (x)
  (with-output-to-string (*standard-output*)
    (haskell x)))

(defun haskells (&rest args)
  (mapc #'haskell args))

(defun %rechask (x fn between)
  (labels ((rec (x xs)
             (funcall fn x)
             (when xs
               (write-string between)
               (rec (car xs) (cdr xs)))))
    (typecase x
      (null)
      (cons (rec (car x) (cdr x)))
      (t (haskell x)))))

(defmacro defrechask (name fn default)
  (with-gensyms (x between)
    `(defun ,name (,x &optional (,between ,default))
       (%rechask ,x ,fn ,between))))

(defrechask rechask #'haskell " ")
(defrechask arrange #'rechask ", ")


(defmacro def-key-table (table setter)
  `(progn
     (defvar ,table (make-hash-table :test 'eq))
     (defmacro ,setter (name &body body)
       `(progn
          (shadow-haskell ',name)
          (setf (gethash ',name ,',table)
                #'(lambda ,@body))))))

(def-key-table *syntax-macros* def-syntax-macro)
(def-key-table *syntax* defsyntax)
(def-key-table *topkeys* deftopkey)


(defun hs-macro-expand (expr)
  (let ((fn (if (consp expr)
              (gethash (car expr) *syntax-macros*))))
    (if fn
      (hs-macro-expand (apply fn (cdr expr)))
      expr)))

(defun %haskell-cons (expr)
  (let ((fn (gethash (car expr) *syntax*)))
    (if fn
      (apply fn (cdr expr))
      (with-paren
        (rechask expr)))))

(defun haskell-top (expr)
  (if (atom expr)
    (haskell expr)
    (let* ((expanded (hs-macro-expand expr))
           (fn (gethash (car expanded) *topkeys*)))
      (if fn
        (apply fn (cdr expanded))
        (%haskell-cons expanded)))))

(defun haskell-tops (&rest args)
  (mapc #'haskell-top args))


(defmethod haskell (x) (princ x))

(defmethod haskell ((x character))
  (cond
    ((char= x #\') (format t "'\\''"))
    ((char= x #\\) (format t "'\\\\'"))
    ((graphic-char-p x) (format t "'~c'" x))
    (t (format t "'\\x~x'" (char-code x)))))

(defmethod haskell ((x null))
  (write-string "()"))

(defmethod haskell ((x cons))
  (%haskell-cons (hs-macro-expand x)))


(defvar *patterns* (make-hash-table :test 'eq))

(defun patternp (x) (gethash x *patterns*))

(defmacro defpattern (name &body body)
  `(progn
     (setf (gethash ',name *patterns*) t)
     (defsyntax ,name ,@body)))


(defmacro defhasq (name body)
  `(progn
     (shadow-haskell ',name)
     (defmethod haskell ((x (eql ',name)))
       (write-string ,body))))


(load-relative "keywords.lisp")
(load-relative "cl-keywords.lisp")
(load-relative "unify.lisp")
(load-relative "functions.lisp")

;; Local Variables:
;; eval: (add-cl-indent-rule (quote with-picking-out) (quote (6 4 &body)))
;; eval: (add-cl-indent-rule (quote with-paren) (quote (&body)))
;; End:
