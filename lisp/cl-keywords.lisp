(in-package :hs)

(defun defun->define (def)
  (ds-bind (name args &body body) def
    (if (eq name '|type|) def `((,name ,@args) ,@body))))

(def-hs-macro |defun| (&body body)
  `(|define| ,@(defun->define body)))

(def-syntax-macro |labels| (fs &body body)
  `(|where| ,(mapcar #'defun->define fs) ,@body))


(defun defpac-import (module &rest args)
  (flet ((gencode (key mod)
           (list* key mod args)))
    (cond
      ((atom module)
        (gencode '|import| module))
      ((atom (cdr module))
        (gencode '|import| (car module)))
      (t (gencode '|require| (cadr module))))))

(defun defpac-option (option)
  (let ((key (car option))
        (args (cdr option)))
    (case key
      (:|use| `(progn ,@(mapcar #'defpac-import args)))
      ((:|shadow| :|import|)
        (defpac-import (car args) (cdr args) (eq key :|shadow|))))))

(def-hs-macro |defpackage| (name &rest args)
  (let ((exports (filter :|export| args :test #'eq :key #'car)))
    `(progn
       (|defmodule| ,name ,@(if exports
                              (list (mapcan #'cdr exports))))
       ,@(mapcar #'defpac-option args))))

;; Local Variables:
;; eval: (add-cl-indent-rule (quote ds-bind) (quote (&lambda 4 &body)))
;; End:
