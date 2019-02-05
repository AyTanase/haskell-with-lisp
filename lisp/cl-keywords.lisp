(in-package :hs)

(defun defun->define (def)
  (ds-bind (name args &body body) def
    (if (eq name '|type|) def `((,name ,@args) ,@body))))

(def-hs-macro |defun| (&body body)
  `(|define| ,@(defun->define body)))

(def-syntax-macro |labels| (fs &body body)
  `(|where| ,(mapcar #'defun->define fs) ,@body))


(defun defpac-import (opt)
  (ds-bind (key &rest args) opt
    (case key
      (:|use| `(progn ,@(mapcar (curry #'list '|import|) args)))
      (:|import| `(|import| ,(car args) ,(cdr args)))
      (:|shadow| `(|import| ,(car args) (:|hide| ,@(cdr args)))))))

(def-hs-macro |defpackage| (name &rest args)
  (mv-bind (exs ins) (partition (curry #'eq :|export|) args :key #'car)
    `(progn
       (|defmodule| ,name ,@(if exs (list (mapcan #'cdr exs))))
       ,@(mapcar #'defpac-import ins))))

;; Local Variables:
;; eval: (cl-indent-rules (quote (&lambda 4 &body)) (quote ds-bind) (quote mv-bind))
;; End:
