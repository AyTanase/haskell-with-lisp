(in-package :hs)

(def-syntax-macro |cond| (x &body xs)
  `(|if| ,@x ,@(if xs `((|cond| ,@xs)))))


(defun defun->define (def)
  (ds-bind (name args . body) def
    (if (eq name '|type|) def `((,name ,@args) ,@body))))

(defmacro |defun| (&body body)
  `(|define| ,@(defun->define body)))

(def-syntax-macro |labels| (fs &body body)
  `(|where| ,(mapcar #'defun->define fs) ,@body))


(defun defpac-import (opt)
  (ds-bind (key . args) opt
    (case key
      (:|use| `(progn ,@(mapcar (curry #'list '|import|) args)))
      (:|import| `(|import| ,(car args) ,(cdr args)))
      (:|shadow| `(|import| ,(car args) (:|hide| ,@(cdr args)))))))

(defmacro |defpackage| (name &rest args)
  (mv-bind (exs ins) (partition (curry #'eq :|export|) args :key #'car)
    `(progn
       (|defmodule| ,name ,@(if exs (list (mapcan #'cdr exs))))
       ,@(mapcar #'defpac-import ins))))

;; Local Variables:
;; eval: (cl-indent-rules (quote (&lambda 4 &body)) (quote ds-bind) (quote mv-bind))
;; End:
