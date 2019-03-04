(in-package :hs)

(def-syntax-macro |cond| (x &body xs)
  `(|if| ,@x ,@(if xs `((|cond| ,@xs)))))


(defun defun-to-define (def)
  (ds-bind (name args . body) def
    (if (eq name '|type|)
      def
      `((,name ,@args) ,@body))))

(defmacro |defun| (&body body)
  `(|define| ,@(defun-to-define body)))

(def-syntax-macro |labels| (fs &body body)
  `(|where| ,(mapcar #'defun-to-define fs) ,@body))


(defun defpack-import (opt)
  (ds-bind (key . args) opt
    (case key
      (:|use| `(progn ,@(mapcar (curry #'list '|import|) args)))
      (:|import| `(|import| ,(car args) ,(cdr args)))
      (:|shadow| `(|import| ,(car args) (:|hide| ,@(cdr args)))))))

(defmacro |defpackage| (name &rest args)
  (let ((exs (remove-if-not (curry #'eq :|export|) args :key #'car)))
    `(progn
       (|defmodule| ,name ,@(if exs (list (mapcan #'cdr exs))))
       ,@(mapcar #'defpack-import args))))
