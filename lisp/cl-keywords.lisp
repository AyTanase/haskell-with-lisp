(in-package :hs)

(defun defun->define (def)
  (destructuring-bind (name args &body body) def
    (if (eq name '|type|) def `((,name ,@args) ,@body))))

(def-hs-macro |defun| (&body body)
  `(|define| ,@(defun->define body)))

(def-syntax-macro |labels| (fs &rest body)
  `(|where| ,(mapcar #'defun->define fs) ,@body))
