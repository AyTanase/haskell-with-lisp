(in-package :hs)


(defun tuple (xs)
  (if (listp xs)
    (with-paren
      (arrange xs))
    (haskell-top xs)))

(defpattern |tuple| (&rest xs)
  (tuple xs))


(defun =>-left (args)
  (when (consp args)
    (cond
      ((atom (car args))
        (haskell-top args))
      ((atom (cdr args))
        (haskell-top (car args)))
      (t (tuple args)))
    (write-string " => ")))

(defsyntax => (args type)
  (=>-left args)
  (haskell-top type))


(defun %class-derive (args)
  (if (consp args)
    (ds-bind (key . rest) args
      (if (keywordp key)
        (progn
          (format t "{-# ~@:(~a~) #-} " key)
          (=>-left rest))
        (=>-left args)))))

(defun %class (key name derive defs)
  (format t "~a " key)
  (%class-derive derive)
  (haskell-top name)
  (when defs
    (write-string " where")
    (with-indent 1
      (map-indent #'%define defs))))

(defmacro def-class-macro (key)
  `(def-hs-macro ,key (name &optional derive &body defs)
     `(%class ',',key ',name ',derive ',defs)))

(def-class-macro |class|)
(def-class-macro |instance|)


(defun module-names (svar names)
  (flet ((print-names (args)
           (with-paren
             (%map-hs (curry #'%map-hs #'tuple " ")
                      ", " args))))
    (when svar
      (write-string " ")
      (if (callp names :|hide|)
        (progn
          (write-string "hiding ")
          (print-names (cdr names)))
        (print-names names)))))

(def-hs-macro |defmodule| (module &optional (names nil svar))
  `(progn
     (haskell-tops "module " ',module)
     (module-names ,svar ',names)
     (write-string " where")))

(def-hs-macro |import| (module &optional (names nil svar))
  `(progn
     (haskell-tops "import " ',module)
     (module-names ,svar ',names)))

(defhasq :|m| "module")
(defhasq :|q| "qualified")
(defhasq :|all| "(..)")


(def-hs-macro |deftype| (name type)
  `(haskell-tops "type " ',name " = " ',type))


(defun %data-body (body)
  (if (and (consp body)
           (callp (cdr body) :|name|))
    (progn
      (haskell-tops (car body) " { ")
      (%map-hs (curry #'apply #'%type)
               ", " (cddr body))
      (write-string " }"))
    (haskell-top body)))

(defun %data (key name body deriving)
  (format t "~a " key)
  (haskell-tops name " = ")
  (if (callp body '|or|)
    (%map-hs #'%data-body " | " (cdr body))
    (%data-body body))
  (when deriving
    (write-string " deriving ")
    (tuple deriving)))

(defmacro def-data-macro (key)
  `(def-hs-macro ,key (name constr &optional deriving)
     `(%data ',',key ',name ',constr ',deriving)))

(def-data-macro |data|)
(def-data-macro |newtype|)


(def-hs-macro |extension| (&rest args)
  `(format t "{-# LANGUAGE ~{~a~^, ~} #-}" ',args))

;; Local Variables:
;; eval: (add-cl-indent-rule (quote ds-bind) (quote (&lambda 4 &body)))
;; eval: (add-cl-indent-rule (quote with-paren) (quote (&body)))
;; End:
