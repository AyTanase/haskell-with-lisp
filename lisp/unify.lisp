(in-package :hs)

(defun string-tail (s)
  (coerce (cdr (coerce s 'list)) 'string))

(defun symbol-tail (s)
  (intern (string-tail (string s))))

(defun uvarp (x)
  (if (symbolp x)
    (let ((s (string x)))
      (if (char= (char s 0) #\?)
        (intern (string-tail s))))))

(defun unify (expr)
  (let ((vars nil)
        (guards nil))
    (flet ((unify-1 (x)
             (let ((y (uvarp x)))
               (cond
                 ((null y) x)
                 ((find y vars :test #'eq)
                   (let ((v (genvar)))
                     (push `(= ,y ,v) guards)
                     v))
                 (t (push y vars)
                    y)))))
      (let ((uexp (map-tree #'unify-1 expr)))
        (values uexp (nreverse guards))))))


(defun %udef-body (gs val)
  (if gs
    `(|if| (|and| ,@gs) ,val)
    val))

(defun %udef (var val &rest args)
  (multiple-value-bind (expr guards) (unify var)
    (list* expr (%udef-body guards val) args)))

(def-hs-macro |udef| (var val)
  `(|define| ,@(%udef var val)))


(defun map-udef (defs)
  (mapcar (curry #'apply #'%udef) defs))

(defmacro defulet (name)
  `(def-syntax-macro ,name (defs &rest values)
     `(,',(symbol-tail name) ,(map-udef defs) ,@values)))

(defulet |ulet|)
(defulet |uwhere|)

(def-syntax-macro |ulabels| (defs &rest values)
  `(|uwhere| ,(mapcar #'defun->define defs) ,@values))

(def-syntax-macro |ucase| (val &rest defs)
  `(|case| ,val ,@(map-udef defs)))

(defmacro defuclass (name)
  `(def-hs-macro ,name (dec &optional derive &body defs)
     `(,',(symbol-tail name) ,dec ,derive ,@(map-udef defs))))

(defuclass |uclass|)
(defuclass |uinstance|)

(def-hs-macro |udefdata| (name &body defs)
  `(|defdata| ,name ,@(map-udef defs)))
