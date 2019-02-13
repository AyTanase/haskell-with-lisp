(in-package :hs)


(defun %type (vars type)
  (arrange vars)
  (haskell-tops " :: " type))

(def-hs-macro |type| (vars type)
  `(%type ',vars ',type))

(defspecial |type| (expr type)
  (haskell-tops expr " :: " type))


(defun %define-expand (x)
  (let ((expr (hs-macro-expand x)))
    (if (atom expr)
      expr
      (let ((spec (car expr))
            (args (cdr expr)))
        (case (gethash spec *specials*)
          ((special pattern) expr)
          (operator (cons spec (mapcar #'%define-expand args)))
          (t (if (atom args)
               (%define-expand spec)
               `(|funcall| ,@expr))))))))

(declaim (inline %define-print))
(defun %define-print (expr)
  (%haskell-top (%define-expand expr)))


(declaim (ftype function %define))
(defun where (defs)
  (if defs
    (with-indent 1
      (indent)
      (write-string "where")
      (with-indent 1
        (map-indent #'%define defs)))))

(defun print-guard-1 (expr)
  (if (callp expr '|and|)
    (%rechask (cdr expr) #'print-guard-1 ", ")
    (haskell expr)))

(defun print-guards (assign gvs defs)
  (flet ((print-1 (g v)
           (write-string "| ")
           (print-guard-1 g)
           (write-string assign)
           (%define-print v)))
    (with-indent 1
      (map-indent #'print-1 gvs))
    (where defs)))


(declaim (ftype function reduce-guard-1)
         (inline truep))

(defun truep (x)
  (or (eq x '|True|)
      (eq x '|otherwise|)))

(defun merge-guards (x y)
  (cond
    ((truep x) y)
    ((truep y) x)
    (t `(|and| ,x ,y))))

(defun reduce-guard-if (f guard expr)
  (ds-bind (x y &optional (z nil svar)) (cdr expr)
    (let ((vg (if svar (funcall f guard) guard)))
      (nconc (reduce-guard-1 f (merge-guards vg x) y)
             (if svar (reduce-guard-1 f vg z))))))

(defun reduce-guard-if-bind (f guard expr)
  (ds-bind (x y &optional (z nil svar)) (cdr expr)
    (let ((w `(|setf| ,@x))
          (vg (if svar (funcall f guard) guard)))
      (cons (list (merge-guards vg w) y)
            (if svar (reduce-guard-1 f vg z))))))

(defun reduce-guard-1 (f guard value)
  (let ((expr (hs-macro-expand value)))
    (flet ((base () (list (list guard expr))))
      (cond
        ((atom expr) (base))
        ((eq (car expr) '|if|)
          (reduce-guard-if f guard expr))
        ((eq (car expr) '|if-bind|)
          (reduce-guard-if-bind f guard expr))
        (t (base))))))

(defun reduce-guards (assign defs expr)
  (let ((gs nil))
    (flet ((gpush (guard)
             (if (truep guard)
               guard
               (let ((v (genvar)))
                 (push (list v guard) gs)
                 v))))
      (let ((gvs (reduce-guard-1 #'gpush '|otherwise| expr)))
        (print-guards assign gvs (append defs (nreverse gs)))))))

(defun %define-guard (assign defs expr)
  (if (callp expr '|if|)
    (reduce-guards assign defs expr)
    (progn
      (write-string assign)
      (%define-print expr)
      (where defs))))

(defun %define-right (assign value)
  (let ((expr (hs-macro-expand value)))
    (if (callp expr '|where|)
      (%define-guard assign (second expr)
                     (hs-macro-expand (third expr)))
      (%define-guard assign nil expr))))


(defun remove-dot (list)
  (loop for (x . xs) on list
    nconc (cons x (if (atom xs) (list xs)))))

(defun expand-? (list)
  (if (last list 0)
    `(|list*| ,@(remove-dot list))
    `(|list| ,@list)))

(defun read-? (stream &rest args)
  (declare (ignore args))
  (let ((item (read stream t nil t)))
    (typecase item
      (list (expand-? item))
      (symbol `(? ,item))
      (t item))))

(set-macro-char #\? #'read-? t *hs-readtable*)


(let ((accept nil) found bound)
  (defun %define-left (expr)
    (setf accept t found nil bound nil)
    (haskell-top expr)
    (setf accept nil)
    (if bound
      `(|and| ,@(nreverse bound))))
  (defpattern ? (expr)
    (if (and accept (symbolp expr))
      (if (member expr found :test #'eq)
        (let ((var (genvar)))
          (push `(= ,expr ,var) bound)
          (haskell var))
        (progn
          (push expr found)
          (haskell expr)))
      (haskell expr))))

(defun %define (var val &optional (assign " = "))
  (if (eq var '|type|)
    (%type val assign)
    (let* ((bound (%define-left var))
           (expr (if bound `(|if| ,bound ,val) val)))
      (%define-right assign expr))))

(def-hs-macro |define| (var val)
  `(%define ',var ',val))

;; Local Variables:
;; eval: (add-cl-indent-rule (quote ds-bind) (quote (&lambda 4 &body)))
;; End:
