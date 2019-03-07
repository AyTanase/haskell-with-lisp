(in-package :hs)


(defun %type (vars type)
  (arrange vars)
  (haskell-tops " :: " type))

(def-hs-macro |type| (vars type)
  `(%type ',vars ',type))

(defpattern |type| (expr type)
  (with-paren
    (haskell-tops expr " :: " type)))


(defun %define-expand (x)
  (let ((expr (hs-macro-expand x)))
    (if (atom expr)
      expr
      (ds-bind (spec . args) expr
        (case (get-keytype spec)
          ((:special :pattern)
            expr)
          ((:operator)
            (cons spec (mapcar #'%define-expand args)))
          (otherwise
            (if (atom args)
              (%define-expand spec)
              `(|funcall| ,@expr))))))))

(definline %define-print (expr)
  (%haskell-top (%define-expand expr)))


(declaim (ftype function %define))
(defun where (defs)
  (when defs
    (with-indent 1
      (indent)
      (write-string "where")
      (with-indent 1
        (map-indent #'%define defs)))))

(defun print-guard-1 (expr)
  (if (callp expr '|and|)
    (%map-hs #'print-guard-1 ", " (cdr expr))
    (haskell-top expr)))

(defun print-guards (assign gvs defs)
  (with-indent 1
    (do-indent ((g v) gvs)
      (write-string "| ")
      (print-guard-1 g)
      (princ assign)
      (%define-print v)))
  (where defs))


(definline truep (x)
  (or (eq x '|True|)
      (eq x '|otherwise|)))

(defun merge-guards (x y)
  (cond
    ((truep x) y)
    ((truep y) x)
    (t `(|and| ,x ,y))))

(defun include-bind-p (expr)
  (and (consp expr)
       (case (car expr)
         (|bind| t)
         (|and| (some #'include-bind-p (cdr expr))))))

(declaim (ftype function reduce-guard-1))
(defun reduce-guard-if (gpush guard expr)
  (ds-bind (test then &optional (else nil s?)) (cdr expr)
    (if (and s? (include-bind-p guard))
      (list (list guard expr))
      (let ((var (if s?
                   (funcall gpush guard)
                   guard)))
        (nconc (reduce-guard-1 gpush (merge-guards var test) then)
               (if s? (reduce-guard-1 gpush var else)))))))

(defun reduce-guard-1 (gpush guard value)
  (let ((expr (hs-macro-expand value)))
    (if (callp expr '|if|)
      (reduce-guard-if gpush guard expr)
      (list (list guard expr)))))

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
  (cond
    ((callp expr '|if|)
      (reduce-guards assign defs expr))
    ((null defs)
      (princ assign)
      (%define-print expr))
    (t (write-string assign)
       (with-indent 1
         (%define-print expr))
       (where defs))))

(defun %define-right (assign value)
  (let ((expr (hs-macro-expand value)))
    (if (callp expr '|where|)
      (%define-guard assign (second expr)
                     (hs-macro-expand (third expr)))
      (%define-guard assign nil expr))))


(defclass question ()
  ((value :reader get-? :initarg :value)))

(definline cons-? (value)
  (make-instance 'question :value value))

(defun print-? (stream object)
  (write-char #\? stream)
  (prin1 (get-? object) stream))

(set-pprint-dispatch 'question #'print-?)

(defun/i read-? (stream &rest _)
  (cons-? (read stream t nil t)))

(set-macro-char #\? #'read-? t *hs-readtable*)


(defun remove-dot (list)
  (loop for (x . xs) on list
    nconc (cons x (if (atom xs) (list xs)))))

(defun expand-? (list)
  (if (last list 0)
    `(|list*| ,@(remove-dot list))
    `(|list| ,@list)))

(defmethod hs-macro-expand ((expr question))
  (let ((value (get-? expr)))
    (typecase value
      (list (expand-? value))
      (symbol `(question ,value))
      (t (hs-macro-expand value)))))

(let ((accept nil) found bound)
  (defun %define-left (expr)
    (setf accept t found nil bound nil)
    (haskell-top expr)
    (setf accept nil)
    (if bound
      `(|and| ,@(nreverse bound))))
  (defpattern question (expr)
    (if (and accept (symbolp expr))
      (cond
        ((member expr found)
          (let ((var (genvar (string expr))))
            (push `(= ,expr ,var) bound)
            (haskell var)))
        (t (push expr found)
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
