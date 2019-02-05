(defun add-cl-indent-rule (symbol rule)
  "Add an indentation RULE of SYMBOL in `common-lisp-indent-function'."
  (interactive "SSymbol: \nxRule: ")
  (eval-after-load 'cl-indent
    `(put ',symbol 'common-lisp-indent-function ',rule)))

(defun cl-indent-rules (rule &rest symbols)
  (mapc (lambda (symbol)
          (add-cl-indent-rule symbol rule))
        symbols))

(defun lisp-indent-let-method
    (path state indent-point sexp-column normal-indent)
  (if (>= (car path) 2)
      (if (cdr path)
          normal-indent
        (+ sexp-column 2))
    (+ sexp-column
       (cond ((null (cdr path)) 4)
             ((null (cddr path)) 1)
             (t 2)))))

(defun lisp-indent-cond-method
    (path state indent-point sexp-column normal-indent)
  (if (and (cdr path) (cddr path))
      normal-indent
    (max normal-indent
         (+ sexp-column 2))))

(defun lisp-indent-defpackage-method
    (path state indent-point sexp-column normal-indent)
  (if (cdr path)
      normal-indent
    (+ sexp-column (if (= (car path) 1) 4 2))))

(defun runcl (file-name)
  (interactive "sFile Name: ")
  (shell-command (format "runcl %S" file-name)))

(defun runcl-this ()
  (interactive)
  (runcl (buffer-file-name)))


(setq lisp-indent-function 'common-lisp-indent-function
      lisp-loop-keyword-indentation 2
      lisp-simple-loop-indentation 2)

(add-cl-indent-rule 'if '(4 &rest 2))
(add-cl-indent-rule 'with-open-file '(&lambda &body))
(add-cl-indent-rule 'ftype '((&whole 4 &lambda 2) &rest 2))
(add-cl-indent-rule 'cond 'lisp-indent-cond-method)
(add-cl-indent-rule 'case '(4 &rest (&whole 2 &body)))
(add-cl-indent-rule 'defpackage 'lisp-indent-defpackage-method)
(cl-indent-rules 'lisp-indent-let-method 'let 'let*)

(font-lock-add-keywords 'lisp-mode '(("#:.+?\\_>" . font-lock-builtin-face)))

(modify-syntax-entry ?. "'2" lisp-mode-syntax-table)

(define-key lisp-mode-map (kbd "C-c C-c") 'runcl-this)


(provide 'lisp-custom)
