(defun add-cl-indent-rule (symbol rule)
  "Add an indentation RULE of SYMBOL in `common-lisp-indent-function'."
  (interactive "SSymbol: \nxRule: ")
  (eval-after-load 'cl-indent
    `(put ',symbol 'common-lisp-indent-function ',rule)))

(defun cl-indent-rules (rule &rest symbols)
  (mapc (lambda (symbol)
          (add-cl-indent-rule symbol rule))
        symbols))

(defun runcl (file-name)
  (interactive "sFile Name: ")
  (shell-command (format "runcl %S" file-name)))

(defun runcl-this ()
  (interactive)
  (runcl (buffer-file-name)))

(defun lisp-indent-cond-method
    (path state indent-point sexp-column normal-indent)
  (if (and (cdr path) (cddr path))
      normal-indent
    (max normal-indent (+ sexp-column 2))))


(setq lisp-indent-function 'common-lisp-indent-function
      lisp-loop-keyword-indentation 2
      lisp-simple-loop-indentation 2)

(add-cl-indent-rule 'or '(&body))
(add-cl-indent-rule 'with-open-file '(&lambda &body))
(add-cl-indent-rule 'ftype '((&whole 4 &lambda 2) &rest 2))
(add-cl-indent-rule 'cond 'lisp-indent-cond-method)
(add-cl-indent-rule 'case '(4 &rest (&whole 2 &body)))
(add-cl-indent-rule 'defsystem '(4 &rest (&whole 2 &lambda)))
(cl-indent-rules '(4 &body) 'if 'unwind-protect)
(cl-indent-rules '((&whole 4 &rest (&whole 1 2)) &body) 'let 'let*)

(font-lock-add-keywords
 'lisp-mode
 '(("(\\(def\\S-*?\\_>\\)" 1 font-lock-keyword-face)
   ("(def\\S-*?\\_>\\s-+\\_<\\(\\S-*?\\_>\\)" 1 font-lock-function-name-face))
 t)

(font-lock-add-keywords 'lisp-mode '(("#:\\S-+?\\_>" . font-lock-builtin-face)))

(modify-syntax-entry ?| "_ 23bn" lisp-mode-syntax-table)

(define-key lisp-mode-map (kbd "C-c C-c") 'runcl-this)


(provide 'lisp-custom)
