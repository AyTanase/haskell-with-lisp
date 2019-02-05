(require 'lisp-custom)

(defun lisp-to-haskell (file-name)
  (interactive "sFile Name: ")
  (shell-command (format "cl2hs %S" file-name)))

(defun to-haskell ()
  (interactive)
  (lisp-to-haskell (buffer-file-name)))

(define-derived-mode haskell-lisp-mode lisp-mode "Haskell-Lisp"
  :after-hook
  (setq-local font-lock-keywords-case-fold-search nil))

(cl-indent-rules '(4 &body) 'define 'udef 'defmodule)
(cl-indent-rules '((&whole 4 &rest (&whole 1 &body)) &body) 'where 'uwhere)
(cl-indent-rules '(4 &lambda (&whole 2 &body)) 'class 'uclass 'instance 'uinstance)
(add-cl-indent-rule 'ulet 'lisp-indent-let-method)
(add-cl-indent-rule 'ulabels '((&whole 4 &rest (&whole 1 &lambda &body)) &body))
(add-cl-indent-rule 'do '(&body))
(add-cl-indent-rule 'if-bind '((&whole 4 &body) &body))

(defconst haskell-lisp-keywords
  '(class data define defmodule enum-from if-bind import instance
    setf type ucase uclass udef ulabels ulet uinstance uwhere where))

(font-lock-add-keywords
 'haskell-lisp-mode
 `((,(concat "(" (regexp-opt (mapcar #'symbol-name haskell-lisp-keywords) t) "\\_>")
     1 font-lock-keyword-face)
   ("(type\\s-+(\\([^)]*\\))" 1 font-lock-function-name-face)
   ("(\\(?:define\\s-+(?\\|type\\s-\\)\\s-*\\(\\S-*\\)" 1 font-lock-function-name-face)
   ("(\\(extension\\s-[^)]*\\))" 1 font-lock-preprocessor-face)
   ("(\\(a\\(?:ppe\\)?nd\\|or\\|compose\\|->?\\|[+*/]\\)\\_>" 1 font-lock-variable-name-face)
   ("#\\(?:\\?\\|\\\\.\\)" . font-lock-negation-char-face)
   ("\\_<\\([[:upper:]][[:word:]']*\\|list\\*?\\|tuple\\|nil\\|cons\\|pair\\)\\_>" 1 font-lock-type-face)
   ("\\_<\\?.*?\\_>" . font-lock-builtin-face)))

(modify-syntax-entry ?{ "_ 2bn" haskell-lisp-mode-syntax-table)
(modify-syntax-entry ?} "_ 3bn" haskell-lisp-mode-syntax-table)

(define-key haskell-lisp-mode-map (kbd "C-c C-c") 'to-haskell)

(push '("\\.hl\\'" . haskell-lisp-mode) auto-mode-alist)

(provide 'haskell-lisp)
