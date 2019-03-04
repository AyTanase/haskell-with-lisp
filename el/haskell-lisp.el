(require 'lisp-custom)

(defun symbol-regexp-opt (symbols &optional paren)
  (regexp-opt (mapcar #'symbol-name symbols) paren))

(defun lisp-to-haskell (file-name)
  (interactive "sFile Name: ")
  (shell-command (format "cl2hs %S" file-name)))

(defun to-haskell ()
  (interactive)
  (lisp-to-haskell (buffer-file-name)))

(define-derived-mode haskell-lisp-mode lisp-mode "Haskell-Lisp"
  :after-hook
  (setq-local font-lock-keywords-case-fold-search nil))

(cl-indent-rules '(4 &body) 'define 'defmodule 'handle 'type)
(cl-indent-rules '(4 &lambda &rest (&whole 2 &body)) 'class 'instance)
(add-cl-indent-rule 'where '((&whole 4 &rest (&whole 1 &body)) &body))
(add-cl-indent-rule 'do '(&body))

(let ((keywords
        '(as bind class data define defmodule enum-from
          flip funcall import instance newtype type where))
      (operators
        '(+ - * / and or append compose
          -> => = /= <= >= < >
          progn prog1 alt >>= =<< >>))
      (constructors '(nil cons list* list tuple pair)))
  (font-lock-add-keywords
   'haskell-lisp-mode
   `((,(concat "(" (symbol-regexp-opt keywords t) "\\_>") 1 font-lock-keyword-face)
     ("(type\\s-+(\\([^)]*\\))" 1 font-lock-function-name-face)
     ("(\\(?:define\\s-+(?\\|type\\s-\\)\\s-*\\(\\S-+\\)" 1 font-lock-function-name-face)
     ("(\\(extension\\s-[^)]*\\))" 1 font-lock-preprocessor-face)
     (,(concat "(" (symbol-regexp-opt operators t) "\\_>") 1 font-lock-variable-name-face)
     ("#\\(?:\\?\\|\\\\.\\)" . font-lock-negation-char-face)
     ,(cons (concat "\\_<\\(?:[[:upper:]][[:word:]']*\\|"
                    (symbol-regexp-opt constructors)
                    "\\_>\\)")
            font-lock-type-face)
     ("\\_<\\?\\S-*?\\_>" . font-lock-builtin-face))))

(let ((table haskell-lisp-mode-syntax-table))
  (modify-syntax-entry ?{ "_ 2bn" table)
  (modify-syntax-entry ?} "> bn" table))

(define-key haskell-lisp-mode-map (kbd "C-c C-c") 'to-haskell)

(push '("\\.hl\\'" . haskell-lisp-mode) auto-mode-alist)

(provide 'haskell-lisp)
