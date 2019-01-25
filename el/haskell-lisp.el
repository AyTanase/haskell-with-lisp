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

(add-cl-indent-rule 'define '(4 &body))
(add-cl-indent-rule 'defmodule '(4 &body))
(add-cl-indent-rule 'where '((&whole 4 &rest (&whole 1 &body)) &body))
(add-cl-indent-rule 'class '(4 &lambda &body))
(add-cl-indent-rule 'instance '(4 &lambda &body))

(font-lock-add-keywords
 'haskell-lisp-mode
 '(("(\\(d\\(?:ef\\(?:ine\\|module\\|data\\)\\|ata\\)\\|u\\(?:def\\(?:data\\)?\\|l\\(?:et\\|abels\\)\\|c\\(?:lass\\|ase\\)\\|instance\\|where\\)\\|type\\|enum-from\\|class\\|i\\(?:nstance\\|mport\\)\\|module\\|where\\)\\_>"
    1 font-lock-keyword-face)
   ("(\\(?:define\\s-+(?\\|type\\s-\\)\\s-*\\([[:lower:]][[:word:]']*\\)" 1 font-lock-function-name-face)
   ("(type\\s-+(\\([^)]*\\))" 1 font-lock-function-name-face)
   ("(\\(extension\\s-[^)]*\\))" 1 font-lock-preprocessor-face)
   ("(\\(a\\(?:ppe\\)?nd\\|or\\|compose\\|->?\\|[+*/]\\)\\_>" 1 font-lock-variable-name-face)
   ("\\_<\\([[:upper:]][[:word:]']*\\|list\\*?\\|tuple\\|nil\\|cons\\|pair\\)\\_>" 1 font-lock-type-face)))

(define-key haskell-lisp-mode-map (kbd "C-c C-c") 'to-haskell)

(push '("\\.hl\\'" . haskell-lisp-mode) auto-mode-alist)

(provide 'haskell-lisp)
