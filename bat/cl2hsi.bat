@echo off
setlocal
set BASE=%~dp0\..
runcl "%BASE%\lisp\interactive.lisp" --eval (hs:repl) | stack ghci --ghci-options "-ghci-script \"%BASE:\=/%/.ghci\""
endlocal
