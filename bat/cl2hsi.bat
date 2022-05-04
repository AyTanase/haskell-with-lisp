@echo off
setlocal
set BASE=%~dp0\..
ros run --load "%BASE%\lisp\interactive.lisp" --eval (hs:repl) --quit | stack ghci --ghci-options "-ghci-script \"%BASE:\=/%/.ghci\""
endlocal
