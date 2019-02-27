@echo off
setlocal
set SRC="%~dp0\..\lisp\haskell.lisp"
if %~1==all (call :compile-all) else (call runcl %SRC% --eval "(hs:compile \"%~1\")")
endlocal
exit /b

:compile-all
set FILES=
for /r %%F in (*.hl) do (call :collect %%F)
call runcl %SRC% --eval "(hs:compile-all %FILES%)"
exit /b

:collect
set NAME=%~1
set FILES=%FILES% \"%NAME:\=/%\"
exit /b
