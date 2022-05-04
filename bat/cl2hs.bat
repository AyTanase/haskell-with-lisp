@echo off
if %~1==all (call :compile-all) else (call :run "(hs:compile \"%~1\")")
exit /b

:run
ros run --eval "(load \"%~dp0\..\haskell-with-lisp.asd\")" --eval "(require \"haskell-with-lisp\")" --eval %1 --quit
exit /b

:compile-all
setlocal
set FILES=
for /r %%F in (*.hl) do (call :collect %%F)
call :run "(hs:compile-all %FILES%)"
endlocal
exit /b

:collect
set NAME=%~1
set FILES=%FILES% \"%NAME:\=/%\"
exit /b
