@echo off
if %~1==all (call :compile-all) else (call :run "(hs:compile \"%~1\")")
exit /b

:run
ros run -e "(load \"%~dp0\..\haskell-with-lisp.asd\")" -e "(require \"haskell-with-lisp\")" -e %1 -q
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
