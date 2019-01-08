@echo off
setlocal
set SRC="%~dp0\..\lisp\haskell.lisp"
if %~1==all (call :compile-all) else (call runcl %SRC% --eval "(hs:compile \"%~1\")")
endlocal
exit /b

:compile-all
set "FILES="
call :collect-files .
call runcl %SRC% --eval "(hs:compile-all %FILES%)"
rem echo %FILES%
exit /b

:collect-files
for /d %%d in (*) do (
	cd %%d
	call :collect-files "%~1/%%d"
	cd ..
)
for %%f in (*.hl) do (call :collect-file-1 "%~1/%%f")
exit /b

:collect-file-1
set "FILES=\"%~1\" %FILES%"
exit /b
