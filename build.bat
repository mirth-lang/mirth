@echo off

cl /WX /MD /TC mirth0.c || goto :error
mirth0.exe || goto :error
del mirth1.c
rename mirth.c mirth1.c || goto :error

cl /WX /MD /TC mirth1.c || goto :error
mirth1.exe || goto :error
del mirth2.c
rename mirth.c mirth2.c || goto :error

cl /WX /MD /TC mirth2.c || goto :error
mirth2.exe || goto :error
del mirth3.c
rename mirth.c mirth3.c || goto :error

goto :EOF

:error
echo Build failed with error #%errorlevel%.
exit /b %errorlevel%
