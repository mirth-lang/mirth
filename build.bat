@echo off

cl /WX /MD /TC mirth0.c /link /WX || goto :error
if exist mirth0.obj del mirth0.obj
mirth0.exe || goto :error
if exist mirth1.c del mirth1.c
rename mirth.c mirth1.c || goto :error

cl /WX /MD /TC mirth1.c /link /WX || goto :error
if exist mirth1.obj del mirth1.obj
mirth1.exe || goto :error
if exist mirth2.c del mirth2.c
rename mirth.c mirth2.c || goto :error

cl /WX /W3 /MD /TC mirth2.c /link /WX || goto :error
if exist mirth2.obj del mirth2.obj
mirth2.exe || goto :error
if exist mirth3.c del mirth3.c
rename mirth.c mirth3.c || goto :error

goto :EOF

:error
echo Build failed with error #%errorlevel%.
exit /b %errorlevel%
