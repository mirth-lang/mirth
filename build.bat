@echo off

cl /WX /MD /TC bin\mirth0.c /link /WX /OUT:bin\mirth0.exe  || goto :error
bin\mirth0.exe || goto :error
if exist bin\mirth1.c del bin\mirth1.c
rename bin\mirth.c bin\mirth1.c || goto :error

cl /WX /MD /TC bin\mirth1.c /link /WX /OUT:bin\mirth1.exe  || goto :error
bin\mirth1.exe || goto :error
if exist bin\mirth2.c del bin\mirth2.c
rename bin\mirth.c bin\mirth2.c || goto :error

cl /WX /W3 /MD /TC bin\mirth2.c /link /WX  /OUT:bin\mirth2.exe  || goto :error
bin\mirth2.exe || goto :error
if exist bin\mirth3.c del bin\mirth3.c
rename bin\mirth.c bin\mirth3.c || goto :error

goto :EOF

:error
echo Build failed with error #%errorlevel%.
exit /b %errorlevel%
