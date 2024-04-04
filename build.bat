@echo off

cl /WX /MD /TC bin\mirth0.c /link /WX  || goto :error
mirth0.exe -p std:src/std -p args:src/args -p posix:src/posix -p mirth:src/mirth src/mirth/main.mth -o bin/mirth1.c || goto :error

cl /WX /MD /TC bin\mirth1.c /link /WX  || goto :error
mirth1.exe -p std:src/std -p args:src/args -p posix:src/posix -p mirth:src/mirth src/mirth/main.mth -o bin/mirth2.c || goto :error

cl /WX /W3 /MD /TC bin\mirth2.c /link /WX  || goto :error
mirth2.exe -p std:src/std -p args:src/args -p posix:src/posix -p mirth:src/mirth src/mirth/main.mth -o bin/mirth3.c || goto :error

goto :EOF

:error
echo Build failed with error #%errorlevel%.
exit /b %errorlevel%
