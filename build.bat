@echo on
set ACME=%USERPROFILE%\Downloads\acme0.97win\acme
set VICE=%USERPROFILE%\Downloads\GTK3VICE-3.7-win64\bin
%ACME%\acme -f cbm -l build\labels -o build\hires.prg code\hires.asm 
if errorlevel 1 goto quit
bin\win\prgsize.exe build\hires.prg > build\size.dat
if errorlevel 1 goto quit
copy /b build\loaderbasic.prg+build\loaderml.prg+build\size.dat+build\hires.prg build\loadhires.prg
if errorlevel 1 goto quit
%VICE%\c1541 < bin\win\c1541.txt
if errorlevel 1 goto quit
%VICE%\x64sc.exe -moncommands build\labels build\hires.d64
if errorlevel 1 goto quit
exit
:quit
pause