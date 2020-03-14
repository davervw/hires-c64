export ACME=/c/Users/Dave/Downloads/acme0.96.4win/acme/ACME_Lib 
export VICE=/c/Users/Dave/Downloads/GTK3VICE-3.3-win32/GTK3VICE-3.3-win32-r35872
bin/win/acme -f cbm -l build/labels -o build/hires.prg code/hires.asm
[ $? -eq 0 ] || exit 1
bin/win/prgsize build/hires.prg > build/size.dat
[ $? -eq 0 ] && cat build/loaderbasic.prg build/loaderml.prg build/size.dat build/hires.prg > build/loadhires.prg
[ $? -eq 0 ] && ${VICE}/c1541 << EOF
attach build/hires.d64
delete "hires ml"
delete "loadhires"
write build/loadhires.prg "loadhires"
EOF
[ $? -eq 0 ] && cp build/hires.d64 /c/Users/Dave/Dropbox/Commodore/
[ $? -eq 0 ] && ${VICE}/x64.exe -moncommands build/labels build/hires.d64
