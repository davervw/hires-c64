export ACME=${USERPROFILE}/Downloads/acme0.97win/acme 
export VICE=${USERPROFILE}/Downloads/GTK3VICE-3.8-win64/bin
${ACME}/acme -f cbm -l build/3dcube.lbl -r build/3dcube.lst -o build/3dcube.prg code/3dcube.asm
[ $? -eq 0 ] || exit 1
${ACME}/acme -f cbm -l build/labels -r build/hires.lst -o build/hires.prg code/hires.asm
[ $? -eq 0 ] || exit 1
bin/win/prgsize build/hires.prg > build/size.dat
[ $? -eq 0 ] && cat build/loaderbasic.prg build/loaderml.prg build/size.dat build/hires.prg > build/loadhires.prg
[ $? -eq 0 ] && ${VICE}/c1541 << EOF
attach build/hires.d64
delete "hires ml"
delete "loadhires"
delete "3dcube.ml"
write build/loadhires.prg "loadhires"
write build/3dcube.prg "3dcube.ml"
EOF
#[ $? -eq 0 ] && cp build/hires.d64 /c/Users/Dave/Dropbox/Commodore/
[ $? -eq 0 ] && ${VICE}/x64sc.exe -moncommands build/labels build/hires.d64
