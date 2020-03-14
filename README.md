# Hi-Res BASIC Extensions for Commodore 64 and Documentation Demo Program #

I added some commands to Commodore 64 BASIC via software extensions
written in 6502 Assembly.

Notable features
* Multiple hi-res screen support (up to 5) and swap between them
* Two hi-res screens and color tables using 18K RAM under ROM and I/O
* Doesn't take away any BASIC RAM, only uses $C000-D423 extra RAM
* Copy shapes of any size between screen and memory
* HIRES, COLOR, PLOT, RECT, PATTERN, SHAPE commands
* Machine language interface included if BASIC isn't your thing
* [Scrolling BASIC Editor](https://archive.org/details/1988-01-computegazette/page/n82) support (scroll with F1/F7)

Compiling requires [ACME](https://sourceforge.net/projects/acme-crossass/) for use with Microsoft Visual Code.  
Important: use instructions below with Esshahn/acme-assembly-vscode-template on how to install from Visual Code.

Also requires [Esshahn/acme-assembly-vscode-template](https://github.com/Esshahn/acme-assembly-vscode-template).
(just the windows binaries, other template files included here).

Build launches [VICE](http://vice-emu.sourceforge.net/index.html#download) C-64 Emulator so install that too.

And some manual editing of the development system and configuration files is required (e.g. ACME and VICE locations).  
Sorry! See build.sh for use within Visual Code or build.bat for building outside of Visual Code.

Additional work will be required for non-Windows platforms.

The introduction to this software is documented on my blog
[https://blog.davevw.com/](https://blog.davevw.com/2020/03/new-hi-res-graphics-for-commodore-64.html)

[C64 disk image](https://github.com/davervw/hires-c64/raw/master/build/hires.d64)
