; hires.asm
;
; Commodore 64 High Resolution Graphics Functions
; ported to 6502 by Dave Van Wagner (davevw.com)
;
; original graphics on/off and plot from BASIC program by Paul Soper
;   https://paulnotebook.net/2019/06/08/plotting-a-function-in-hi-res-graphics-mode-on-a-commodore-64/
;   who also credits https://archive.org/details/The_Graphics_Book_for_the_Commodore_64
;
; I credit my research to the following
;   https://archive.org/details/Compute_s_Mapping_the_Commodore_64
;   https://archive.org/details/c64-programmer-ref
;   https://www.pagetable.com/c64disasm/
;   https://dustlayer.com/c64-architecture/2013/4/13/ram-under-rom
;   http://www.zimmers.net/cbmpics/cbm/c64/vic-ii.txt (see 2.4.2 Memory map as seen by the VIC)
;
; And there is compatibility built in with my ScrollEdit program published in 1988 (written in 1987)
;   https://archive.org/details/1988-01-computegazette/page/n82
;
; original keyword extensions by Dave Van Wagner
;
; DVW added line graphics and shape routines (note: BASIC algorithm shown in comments)
; and moved graphics to E000 under ROM and moved color ram to DC00 user ram
; Implemented BASIC enhanced commands and functions
; then implemented support for multiple graphic screens and drawing on non-active screen
; added patch for compatibility with Scrolling BASIC Editor
;
; KNOWN ISSUES
; 1. Running program multiple times, sometimes just does BRK - resets screen with READY.
;
; HIRES 0 - off, text
; HIRES 1 - on, no clear
; HIRES 1 CLR - on, w/ clear
; HIRES 1[,gaddr[,caddr][CLR][PLOT]] - set graphics plotting address(es), copy old to alternate.  If PLOT specified, do not switch
; HIRES 1,gaddr1[,caddr1]] SWAP gaddr2[,caddr2] - new syntax to allow specifying swap address(es)
; HIRES 1 SWAP - swap graphics screen between primary and alternate - fast switching or copying bytes depending on configuration
; HIRES 1 PUT - like SWAP but only copy *to* alternate screen
; HIRES 1 GET - like SWAP but only copy *from* alternate screen
; HIRES 0 PLOT - copy text screen to graphics screen
; COLOR 0, fg, bg, bd - resolution mode, foreground, background, border (all optional. resolution mode defaults to 0/text)
; COLOR 0, fg, bg @ row, col
; COLOR 1, fg, bg, bd - change colors on hires screen
; COLOR 1, fg, bg @ x, y - change pixel 8x8 cell colors
; COLOR 1, fg, bg @ x1, y1 TO x2, y2 - change pixel 8x8 cell colors for region of screen
; COLOR ,,,3 - change bg color
; PLOT 1, x, y - set pixel
; PLOT 0, x, y - clear pixel
; PLOT 1 @ x,y - alternate syntax
; PLOT 1 TO x2,y2
; PLOT 1,x1,y1 TO x2,y2
; PLOT 1,x1,y1 TO x2,y2 TO x3,y3 TO x4,y4 TO ...
; PLOT 1 TO x2,y2 TO x3,y3 TO ...
; PLOT 1,x1,y1,x2,y2 ... - alternate syntax
; SHAPE PUT addr, x1,y1 TO x2,y2
; SHAPE PUT OR addr, x1,y1 TO x2,y2
; SHAPE PUT XOR addr, x1,y1 TO x2,y2
; SHAPE PUT AND addr, x1,y1 TO x2,y2
; SHAPE OR addr, x1,y1 TO x2,y2
; SHAPE XOR addr, x1,y1 TO x2,y2
; SHAPE AND addr, x1,y1 TO x2,y2
; SHAPE NOT addr, x1,y1 to x2,y2
; SHAPE GET addr,x1,y1,x2,y2
; SHAPE GET addr, x1, y1 TO x2,y2
; PATTERN addr @ x1,y1 TO x2,y2 - from pattern stored in memory
; COLOR 0, fg @ r1, c1 TO r2, c2 - change text color for region of screen
; PLOT 1,"ABC",x1,y1 - note CHR$(14) lowercase, CHR$(18) reverse on, +128 to turn options off
; PLOT COLOR [fg][,bg] - temporarily color everything plotted, shape, and pattern, or clear options if no colors specified
; PLOT 0,"ABC",x1,y1 - should remove image of letters from screen - equivalent to OR+XOR(NOT)
; RECT pixel,x1,y1 TO x2,y2
; RECT pixel @ x1,y1 TO x2,y2
;
; case 0 - init variables, set vectors, etc.
; case 1 - graphics on
; case 2 - set graphic memory address to predefined
; case 3 - set color ram and initialize fg/bg cell colors
; case 4 - graphics clear
; case 5 - graphics off 
; case 6 - plot point
; case 7 - locate
; case 8 - drawto
; case 9 - shape size
; case 10- shape get
; case 11- shape/pattern put
; case 12- screen swap
; case 13- draw character at pixel location
; case 14- set fg/bg 8x8 cell color at pixel location
; case 15- text to graphics

; MEMORY MAP
; 0000-00FF Zero Page (0.25K)
; 0100-01FF Stack (0.25K)
; 0200-03FF BASIC/Kernal Usage (0.5K)
; 0400-07FF Text Screen (1K)
; 0800-9FFF BASIC Program/Variables (38K)
; A000-BFFF BASIC ROM // Hires bitmap SWAP (8K)
; C000-CFFF Hires ML (4K)
; D000-DBFF I/O and Text Color D800-DBFF // Character ROM D000-D7FF // Available RAM (except D800-DBFF 1K Hires SWAP color RAM) (3K)
; DC00-DFFF Hires color RAM (1K)
; E000-FFFF Kernal ROM // Hires bitmap (8K)

start=$c000 ; machine language org
;sc=$d800    ; graphics mode swap color ram (RAM Bank)
border=$d020; border color setting
scroly=$d011; and enable graphics
scrolx=$d016; and control register
vmcsb=$d018 ; vic-ii mem ctrl reg
syntax_error=$af08 ; BASIC ROM
illegal_quantity=$b248 ; BASIC ROM
chkcls=$aef7 ; checks for $29
chkopn=$aefa ; checks for $28
chkcom=$aefd ; checks for $2c
chkany=$aeff ; checks for character in accumulator
getbytc=$b79b ; parse byte expression from BASIC input
getnum=$b7eb ; 16-bit [$14,$15] comma 8-bit [x]
getadr=$b7f7 ; getadr - convert fp to 2 byte integer
frmnum=$ad8a ; evaluate expression, check data type
frmevl=$ad9e ; evaluate expression
listchr=$ab47 ; output a character
ayint=$b1bf ; convert fp to signed int

;zero page memory usage
arg1=$fb
arg2=$fc
arg3=$fd
arg4=$02
ptrl=$fe
ptrh=$ff

; "hires ml"
* = start
        sta arg1
        stx arg2
        sty arg3
        php
        pla
        sta arg4 ; store register flags as 4th argument

        cld ; clear decimal flag just in case
        ;cli ; clear interrupt flag just in case

        lda arg1
        cmp #0
        beq case0
        jmp try1

option_plotting !byte 0
option_alternate !byte 0
option_apply_color !byte 0
option_colors_0 !byte 0
option_colors_1 !byte 0
option_colors_2 !byte 0
option_colors_3 !byte 0
option_colors_4 !byte 0
option_colors_5 !byte 0
option_colors_6 !byte 0
option_colors_7 !byte 0

case0 ; init
        sta option_apply_color
        lda hiresfg
        lda hiresbg

        lda #<basic_error
        sta $300
        lda #>basic_error
        sta $301

        lda #<hires_crunch
        sta $304
        lda #>hires_crunch
        sta $305

        lda #<list_tokens
        sta $306
        lda #>list_tokens
        sta $307

        lda #<execute
        sta $308
        lda #>execute
        sta $309

        lda #$e0
        sta option_plotting
        lda #$a0
        sta option_alternate

        lda #0 ; not available
        sta option_colors_0
        sta option_colors_4
        sta option_colors_6
        lda #$0C
        sta option_colors_1
        lda #$60
        sta option_colors_2
        lda #$5C
        sta option_colors_3
        lda #$D8 ; should switch to $8C (or $80, $84, $88) for hardware supported color address
        sta option_colors_5
        lda #$DC
        sta option_colors_7

        jsr scrledit_patch

        lda #<no_interrupt
        sta $fffa
        sta $fffc
        lda #>no_interrupt
        sta $fffb
        sta $fffd

        jmp crunch_patch

no_interrupt ; just return
        rti

try1    cmp #$01
        bne try2

; case 1 - init graphics
        lda scroly
        ora #$b0 ; set bits 7,5,4
        sta scroly
        lda scrolx
        and #$ef ; turn off bit 4
        sta scrolx
        rts

try2
        cmp #$02
        bne try3

        ; case 2 - set graphic ram area
        lda option_plotting
        sta arg1
        lda $dd00
        lsr             ; shift out bit0
        lsr             ; shift out bit1
        asl arg1        ; c=hires address bit 7
        rol             ; shift in bit1
        asl arg1        ; c=hires address bit 6
        rol             ; shift in bit0
        eor #$03        ; toggle bits for vic-ii
        pha

        lda option_plotting
        jsr hires_to_color
        and #$3C        ; valid bits for color address
        asl
        asl             ; convert to $00..$70, high nibble is 1K bank for color
        asl arg1        ; c=hires address bit 5
        bcc +
        ora #$08
+       sta vmcsb

        pla
        sta $dd00 ;switch to vic-ii bank associated with hires address

        rts

bankram
        sei      ; disable interrupts
        lda $01
        and #$f8 ; mask out bits 0,1,2
        sta $01  ; a000-ffff is now ram, no i/o
	rts

banknorm
        lda $01
        ora #$07        ; a000-ffff is back to defaults
        sta $01
        cli
	rts

try3
        cmp #$03
        bne try4

        ; case 3 - set color ram
	jsr bankram
	jsr case3
	jmp banknorm

try4
        cmp #$04
        bne try5

        ; case 4 - clear graphic ram
	jsr bankram
	jsr case4
	jmp banknorm

try5
        cmp #$05
        bne try6

; case 5 - turn graphics off
        lda scroly
        and #$9f ; clr bits 5/6
        sta scroly
        lda scrolx
        and #$ef ; clr bit 4
        sta scrolx
        lda #$14
        sta vmcsb
        lda $dd00
        ora #$03  ; set bits 0,1
        sta $dd00 ;switch to vicii bank 0
        rts

try6
        cmp #$06
        beq is6
        jmp try7

is6
	jsr bankram
        jsr case6
        jmp banknorm

xyaddr ; given coordinates in x1lo, x1hi, y1 calculate ptrl/ptrh on screen

        lda arg1 ; we're gonna trash arg1,arg2 so save original values on stack
        pha
        lda arg2
        pha

        lda y1
        and #$f8 ; int(yc/8)*8
        sta arg1
        lda #$00
        sta arg2
        asl arg1 ; *2 more (=*16)
        rol arg2
        asl arg1 ; *2 more (=*32)
        rol arg2
        asl arg1 ; *2 more (=*64)
        rol arg2
        ldx #5  ; setup *5 more (=*320)
        lda #$00
        sta ptrl ; zero out result
        sta ptrh

mult5
        clc
        lda ptrl
        adc arg1
        sta ptrl
        lda ptrh
        adc arg2
        sta ptrh
        dex
        bne mult5 ; loop until *320 done

        lda y1
        and #$07
        clc
        adc ptrl ; ptr += yc and 7
        sta ptrl
        lda ptrh
        adc #$00
        sta ptrh

        lda x1lo
        and#$f8 ; 8*int(xc/8)
        clc
        adc ptrl  ; ptr += 8*int(xc/8)
        sta ptrl
        lda x1hi
        adc ptrh
        sta ptrh

        clc       ; add offset to graphics adapter
        lda ptrh
        adc option_plotting
        sta ptrh

        pla      ; restore original values of arg1,arg2 from stack
        sta arg2
        pla
        sta arg1

        rts

try7
        cmp #7
        beq case7
        cmp #8
        beq case8
        cmp #9
        bne +
        jmp case9
+       cmp #10
        bne +
        jmp case10
+       cmp #11
        bne +
        jmp case11
+       cmp #12
        bne +
        jmp case12
+       cmp #13
        bne +
        jmp case13
+       cmp #14
        bne +
        jmp case14
+       cmp #15
        bne +
        jmp case15
+       rts ; default case - do nothing

; line graphics algorithm implemented by davevw
; from memory from the 80s, and it worked!
;
; 32000 xs=x2-x1:ys=y2-y1
; 32010 if abs(xs) < abs(ys) then 32100
; 32020 yc=y1:yf=0:xi=xs/abs(xs)
; 32030 for xc=x1 to x2 step xi
; 32040 gosub 30000 ; rem plot point xc,yc
; 32050 yf=yf+ys
; 32060 if (ys>0 and yf>=abs(xs)) then yc=yc+1:yf=yf-abs(xs)
; 32070 if (ys<0 and yf<=-abs(xs)) then yc=yc-1:yf=yf+abs(xs)
; 32080 next xc
; 32090 return
; 32100 xc=x1:xf=0:yi=ys/abs(ys)
; 32110 for yc=y1 to y2 step yi
; 32120 gosub 30000 ; rem plot point xc,yc
; 32130 xf=xf+xs
; 32140 if (xs>0 and xf>=abs(ys)) then xc=xc+1:xf=xf-abs(ys)
; 32150 if (xs<0 and xf<=-abs(ys)) then xc=xc-1:xf=xf+abs(ys)
; 32160 next yc
; 32170 return

case7 ; locate x1, y1
        lda arg2
        sta x1lo
        lda arg3
        sta y1
        lda arg4
        and #$01
        sta x1hi
        rts

case8 ; draw line to x2, y2
        lda arg2 ; store arguments at x2, y2
        sta x2lo
        lda arg3
        sta y2
        lda arg4
        and #$01
        sta x2hi

	jsr bankram
        jsr line
	jmp banknorm
        ; note x1,y1 should match x2,y2 at this point

; test code for shape size
; (dependency: locate)
; 33000 rem locate x1,y1
; 33001 poke 780,9:poke 781,x1 and 255:poke 782,y1:poke 783,x1/256:sys ml
; 33002 return
; 35000 rem size shape to x2,y2
; 35001 poke 780,9:poke 781,x2 and 255:poke 782,y2:poke 783,x2/256:sys ml
; 35002 sz=peek(781)+256*peek(782)
; 35003 return
; ml=49152:x1=0:y1=0:x2=319:y2=199:gosub 33000:gosub 35000:print sz

case9                   ; shape size
        lda arg2
        sta x2lo
        lda arg3
        sta y2
        lda arg4
        and #$01
        sta x2hi

case9c  ldx x2lo        ; increment x2 one pixel for size
        ldy x2hi        ; in registers only so don't affect values
        inx
        bne +
        iny
+       sec
        txa             ; (x2+1)lo
        sbc x1lo
        sta xslo
        tya             ; (x2+1)hi
        sbc x1hi
        sta xshi
        bmi sizeinvalid
        ora xslo
        beq sizeinvalid
        ldy y2
        iny             ; increment for size without affecting memory
        tya
        sbc y1
        sta yslo
        bcc sizeinvalid
        beq sizeinvalid
        clc
        lda xslo
        adc #$07        ; round up to nearest 8 bits
        and #$F8        ; mask out lower bits
        ldx xshi        ; retrieve high byte
        bcc +
        inx             ; carry increases high byte
+       tay             ; save A
        txa             ; retrieve high byte
        lsr             ; transfer high bit into carry
        tya             ; retrieve A
        ror             ; /2 with high bit rotating in
        lsr             ; /2
        lsr             ; /2
        tax
        ldy yslo
        jsr multxy      ; xy=x*y
        rts             ; have answer
sizeinvalid
        lda #$00
        tax
        tay
        rts

case10  ; shape get to (X2,Y2), see locate and shape size, input dst ptr
        ; create multiply function to position to (X1,Y1) address ptr -> src
        ; shift = (X1 AND 7)
        ; right_mod = ((X2+1-X1) AND 7);
        ; right_mask = (255 << (8-right_mod)) AND 255;
        ; // 0:255, 1:128, 2:192, ..., 7:254
        ; columns = int((x2+1-x1+7)/8)
        ; do
        ; {
        ;   ys = y2+1-y1;
        ;   do
        ;   {
        ;     *dst = (*src << shift) | ((*(src+8) << shift) >> 8)
        ;     if (columns == 1)
        ;       *dst = *dst & right_mask;
        ;     ++dst;
        ;     ++src;
        ;     if ((src & 7) == 0)
        ;        src += 312;
        ;   } while (--ys > 0);
        ;   x1 += 8;
        ;   src = xyaddr(x1, y1)
        ; } while (--columns > 0);

        jsr case9c      ; get shape size, verify not invalid
        cpx #$00
        bne +
        cpy #$00
        bne +
        sta ptrl        ; wipe out source address
        sta ptrh
        ldx ptrl        ; return null address end to signal error
        ldy ptrh
        rts             ; FAILED

+       jsr xyaddr      ; get address of (X,Y)

        lda x1lo
        and #$07
        sta arg1        ; shift

        ldx x2lo
        inx
        txa
        sec
        sbc x1lo
        and #$07
        tax
        lda #$ff
        cpx #$00
        beq +
        lda #0
-       sec
        ror
        dex
        bne -
+       sta arg4        ; right_mask

        ldy x2hi        ; calculate x distance
        ldx x2lo
        inx
        bne +
        iny
+       sec
        txa
        sbc x1lo
        sta xslo
        tya
        sbc x1hi
        sta xshi

        clc             ; round up to full byte
        lda xslo
        adc #7
        and #$F8
        sta xslo
        bcc +
        inc xshi
+

        lsr xshi        ; divide x distance by 8, result fits in byte
        ror xslo
        lsr xslo
        lsr xslo

        ldy y2          ; calculate y distance
        iny
        tya
        sec
        sbc y1
        sta yslo
        sta yshi

	jsr bankram

                        ; for y=Y1 to Y2 (in count only)
                        ; for x=X1 to X2 step 8 (in count only)
        ldy #$00        ; clear pointer offset

--      lda (ptrl),y    ; retrieve this column
        sta fraclo      ; left column
        ldy #8          ; next column is only 8 bytes away
        lda (ptrl),y    ; retrieve from next column to right
        sta frachi      ; right column
        ldx arg1        ; retrieve shift count again for loop
        beq +
-       asl frachi      ; shift right bits, direction left
        rol fraclo      ; rotate carry into left bits, direction left
        dex
        bne -           ; repeat
+       lda fraclo      ; get shifted bits for this column
        ldx xslo
        cpx #1          ; last column?
        bne +           ; no - so skip
        and arg4        ; and right_mask
+       ldy #0          ; clear pointer offset
        sta (arg2),y    ; store resulting data

        inc ptrl        ; ++ptr
        bne +
        inc ptrh
+       lda ptrl

        and #$07        ; (ptr & 7) == 0) ?
        bne +           ; branch if no
        clc             ; yes - ptr += 312
        lda ptrl
        adc #<312
        sta ptrl
        lda ptrh
        adc #>312
        sta ptrh

+       inc arg2        ; ++dst
        bne +
        inc arg3

+       dec yslo        ; next y
        bne --

        clc             ; x1 += 8
        lda x1lo
        adc #8
        sta x1lo
        bcc +
        inc x1hi

+       jsr xyaddr     ; get next src addr into ptr

        lda yshi        ; restore y distance
        sta yslo

        dec xslo        ; next x column (always a byte)
        bne --

        lda arg2        ; put end of buffer in x,y registers
        tax
        lda arg3
        tay

	jmp banknorm

case11  ; shape put to (X2,Y2), see locate and shape size, input src ptr
        ; create multiply function to position to (X1,Y1) address ptr -> dst
        ; shift = (X1 AND 7);
        ; left_mask = 255 >> shift;
        ; right_mask = fn(X2 AND 7) = 0:128,1:192,2:224,3:240,4:248,5:252,6:254,7:255
        ; columns = int((x2+1-(x1 and 248)+7)/8); // screen columns
        ; if (columns == 1)
        ; {
        ;   left_mask = left_mask and right_mask;
        ;   right_mask = left_mask;
        ; }
        ; do
        ; {
        ;   ys = y2+1-y1;
        ;   do
        ;   {
        ;     if (left_mask != 255)
        ;       data = (*src >> shift); // left-most column
        ;     else
        ;       data = ((*(src-(y2+1-y1)) << 8) >> shift) | (*src >> shift); // other
        ;     if (columns == 1)
        ;       *dst = *dst & (^right_mask) | (data & right_mask);
        ;     else if (left_mask = 255)
        ;       *dst = data;
        ;     else
        ;       *dst = *dst & (^left_mask) | (data & left_mask);
        ;     ++dst;
        ;     if ((dst & 7) == 0)
        ;        dst += 312;
        ;     ++src;
        ;   } while (--ys > 0);
        ;   x1 += 8;
        ;   left_mask = 255;
        ;   dst = xyaddr(x1, y1)
        ; } while (--columns > 0);

        lda #0
        sta pattern_flag ; *NOT* PATTERN
case11b
        lda arg4        ; flags
        and #$43        ; mask to flags we care about
+       sta frachi      ; save shape mode: 0x00=normal, 0x01=or, 0x02=and, 0x03=eor, 0x04=not

        jsr case9c      ; get shape size, verify not invalid
        cpx #$00
        bne +
        cpy #$00
        bne +
        sta ptrl        ; wipe out source address
        sta ptrh
        rts             ; FAILED

+       jsr xyaddr      ; get address of (X,Y)

        lda y1
        sta case14_yslo ; save copy of y1 in case applying color later
        lda x1hi
        sta case14_xshi ; save copy of x1hi in case applying color later
        lda x1lo
        sta case14_xslo ; save copy of x1lo in case applying color later
        and #$07
        sta arg1        ; shift

        lda x2lo        ; calculate right_mask, based solely on x2 bit position
        and #$07
        tax
        inx
-       sec
        ror
        dex
        bne -
+       sta arg4        ; right_mask

        lda x1lo        ; include full left column screen byte
        and #$f8        ;   so will count all screen columns necessary for shape put
        sta x1lo

        ldy x2hi        ; calculate x distance
        ldx x2lo
        inx
        bne +
        iny
+       sec
        txa
        sbc x1lo
        sta xslo
        tya
        sbc x1hi
        sta xshi

        clc             ; round up to full byte
        lda xslo
        adc #7
        and #$F8
        sta xslo
        bcc +
        inc xshi
+

        lsr xshi        ; divide x distance by 8, result fits in byte
        ror xslo
        lsr xslo
        lsr xslo

        ldy y2          ; calculate y distance
        iny
        tya
        sec
        sbc y1
        sta yslo
        sta yshi

        lda #$ff        ; calculate left_mask
        ldx arg1        ; shift
        beq +
-       lsr
        dex
        bne -
+       sta xshi        ; left_mask

        ; if one column, left_mask and right_mask need combining into one mask
        ldx xslo        ; columns
        cpx #$01        ; 1?
        bne +           ; no, skip this
        and arg4        ; yes, combine with right_mask
        sta arg4        ; store right_mask
        sta xshi        ; store left_mask

+       sei
        lda $01
        and #7
        cmp #7
        beq +
        lda $01         ; non-default bank already selected (e.g. char rom visible)
        and #$fd        ; mask out only bit 1
        sta $01         ; make sure e000-ffff is now ram, no i/o
        jmp ++
+       lda $01
        and #$f8        ; mask out bits 0,1,2
        sta $01         ; all 64k ram

++      lda #01
        sta incr        ; record that this is the first column

                        ; for y=Y1 to Y2 (in count only)
                        ; for x=X1 to X2 step 8 (in count only)

        ldy #$00        ; clear pointer offset

--      bit pattern_flag
        bvs .pattern_case

        ;shape case
        lda (arg2),y    ; retrieve shape data from memory
        ldx incr        ; left-most column flag? simple case, no data to our left
        beq +           ; no - skip to general case
        ldx arg1        ; shift count
        beq ++          ; simple case - no shift, branch to store
-       lsr             ; shift data right by count
        dex
        bne -           ; repeat until done
        beq ++          ; skip over general case

+       sta fraclo      ; left data
        ldy yshi        ; y size
        lda (arg2),y    ; load this column data
        ldy #0          ; reset index for pointers
        ldx arg1        ; shift count
        beq ++          ; done with this case
-       lsr fraclo      ; rotate left data
        ror             ; rotate carry into right data
        dex
        bne -           ; repeat until done
        beq ++          ; skip over pattern_case

.pattern_case
        lda ptrl
        and #$07
        tay
        lda (arg2),y    ; retrieve shape data from memory
        ldx incr        ; left-most column flag? simple case, no data to our left
        bne +
        and xshi
        beq .pattern_case_exit ; skip over general case

+       sta fraclo      ; left data
        lda (arg2),y    ; load next column data
.pattern_case_exit
        ldy #0          ; reset index for storage

        ; check if columns = 1 (last column)
++      ldx xslo        ; column
        cpx #1          ; last?
        bne +
        and arg4        ; right_mask
        sta fraclo
        lda arg4
        eor #$ff
        jmp ++

        ; check if general interior case, simple store
+       ldx xshi        ; left_mask
        cpx #$ff        ; left most?
        bne +           ; no, branch to left case
        sta fraclo      ; simple case, just store
        lda #$00
        jmp ++

        ; handle first column case where shift was required
+       and xshi        ; left_mask
        sta fraclo
        lda xshi
        eor #$ff

++      ldx frachi      ; check put mode
        beq ++          ; branch if zero
+       cpx #1
        bne +           ; branch if not or mode
        ; or mode
        lda (ptrl),y    ; keep all previous bits
        ora fraclo      ; combine with what to put
        sta (ptrl),y    ; store result
        jmp +++
+       cpx #2
        bne +
        ; and mode
        lda (ptrl),y    ; keep all previous bits
        and fraclo      ; combine with what to put
        sta (ptrl),y    ; store result
        jmp +++
+       cpx #3
        bne +           ; branch if not eor mode
        ; eor mode
        lda (ptrl),y    ; keep all previous bits
        eor fraclo      ; combine with what to put
        sta (ptrl),y    ; store result
        jmp +++
+       cpx #$40
        bne +++         ; branch if not not mode
        ; not/erase mode
        lda (ptrl),y    ; keep all previous bits
        ora fraclo      ; turn on bits
        eor fraclo      ; invert to erase
        sta (ptrl),y    ; store result
        jmp +++

        ; store mode
++      and (ptrl),y    ; mask what to keep
        ora fraclo      ; combine with what to put
        sta (ptrl),y    ; store result

+++     inc ptrl        ; ++ptr
        bne +
        inc ptrh
+       lda ptrl

        and #$07        ; (ptr & 7) == 0) ?
        bne +           ; branch if no
        clc             ; yes - ptr += 312
        lda ptrl
        adc #<312
        sta ptrl
        lda ptrh
        adc #>312
        sta ptrh

+       bit pattern_flag
        bvs +           ; skip if pattern
        inc arg2        ; ++src
        bne +
        inc arg3

+       dec yslo        ; next y
        beq +
        jmp --

+       clc             ; x1 += 8
        lda x1lo
        adc #8
        sta x1lo
        bcc +
        inc x1hi

+       lda #$ff        ; interior - not left column anymore
        sta xshi        ; left_mask

        lda incr        ; check if was first/left column
        beq +
        dec incr        ; reset to zero
        bit pattern_flag
        bvs +           ; skip if pattern
        sec             ; reset source pointer to left most data by rewinding y distance bytes
        lda arg2        ;  because we are now going to shift two sets of data together
        sbc yshi
        sta arg2
        bcs +
        dec arg3        

+       jsr xyaddr      ; get next src addr into ptr

        lda yshi        ; restore y distance
        sta yslo

        dec xslo        ; next x column (always a byte)
        beq +
        jmp --

+       lda arg2        ; put end of buffer in x,y registers
        tax
        lda arg3
        tay

        bit option_apply_color
        bvc +

        jsr bankram     ; required in case was char rom
        jsr case11_apply_color

+	jmp banknorm    ; no matter what, go back to normal banking

pattern
        lda #$40        ; BIT $pattern_flag will set overflow
        sta pattern_flag ; YES PATTERN
        jmp case11b

case12           ; screen swap
        jsr bankram
        jsr screen_swap
        jmp banknorm

hires_to_color
        lsr
        lsr
        lsr
        lsr
        lsr
        tax
        lda option_colors_0,x
        rts

case13          ; draw character at prior set location, screen character X, high bit in Y
        txa
        pha             ; save x register to stack
        tya
        pha             ; save y register to stack

        ;               calculate (x2,y2) for character placment
        clc
        lda x1lo
        adc #7
        sta x2lo
        lda x1hi
        adc #0
        sta x2hi
        clc
        lda y1
        adc #7
        sta y2
        jsr case9c      ; call shape size operation
        cpx #0
        bne +
        cpy #0
        bne +
        tay             ; set Y to zero for high size byte
        pla             ; reclaim y register from stack
        pla             ; reclaim x register from stack
        ldx #0          ; set X to zero for low size byte
        rts             ; size failed

        ;               compute address of character in rom
        ;                 multiply character by 8, then add to $D000
+       pla             ; restore character high bit and mode from stack
        sta arg4        ; store in arg4 for mode
        and #$01        ; just need one bit for high bit of character
        tay             ; character high bit in Y
        pla             ; restore character low byte from stack
        sta arg1        ; store character into arg1
        tya             ; high byte moved to accumulator

        asl arg1        ; *2
        rol

        rol arg1        ; *2
        rol

        rol arg1        ; *2
        rol

        clc
        adc #$d0        ; character rom high byte
        sta arg2

        ;               put character shape
        sei
        lda $01
        ora #$01        ; a000-bfff ROM
        and #$f9        ; character ROM and e000-ffff RAM
        sta $01
        ldx arg1
        ldy arg2
        lda arg4
        jsr setnvzcflags; set shape mode to put normal (overwrite)
        lda #11
        jsr start       ; put shape and restore memory map defaults
	jmp banknorm    ; restore normal banking just in case

setnvzcflags ; set NV----ZC flags based on accumulator, same positioning as CPU
             ; not ---BDI-- flags
             ;  effectively saving other registers and flags
        php             ; save current flags at what will be 102+X
        pha             ; save A register at what will be 101+X
        txa
        pha             ; save X register at what will be 100+X
        tsx             ; put SP into X
        inx             ; adjust to point to last entry to stack
        lda $101,x      ; retrieve desired flags
        and #$c3        ; only what to change bits 7,6,1,0
        sta $101,x
        lda $102,x      ; retrieve original flags into accumulator
        and #$3c        ; mask bits that we don't want to change
        ora $101,x      ; combine with bits we want to change
        sta $102,x      ; store flags where we will retrieve later
        pla
        tax             ; restore X register
        pla             ; restore A register
        plp             ; retrieve new flags
        rts

case14  ;               change foreground/background color at located pixel (color in arg2, arg3 must be 1 for hires)
        lda x1lo
        sta case14_x2lo
        lda x1hi
        sta case14_x2hi
	lda y1
	sta case14_y2
        lda arg2
        sta case14_arg2
        lda arg3
        sta case14_arg1
        bne color_range

-      rts
color_range ; given x1,y1,x2,y2, color in arg2, set color for range of pixels (text:arg1=0, hires:arg1=1)
            ; note expects caller to validate x1,y1 in range
            ; but we'll validate that x2>=x1, y2>=y1 and in range for proper operation
            ; number of columns = ((x2 and $f8) - (x1 and $f8)) / 8 + 1
        sec             ; compute number of columns
        lda x1lo
        and #$f8
        sta case14_arg3
        lda case14_x2lo
        and #$f8
        sbc case14_arg3
        sta case14_xslo
        lda case14_x2hi
        sbc x1hi
        sta case14_xshi
        bcc -           ; exit if out of range
        lsr case14_xshi ; xshi = xshi / 2
        bne -           ; exit if out of range
        lda case14_xslo
        ror             ; = xshi remainder + xslo / 2
        lsr             ; /= 2 (/4 so far)
        lsr             ; /= 2 (/8)
        clc
        adc #1          ; necessary to add one
        cmp #41
        bcc +
	jmp ++          ; exit if out of range
+       sta case14_xslo ; store number of columns

        sec             ; compute number of rows
        lda y1
        and #$f8
        sta case14_arg4
        lda case14_y2
        and #$f8
        sbc case14_arg4
        bcc ++          ; exit if out of range
        lsr 
        lsr
        lsr
        clc
        adc #1
        cmp #26
        bcs ++          ; exit if out of range
        sta case14_yslo ; store number of rows

        ;               addr = gc + int(y1/8) * 40 + int(x1/8)
        lda x1hi
	lsr
	lda x1lo
        ror
        lsr
        lsr             ; int(x1/8)
        sta ptrl
        lda option_plotting
        jsr hires_to_color
        ldx case14_arg1
        bne +
        lda case14_arg2 ; text color is just foreground
        lsr
        lsr
        lsr
        lsr
        sta case14_arg2
        lda #$d8        ; text color screen (I/O bank)
+       sta ptrh
        ldx #5          ; prep *5
-       clc
        lda ptrl
        adc case14_arg4 ; int(y1/8)*8
        sta ptrl
        bcc +
        inc ptrh
+       dex
        bne -

        lda case14_arg1
        beq +           ; no need to switch banks if text color ram
        lda $01         ; save banks
        pha
	jsr bankram     ; ensure 64k ram bank
+        

--      ldy #0
        ldx case14_xslo
        lda case14_arg2
-       sta (ptrl),y
        iny             ; advance to next column
        dex
        bne -           ; loop for columns
        clc             ; advance to next row
        lda ptrl
        adc #40
        sta ptrl
        bcc +
        inc ptrh 
+       dec case14_yslo
        bne --          ; loop for rows

        lda case14_arg1
        beq ++          ; no need to switch banks if text color ram
        pla
        tax
        and #$07        ; check if previous bank
        cmp #$07        ; ...was normal
        bne +           ; nope, no cli
        cli
+       stx $01         ; restore banks to before
++	rts

case15 ; copy text screen to graphics screen, and copy color too
        ; initialize variables
        lda #0
        sta copyx
        sta copyx+1
        sta copyy
        sta copyptr
        lda #$04 ; text ram high byte
        sta copyptr+1
        lda #<(40*25)
        sta copycount
        lda #>(40*25)
        sta copycount+1

        ; locate
--      ldx copyx
        ldy copyy
        lda copyx+1
        lsr             ; move x high bit into C
        lda #7          ; locate op
        jsr start

        ; copy char from text to graphics screen
        lda copyptr
        sta ptrl
        lda copyptr+1
        sta ptrh
        ldy #0
        lda (ptrl),y
        tax
        lda #13         ; draw char
        jsr start

        ; locate
        ldx copyx
        ldy copyy
        lda copyx+1
        lsr             ; move x high bit into C
        lda #7          ; locate op
        jsr start

        ; plot color
        lda copyptr
        sta ptrl
        clc
        lda copyptr+1
        adc #>($d800-$0400)        ; translate text screen to color screen
        sta ptrh
        ldy #0
        lda (ptrl),y
        asl
        asl
        asl
        asl
        sta arg1
        lda $d021
        and #$f
        ora arg1
        tax
        lda #14
	ldy #1 ; graphics screen
        jsr start

        clc
        lda copyx
        adc #8
        sta copyx
        bcc +
        inc copyx+1
+       lda copyx+1
        beq +
        lda copyx
        cmp #<320
        bcc +

        lda #0
        sta copyx
        sta copyx+1
        clc
        lda copyy
        adc #8
        sta copyy

+       inc copyptr
        bne +
        inc copyptr+1

+       ldy copycount
        bne +
        dec copycount+1
+       dey
        sty copycount
        tya
        ora copycount+1
        beq +
        jmp --
+       rts

; this is a high performance multiplier, doing just shifts and adds, loops only 8 times
multxy ; multiplies x*y, 16-bit result in xy (y is high byte), using stack for temporaries, code is relocatable
        cld             ; just in case
        pha             ; A saved
        lda #$00
        pha             ; result hi     @ $0105+SP
        pha             ; result lo     @ $0104+SP
        pha             ; multiplier hi @ $0103+SP
        tya             ; y is multiplier lo
        pha             ; multiplier lo @ $0102+SP
        txa             ; retrieve value x
        pha             ; value x       @ $0101+SP
        tsx             ; x = SP stack pointer

        ldy #$08        ; nbits = 8
-       lsr $0101,x     ; get next bit of value x
        bcc +           ; if clear, skip add

        clc             ; bit set, so result += multiplier
        lda $0104,x     ; load result lo
        adc $0102,x     ; add mult lo
        sta $0104,x     ; store result lo
        lda $0105,x     ; load result hi
        adc $0103,x     ; add with carry mult hi
        sta $0105,x     ; store result hi

+       asl $0102,x     ; multiplier *= 2
        rol $0103,x

        dey             ; --nbits
        bne -           ; branch loop if nbits != 0

                        ; give back stack space
        pla             ; $0101+SP
        pla             ; $0102+SP
        pla             ; $0103+SP
        pla             ; $0104+SP
        tax             ; X = result lo
        pla             ; $0105+SP
        tay             ; Y = result hi
        pla             ; A restored

        rts

list_tokens
        bit $0F   ; quoted?
        bmi +     ; if yes, handle normally in ROM
        cmp #$cc  ; compare to our first token value
        bcc +     ; skip token if less than ours
        cmp #$d5  ; compare past our last token value
        bcc ++    ; branch if our token
+       ora #$00  ; reset Z flag for zero value
        jmp $a71a ; process other token standard QPLOP
++      sty $49   ; save index
        ldy #0
        sec
        sbc #$cc
        tax
        beq +
-       lda tokens1,y
        iny
        ora #0
        bpl -
        dex
        bne -
-
+       lda tokens1,y
        bmi +
        jsr listchr
        iny
        jmp -
+       and #$7f
        jsr listchr ; output character
        ldy $49   ; restore index
        jmp $a700 ; retrieve next token

hires_crunch ; will be copy/patch of C64 BASIC crunch from A57C-A612
!byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
!byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
!byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
!byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
!byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
!byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
!byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
!byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
!byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
!byte 0,0,0,0,0,0,0

patch_table
!byte 0x37, <crunch_start, >crunch_start
!byte 0x40, <crunch_sbc, >crunch_sbc
!byte 0x7e, <crunch_get, >crunch_get
!byte 0x83, <crunch_next, >crunch_next
!byte 0

crunch_patch
        ldy #0
-       lda $a57c,y
        sta hires_crunch,y
        iny
        cpy #$97
        bne -

        ldy #0
-       lda patch_table,y
        beq +
        tax
        lda #$20 ; JSR opcode
        sta hires_crunch,x
        inx
        iny
        lda patch_table,y
        sta hires_crunch,x
        inx
        iny
        lda patch_table,y
        sta hires_crunch,x
        iny
        bne -
+       rts

crunch_start:
        lda #<$A09E	; point to original BASIC tokens, low byte
        sta ptrl
        lda #>$A09E	; point to original BASIC tokens, high byte
        sta ptrh
        STX $7A
        DEX
        rts
crunch_get:		; retrieves character from token table, looking back one index
        dey
        lda (ptrl),y
        iny
        ora #$00 ; restore N based on A (caller will BPL next)
        rts
crunch_next:
        cpy #$FF ; are we at the end of the last token in the first table?
        bne + ; no
        lda #<tokens1 ; update low pointer to next table
        sta ptrl
        lda #>tokens1 ; update high pointer to next table
        sta ptrh
        iny ; reset index to zero, start of second token table
+       lda (ptrl),y        
        rts
crunch_sbc:
        sbc (ptrl),y
        rts

get_coord ; retrieve x,y coordinates from command into a(xlo), x(y), y(xhi)
        jsr getnum ; (16-bit [$14,$15] comma 8-bit [x])
chk_coord
        cpx #200
        bcs ++
        lda $14
        ldy $15
        beq +
        cpy #2
        bcs ++
        cmp #<320
        bcc +
++      jmp .illegal_quantity
+       rts

chktoken:               ; re-check last token parsed
        ldy #0
        lda ($7A),y
        sec
        beq + ; continue
        cmp #$3a ; colon
+       rts

execute:
        jsr $0073 ; get next token (wedge)
        beq loop ; end of line or colon
        bcc loop ; numeric

        cmp #$cc ; HIRES?
        bne +
        jmp hires
+	cmp #$cd ; COLOR?
        bne +
        jmp color
+       cmp #$ce ; PLOT?
	beq plot
+       cmp #$cf ; SHAPE?
        bne +
        jmp shape        
+       cmp #$d2 ; PATTERN?
        bne +
        jmp shape_get_put ; syntax is similar
+       cmp #$d4 ; RECT?
        bne +
        jmp rect
+       sec ; non-numeric
        ; not one of ours, continue with ROM processing
loop    jmp $a7e7 ; handle token

reloop  jsr chktoken ; verify next token is end of statement
        beq loop        
        jmp syntax_error ; syntax error

plot	jsr lookahead
        cmp #$CD        ; COLOR
        bne ++
        jsr $0073       ; consume token
        lda #0
        sta option_apply_color
        jsr lookahead
        bne +
        jsr $0073       ; consume token (end of line or colon)
        jmp reloop
+       jsr commaorbyte
        bcc +
        cpx #16
        bcs .illegal_quantity
        stx hiresfg
        lda #$40
        sta option_apply_color
+       jsr commaorbyte
        bcc +
        cpx #16
        bcs .illegal_quantity
        stx hiresbg
        lda #$40
        sta option_apply_color
+       jmp reloop

++      jsr getbytc     ; pixel state 0=clear, 1=set
        cpx #2
        bcs .illegal_quantity
        stx plotcolor
        cmp #$A4        ; TO token
        beq plotto
        cmp #$40        ; @
	bne +
	jsr chkany
	jsr get_coord
	jmp plot_coord1
+       jsr chkcom      ; comma separates values
        ; first coordinate before TO is locate
	jsr plotstring		; check if is plot string, if is, does not return
	; otherwise has called equivalent to get_coord
	;jsr get_coord
plot_coord1
        sta x1lo                ; locate
        sty x1hi
        stx y1
        jsr chktoken
        bne + 
        lda x1lo
        ldy x1hi
        ldx y1
        jmp ++ ; drawing from/to same coord with draw pixel
+       cmp #$A4
        beq plotto
        jmp syntax_error

.illegal_quantity
       jmp illegal_quantity ; invalid quantity error

plotto
        jsr $0073               ; get next token
        jsr get_coord
++      sta arg2                ; setup args for drawto
        stx arg3
        tya
        ldx plotcolor
        bne +
        ora #2 ; pixel mode clear
+       sta arg4
        jsr case8
        jsr chktoken
        bne plotto
        jmp reloop

hires   jsr getbytc
        cmp #0  ; end of line
        bne +
        jmp hiresX
+       cmp #':' ; colon
        bne +
        jmp hiresX

+       cmp #$d3 ; swap
        bne +
        lda #0
-       sta arg4
        jmp hires_swap

+       cmp #$a1 ; get
        bne +
        lda #$40 ; get option for swap
        bne -

+       cmp #$d0 ; put
        bne +
        lda #$80 ; put option for swap
        bne -

+       cmp #$ce ; plot
        beq hires_plot

        cmp #$9c	; CLR token
        bne +
        jmp hiresX

+       cmp #','
        bne +
        jmp hires_addr

+       jmp syntax_error

hires_plot
        jsr $0073
        beq +
        jmp syntax_error
+       cpx #0
        bne .illegal_quantity
        lda #15 ; text to graphics
        jsr start
        jmp reloop

hires_swap
        jsr $0073
        beq +
        jmp syntax_error
+       cpx #1
        beq do_swap
        jmp illegal_quantity

; 0 poke 56,8*16+12:clr
; 10 hires 1,10*4096,(8*16+12)*256 clr plot
; 15 hires 1,14*4096 clr
; 20 plot 1 @ 0,0 to 319,199
; 30 hires 1 put
; 40 plot 1 @ 0,199 to 319,0
; 50 hires 1 swap
; 60 for i=1 to 100:next
; 70 goto 50
do_swap
        lda arg4
        bne +                   ; GET or PUT, so just copy bytes, don't switch screens
        lda option_alternate    ; check if alt screen is physical hires screen
        jsr chk_phys_screen     ; check if hires bitmap and color screen valid and in same 16k segment
        bne +                   ; nope -- bitmap/color not in same 16K page

        ; swap plotting/alternate, and activate new screen
        ldy option_alternate
        lda option_plotting
        sta option_alternate
        sty option_plotting
-       lda scroly
        bpl -                   ; wait for scan line out of visible area
        lda #1
        jsr start
        lda #2
        jsr start
        jmp reloop

+       lda arg4
        pha
        lda #12 ; swap graphics screens
        ldx #0
        ldy option_alternate
        plp
        jsr start
        jmp reloop

chk_phys_screen
        pha
        txa
        pha
        tya
        pha
        tsx
        lda #1
        sta chk_phys_temp
        lda $103,x              ; retrieve A from stack
        tay                     ; save copy in Y
        beq +                   ; nope - zero not allowed
        and #$1F
        bne +                   ; nope - offset not right
        tya
        cmp #$80
        beq +                   ; nope - $8000 not allowed
        and #$C0                ; mask to 16K page
        sta chk_phys_temp
        tya
        jsr hires_to_color
        and #$C0
        sec
        sbc chk_phys_temp
        sta chk_phys_temp
+       pla
        tay
        pla
        tax
        pla
        lda chk_phys_temp       ; discard A, set Z for return value
        rts

hiresX
        cpx #0 ; HIRES 0
        bne +
-       lda scroly
        bpl -           ; make sure raster is in non-drawable part of screen
        lda #5
        jsr start
        jmp reloop

+       cpx #1 ; HIRES 1
        beq +
        jmp .illegal_quantity
+	jsr chktoken
	beq +
	lda #$9c        ; CLR token
	jsr chkany      ; check for token in A and advance

        ; clear hires screen first
        lda #3
        jsr gethirescolor ; in X without changing A
        jsr start       ; set graphics color ram
        lda #4
        jsr start       ; clear graphics screen

+       jmp switch_hires

; 100 poke 56,32:clr
; 105 goto 130
; 110 hires 1,8192,1024 clr
; 120 plot 1,0,0 to 319,199
; 130 hires 1,4*4096,6*4096 clr
; 140 plot 1,160,0 to 160,199
; 150 hires 1,10*4096,8*4096 clr
; 160 plot 1,319,0 to 0,199
; 165 stop
; 170 hires 1,14*4096 clr
; 180 plot 1,319,100 to 0,100
; 190 for i=0 to 1 step 0
; 200 hires 1,2*4096
; 210 hires 1,4*4096
; 220 hires 1,10*4096
; 230 hires 1,14*4096
; 240 get k$:i=len(k$)
; 250 next
; 260 hires 0:print chr$(147);
hires_addr
        jsr $0073 ; skip comma
        jsr frmnum
        jsr getadr
        cpy #0
        beq +
-       jmp illegal_quantity
+       cmp option_plotting
        beq +
        ldy option_plotting
        sty option_alternate
        sta option_plotting
+       jsr chktoken
        cmp #','
        bne +
        jsr chkcom
        jsr frmnum
        jsr getadr
        cpy #0
        bne -
        tay             ; save color high address
        lda option_plotting
        lsr             ; divide by 32 to get value 0..7
        lsr
        lsr
        lsr
        lsr
        tax             ; x has # of hires screen (0..7)
        tya             ; restore color high address
        sta option_colors_0,x
        jsr chktoken
+       cmp #$D3        ; SWAP
        bne +
        jsr $0073       ; consume SWAP
        jsr frmnum
        jsr getadr
        cpy #0
        bne -
        sta option_alternate
        jsr chktoken
        cmp #','
        bne +++
        jsr chkcom
        jsr frmnum
        jsr getadr
        cpy #0
        bne -
        tay             ; save color high address
        lda option_alternate
        lsr             ; divide by 32 to get value 0..7
        lsr
        lsr
        lsr
        lsr
        tax             ; x has # of hires screen (0..7)
        tya             ; restore color high address
        sta option_colors_0,x
+++     jmp do_swap
+       cmp #$9C        ; CLR
        bne +
        lda #3
        jsr gethirescolor ; in X without changing A
        jsr start
        lda #4
        jsr start
        jsr $0073       ; consume token
+       cmp #$CE        ; PLOT
        bne +
        jsr $0073       ; consume token
        jmp ++
+      
switch_hires
        lda option_plotting
        jsr chk_phys_screen
        beq +
        jmp illegal_quantity
+
-       lda scroly
        bpl -           ; wait for raster off screen before switch screen
        lda #1
        jsr start
        lda #2
        jsr start        
++      jmp reloop


; 10 hires 1 clr
; 20 x1%=rnd(1)*319
; 30 y1%=rnd(1)*199
; 40 x2%=rnd(1)*(320-x1%)
; 50 y2%=rnd(1)*(200-y1%)
; 60 rect rnd(1)*2,x1%,y1% to x2%,y2%
; 70 goto 20
rect    jsr getbytc
        stx rect_pixel
        jsr chktoken
        cmp #','
        beq +
        lda #'@'
+       jsr chkany
        jsr get_coord
        sta rect_x1lo
        sty rect_x1hi
        stx rect_y1
        lda #$A4
        jsr chkany
        jsr get_coord
        sta rect_x2lo
        sty rect_x2hi
        stx rect_y2

        jsr bankram
        jsr draw_rect
        jsr banknorm

        jmp reloop        

rect_pixel !byte 0
rect_x1lo !byte 0
rect_x1hi !byte 0
rect_y1 !byte 0
rect_x2lo !byte 0
rect_x2hi !byte 0
rect_y2 !byte 0

commaorbyte
        jsr lookahead ; (past comma)
        cmp #$00 ; end of line
        bne +
-       clc
        rts
+       cmp #$3a ; colon
        beq -
        cmp #$40 ; @
        beq -
        cmp #$2c ; comma
        bne +
        jsr $0073 ; get token
        bne ++
-       pla
        pla
        jmp syntax_error
++      clc
        ldx #0
        rts
+       jsr getbytc
        beq +
        cmp #$2c ; comma
        beq +
        cmp #$40 ; @
        bne -
+       sec
        rts

color
        jsr commaorbyte
        bcc ++
        cmp #$00
        bne +
        jmp syntax_error; no arguments
+       cpx #2 ; looking for 0 (text) or 1 (hires)
        bcc ++
-       jmp .illegal_quantity
++      stx arg2        ; graphics mode (0 or 1)
        lda #0
        sta arg1        ; whether to apply color to screen
        lda hiresfg
        sta arg3        ; save in case we need to restore
        lda hiresbg
        sta arg4        ; save in case we need to restore
        jsr commaorbyte
        bcc +
        cpx #16
        bcs -
        stx hiresfg
        inc arg1        ; set flag to apply fg color to screen
+       cmp #$40 ; @
        beq ++
+       jsr commaorbyte
        bcc +
        cpx #16
        bcs -
        stx hiresbg
        pha
        lda arg1
        ora #$02        ; set flag to apply bg color to screen
        sta arg1
        pla
+       cmp #$40 ; @
        beq ++
        jsr commaorbyte
        bcc +
        cpx #16
        bcs -
        stx $d020 ; border
+       cmp #$40
        bne .chkstor
++      jsr $0073
        jsr get_coord
        sta x1lo
        sty x1hi
        stx y1
        sta case14_x2lo
        sty case14_x2hi
        stx case14_y2
        jsr chktoken        
        beq ++
        cmp #$a4        ; TO token
        beq +
        jmp syntax_error
+       jsr $0073
        jsr get_coord
        sta case14_x2lo
        sty case14_x2hi
        stx case14_y2
++      jsr gethirescolor
        lda arg2        ; hires(1) or text(0)?
        sta case14_arg1
        stx case14_arg2
        bne +           ; skip multiply if hires
        ; lda x1hi
        ; ora x2hi
        ; bne .illegal_quantity
        ; lda x1lo
        ; cmp #40
        ; bcs .illegal_quantity
        ; lda x2lo
        ; cmp #40
        ; bcs .illegal_quantity
        ; lda y1
        ; cmp #25
        ; bcs .illegal_quantity
        ; lda y2
        ; cmp #25
        ; bcs .illegal_quantity
        ldx #3
-       asl x1lo        ; multiply x1,y1,x2,y2 all by 8
        rol x1hi        ;   to convert text coords to pixels
        asl case14_x2lo
        rol case14_x2hi
        asl y1
        asl case14_y2
        dex
        bne -
+       lda arg3
        sta hiresfg     ; restore global color, just change region
        lda arg4
        sta hiresbg     ; restore global color, just change region
        jsr color_range
        jmp reloop
.chkstor
        lda arg2
        beq .textcolor
        lda arg1
        beq +
        jsr gethirescolor
        lda #3
        jsr start
+       jmp reloop
.textcolor
        lda arg1
        lsr
        bcc +
        ldx hiresfg
        stx $286        ; text foreground
+       ldx arg3
        stx hiresfg     ; restore global hires color because this was for text
        lsr
        bcc +
        ldx hiresbg
        stx $d021       ; background
+       ldx arg4
        stx hiresbg     ; restore global hires color because this was for text
        jmp reloop

gethirescolor ; returns in X, doesn't change A
        pha
        lda hiresfg
        asl
        asl
        asl
        asl
        ora hiresbg
        tax
        pla
        rts

shape   jsr $0073
        beq +
        cmp #$d0 ; PUT?
        beq shape_get_put
        cmp #$d1 ; XOR?
        beq shape_get_put
        cmp #$af ; AND?
        beq shape_get_put
        cmp #$b0 ; OR?
        beq shape_get_put
        cmp #$a1 ; GET?
        beq shape_get_put
        cmp #$a8 ; NOT?
        beq shape_get_put
+       jmp syntax_error

shape_get_put
        sta arg1
        jsr $0073 ; skip GET/PUT
        jsr frmnum
        jsr getadr
        ldx $14
        ldy $15
        stx ptrl
        sty ptrh
        jsr chktoken
        cmp #$2c ; comma?
        beq +
        lda #$40 ; if not comma, must be @
+       jsr chkany ; check for token in A and advance
        jsr get_coord
        sta arg2
        stx arg3
        sty arg4
        jsr chktoken
        cmp #$2c ; comma?
        beq +
        lda #$a4; if not comma, must be TO
+       jsr chkany ; check for token in A and advance
        jsr get_coord
        sta x2lo
        sty x2hi
        stx y2
        lda arg2
        ldx arg3
        ldy arg4
        sta x1lo
        sty x1hi
        stx y1
        ldx ptrl
        ldy ptrh
        stx arg2
        sty arg3
        lda arg1
        cmp #$a1 ; GET
        bne +
        jsr case10 ; GET SHAPE
        jmp reloop
+       cmp #$d0 ; PUT
        bne +        
        lda #0

-       sta arg4
        jsr case11 ; PUT SHAPE        
        jmp reloop
                                ; which PUT is it?
+       cmp #$d1 ; XOR token
        bne +
        lda #$03 ; XOR mode
        bne -

+       cmp #$b0 ; OR token
        bne +        
        lda #$01 ; OR mode
        bne -

+       cmp #$af ; AND token
        bne +
        lda #$02 ; AND mode
        bne -

+       cmp #$a8 ; NOT token
        bne +
        lda #$40 ; NOT mode
        bne -

+       cmp #$d2 ; PATTERN token
        bne +
        lda #0
        sta arg4
        jsr pattern

+       jmp reloop

lookahead
        ldy #0
        lda ($7A),y
        beq +		; branch if end of line
	cmp #$3a	; colon
	beq +		; branch if end of statement
        lda $7A
        sta ptrl
        lda $7B
        sta ptrh
        jsr $0073
        php
        pha
        lda ptrl
        sta $7A
        lda ptrh
        sta $7B
        pla
        plp
+       rts

plotstring
	jsr frmevl	; evaluate expression
	bit $d		; string or numeric?
	bmi ++
	jsr getadr	; convert to integer
	ldx $14
	ldy $15
	stx arg1
	sty arg2
        jsr chktoken
        cmp #$2c        ; comma
        beq +
        jmp syntax_error
+       jsr getbytc
	lda arg1 
	ldy arg2
	jmp chk_coord	; finish equivalent to get_coord, will return to plot

++      jsr $b6a3	; pull string from descriptor stack (a=len, x=lo, y=hi addr of string)
        sta plot_len
        stx arg2
        sty arg3
	jsr chkcom      ; comma separates values
	jsr chktoken
	jsr get_coord
        sta x1lo
        sty x1hi
        stx y1
	sta plot_xlo
	sty plot_xhi
	stx plot_y
        lda plot_len
	cmp #$00
	bne +
        jmp ++++
+       ldx arg2
        ldy arg3
	stx $64
	sty $65

        lda #0
        sta charrvs
        sta charlow

-	ldy #0          ; draw next character
	lda ($64),y
	cmp #$0E
	bne +
	lda #1
	sta charlow
	jmp ++
+	cmp #$8E
	bne +
	lda #0
	sta charlow
	jmp ++
+	cmp #$12
	bne +
	lda #$80
	sta charrvs
	bne ++
+	cmp #$92
	bne +
	lda #$00
	sta charrvs
	beq ++	
+       cmp #$20
        bcc ++          ; skip if out of range
        cmp #$ff        ; pi?
        bne +++
        lda #$5e        ; convert to pi screen code
        bne +           ; display it
+++     cmp #$e0
        bcc +++         ; continue on if not e0..fe
        sbc #$80        ; convert to screen code
        bne +           ; display it
+++     cmp #$c0        ; check if in range c0..df
        bcc +++         ; continue on if not c0..df
        sbc #$80        ; convert to screen code
        bne +           ; display it
+++     cmp #$a0
        bcc +++         ; continue on if not a0..bf
        sec
        sbc #$40
        bne +           ; display it
+++     cmp #$80
        bcs ++          ; skip if out of range 80..9f
        cmp #$40
        bcc +           ; display if in range 20..3f
        cmp #$60
        bcs +++         ; branch if 60..7f
        sec             ; otherwise in range 40..5f
        sbc #$40        ; convert ASCII to screen code
        jmp +
+++     sbc #$20        ; convert to screen code
        bne +           ; display it
+	clc
	adc charrvs
	tax
	lda charlow
        ldy plotcolor
        bne +
        ora #$40        ; NOT mode
+       tay
	jsr case13      ; draw char

        clc             ; advance x, check for overflow
        lda plot_xlo
        adc #8
        sta plot_xlo
        sta x1lo
        tax
        lda plot_xhi
        adc #0
        sta plot_xhi
        sta x1hi
        beq +
        cpx #<313
        bcs ++++
+       lda plot_y
        sta y1

++	inc $64
	bne +
	inc $65
+	dec plot_len
	beq ++++
	jmp -
++++	pla		; remove return address from cpu stack
	pla
	jmp reloop

basic_error
        pha
        cmp #$81        ; READY
        beq +           ; skip if no error
        lda scroly      ; retrieve control register
        and #$20        ; hires active?
        beq +           ; branch if not hires
        txa
        pha
        tya
        pha
        lda #5
        jsr start
        pla
        tay
        pla
        tax

+       pla             ; restore all registers
        jmp $e38b ; IERROR - Print BASIC Error Message Routine

; variables

copyx !word 0
copyy !byte 0
copyptr !word 0
copycount !word 0

pattern_flag !byte 0

plotcolor !byte 0

plot_len !byte 0
plot_xlo !byte 0
plot_xhi !byte 0
plot_y   !byte 0

hiresfg !byte 14
hiresbg !byte 6

x1lo    !byte 0
x1hi    !byte 0
y1      !byte 0
x2lo    !byte 0
x2hi    !byte 0
y2      !byte 0
xslo    !byte 0
xshi    !byte 0
yslo    !byte 0
yshi    !byte 0
fraclo  !byte 0
frachi  !byte 0
incr    !byte 0

charlow	!byte 0
charrvs !byte 0

; coloring needs its own variables to not interfere with other uses
case14_xslo !byte 0
case14_xshi !byte 0
case14_yslo !byte 0
case14_arg1 !byte 0
case14_arg2 !byte 0
case14_arg3 !byte 0
case14_arg4 !byte 0
case14_x2lo !byte 0
case14_x2hi !byte 0
case14_y2 !byte 0

chk_phys_temp !byte 0

; 10 print chr$(147);
; 15 for i=0 to 1
; 20 for r=0 to 15
; 25 if i=0 then print spc(20);
; 30 for c=0 to 15
; 35 if i=0 and r<8 then:poke 1024+(r+17)*40+c,r*16+c
; 36 if i=0 and r>=8 then:poke 1024+(r+9)*40+c+20,r*16+c
; 37 if i=0 and ((r>=2 and r<8) or r>=10) then print chr$(r*16+c);
; 40 if i=1 then:plot 1,chr$(r*16+c),c*8,r*8
; 50 next c
; 54 if i=0 then print
; 55 next r
; 60 if i=0 then:printchr$(19):print:print"please wait...";:hires 0 swap:hires 1
; 70 next i
; 80 print "s";
; 90 for i=0 to 1 step 0:get k$:i=len(k$):next
; 100 hires 0

; 10 hires 1 clr
; 20 plot 1,"hires",rnd(1)*312,rnd(1)*192
; 30 goto 20

        ; C64 tokens are A09E-A19E
tokens1 !text "HIRE"            ; CC
            !byte 'S' OR $80
        !text "COLO"            ; CD
            !byte 'R' OR $80
        !text "PLO"             ; CE
           !byte 'T' OR $80
        !text "SHAP"            ; CF
            !byte 'E' OR $80
        !text "PU"              ; D0
          !byte 'T' OR $80
        !text "XO"              ; D1
          !byte 'R' OR $80
        !text "PATTER"          ; D2
           !byte 'N' OR $80
        !text "SWA"             ; D3
           !byte 'P' OR $80
        !text "REC"             ; D4
           !byte 'T' OR $80
        !byte 0                 ; end of table

scrledit_patch
        lda $2b
        cmp #$01
        bne +
        lda $2c
        cmp #$0d
        bne +
        lda #$b9
        ldx #$9e
        ldy #$a0
        cmp $0b2b
        bne +
        cpx $0b2c
        bne +
        cpy $0b2d
        bne +
        lda #$4c
        ldx #<scrledit_hook
        ldy #>scrledit_hook
        sta $0b1f
        stx $0b20
        sty $0b21
+       rts        

scrledit_hook
        SEC
        SBC #$7F
        CMP #$4C
        BCC +
        CMP #$56
        BCS +
        SBC #$4B
        PHA
        LDA #<tokens1
        LDX #>tokens1
-       STA $0B2C
        STA $0B34
        STX $0B2D
        STX $0B35
        PLA
        JMP $0B22
+       PHA
        LDA #$9E
        LDX #$A0
        BNE -

fix_x   lda x1hi
        beq +++
        cmp #>319
        beq +
        lda #>319
        sta x1hi
        bne ++
+       lda x1lo
        cmp #<319
        bcc +++
++      lda #<319
        sta x1lo
+++     rts
        
case3   lda option_plotting
        jsr hires_to_color
        sta ptrh

        lda arg2
        ldx #$04        ; 4 pages of 256 bytes
        ldy #0
        sty ptrl
        sty option_apply_color  ; clear option when initializing color
initvr
        sta (ptrl),y    ; set fg/bg of hires cell
        iny
        bne initvr
        inc ptrh
        dex
        bne initvr
	
	rts

case4   lda option_plotting
        sta ptrh
        lda #0
        sta ptrl
        ldx #$1F ; number of pages, note: one less than full 8K
        tay
loopclrg
        sta (ptrl),y
        iny
        bne loopclrg
        inc ptrh
        dex
        bne loopclrg

; clear only part of last page 31*256+64 = 8000
-       sta (ptrl),y
        iny
        cpy #$40
        bne -

	rts

case6 ; set/clr pixel
        lda arg2 ; xc lo
        sta x1lo
        lda arg3 ; yc
        sta y1
        lda arg4 ; xc hi and pixel color
        and #$01 ; isolate to xc hi
        sta x1hi
case6_from_line
        jsr xyaddr

        lda x1lo
        and #$07
        sta arg1
        lda #$07
        ldx #$01
        sec
        sbc arg1
        stx arg1
        tay
        cpy #$00
        beq shdone
findbit
        asl arg1
        dey
        bne findbit
        beq shdone

shdone
        lda arg4
        and #$02
        tax
        lda (ptrl),y    ; note y is guaranteed zero by dey branch above
        ora arg1
        cpx #$00        ; zero=set pixel, non-zero=clear pixel
        beq +
        eor arg1
+       sta (ptrl),y
        ; fall through to check_apply_color

; 10 color 1,14,6,14
; 20 hires 1 clr
; 30 plot color rnd(1)*16
; 40 plot 1,rnd(1)*320,rnd(1)*200 : rem or plot to
; 50 goto 30
check_apply_color
        bit option_apply_color ; check if need to apply color
        bvc +
        lda ptrl
        pha
        lda ptrh
        pha
        lda x1lo
        sta case14_x2lo
        lda x1hi
        sta case14_x2hi
	lda y1
	sta case14_y2
        lda #1
        sta case14_arg3
        sta case14_arg1
        jsr gethirescolor
        stx case14_arg2
        jsr color_range
        pla
        sta ptrh
        pla
        sta ptrl
+       rts

line
        jsr fix_x   ; in case overrun from previous shape/plot/pattern operation
        sec         ; xs(lope)=x2-x1
        lda x2lo
        sbc x1lo
        sta xslo
        lda x2hi
        sbc x1hi
        sta xshi
        sec         ; ys(lope)=y2-y1
        lda y2
        sbc y1
        sta yslo
        lda #$00    ;extend sign so 16-bit
        sbc #$00
        sta yshi
        ora yslo    ; xs == 0 && ys == 0 ?
        ora xshi
        ora xslo
        bne isline  ; no - is line
        jmp case6_from_line   ; yes- isdot
isline
        lda #$01
        sta arg2    ; arg2 (xs dir) = +1
        sta arg3    ; arg3 (ys dir) = +1
        lda xshi
        bpl xpos    ; branch if xs is pos.
        lda #$ff
        sta arg2    ; arg2 (xs dir) = -1
        sec         ; xs = abs(xs)
        lda #$00
        sbc xslo
        sta xslo
        lda #$00
        sbc xshi
        sta xshi
xpos
        lda yshi
        bpl ypos    ; branch if ys is pos.
        lda #$ff
        sta arg3    ; arg3 (ys dir) = -1
        sec         ; ys = abs(ys)
        lda #$00
        sbc yslo
        sta yslo
        lda #$00
        sbc yshi
        sta yshi
ypos

        lda yshi; ys(slope) >= xs(slope) ?
        cmp xshi
        bcc ylower  ; no - branch if lower
        beq chklows ; equal - cmp lo bytes
        jmp yhigher ; yes - absolute jump
chklows
        lda yslo
        cmp xslo
        bcc ylower  ; no - branch if lower
        jmp yhigher ; yes - absolute jump

ylower

        lda arg3    ; dir of ys(lope)
        bpl ypos2   ; positive, abs is ok
        sec         ; ys = -ys
        lda #$00
        sbc yslo
        sta yslo
        lda #$00
        sbc yshi
        sta yshi
ypos2

        lda arg2    ; dir of xs(lope)
        sta incr    ; used for step xi
        lda #$00    ; yfrac=0
        sta fraclo
        sta frachi

forx:    ; for x=x1 to x2 step xi

        lda x1lo    ; arg2=x1
        sta arg2
        lda y1
        sta arg3    ; arg3=y1
        lsr arg4    ; remove old x1 9th bit with shift right
        lda x1hi
        and #$01
        lsr         ; move x1 9th bit into carry
        rol arg4    ; rotate new x1 9th bit back in bit0, saving pixel color in bit1

        jsr case6   ; plot point

        clc         ; yfrac += ys(lope)
        lda fraclo
        adc yslo
        sta fraclo
        lda frachi
        adc yshi
        sta frachi

        lda yshi    ; ys(lope) < 0 ?
        bmi ysneg   ; yes - branch
        beq chkyslo ; zero? chk low bits
        bne yspos   ; non-zero
chkyslo
        lda yslo
        beq nextx   ; skip if zero
yspos
        lda frachi  ; yfrac >= abs(xs) ?
        cmp xshi
        bcc nextx  ; no - proceed to nextx
        bne incy1
        lda fraclo
        cmp xslo
        bcc nextx
           ; yes...
incy1
        inc y1      ; ++y1

        sec         ; yfrac -= abs(xs)
        lda fraclo
        sbc xslo
        sta fraclo
        lda frachi
        sbc xshi
        sta frachi

        clc         ; force carry clear
        bcc nextx   ; abs branch nextx

ysneg
        sec         ; yfrac <= -abs(xs) ?
        lda #$00
        sbc xslo
        sta ptrl
        lda #$00
        sbc xshi
        sta ptrh
        lda frachi
        cmp ptrh
        bcc nextx
        bne decy1
        lda fraclo
        cmp ptrl
        bcc nextx
decy1
        dec y1

        clc
        lda fraclo
        adc xslo
        sta fraclo
        lda frachi
        adc xshi
        sta frachi

nextx
        lda x1hi     ; x1 == x2 ?
        cmp x2hi
        bne stepx    ; no - so branch
        lda x1lo
        cmp x2lo
        bne stepx    ; no - so branch
        lda y2       ; reload y2 and restore in y1
        sta y1       ;   because have seen at -1
        rts          ; yes - line is done

stepx:      ; x += inc (+/- 1)
        ldx #$00     ; assume positive
        lda incr
        bpl xnotneg
        dex          ; extend sign to regx
xnotneg
        clc
        adc x1lo
        sta x1lo
        txa
        adc x1hi
        sta x1hi

bforx
        jmp forx

yhigher

        lda arg2    ; dir of xs(lope)
        bpl xpos2   ; positive, abs is ok
        sec         ; xs = -xs
        lda #$00
        sbc xslo
        sta xslo
        lda #$00
        sbc xshi
        sta xshi
xpos2

        lda arg3    ; dir of ys(lope)
        sta incr    ; used for step yi
        lda #$00    ; xfrac=0
        sta fraclo
        sta frachi

fory:    ; for y=y1 to y2 step yi

        lda x1lo    ; arg2=x1
        sta arg2
        lda y1
        sta arg3    ; arg3=y1
        lsr arg4    ; remove old x1 9th bit with shift right
        lda x1hi
        and #$01
        lsr         ; move x1 9th bit into carry
        rol arg4    ; rotate new x1 9th bit back in bit0, saving pixel color in bit1

        jsr case6   ; plot point

        clc         ; xfrac += xs(lope)
        lda fraclo
        adc xslo
        sta fraclo
        lda frachi
        adc xshi
        sta frachi

        lda xshi    ; xs(lope) < 0 ?
        bmi xsneg   ; yes - branch
        beq chkxslo ; zero? chk low bits
        bne xspos   ; non-zero
chkxslo
        lda xslo
        beq nexty   ; skip if zero
xspos
        lda frachi  ; xfrac >= abs(ys) ?
        cmp yshi
        bcc nexty  ; no - proceed to nextx
        bne incx1
        lda fraclo
        cmp yslo
        bcc nexty
           ; yes...
incx1
        inc x1lo    ; ++x1
        bne noincxhi
        inc x1hi
noincxhi

        sec         ; xfrac -= abs(ys)
        lda fraclo
        sbc yslo
        sta fraclo
        lda frachi
        sbc yshi
        sta frachi

        clc         ; force carry clear
        bcc nexty   ; abs branch nexty

xsneg

        sec         ; xfrac <= -abs(ys) ?
        lda #$00
        sbc yslo
        sta ptrl
        lda #$00
        sbc yshi
        sta ptrh
        lda frachi
        cmp ptrh
        bcc nexty
        bne decx1
        lda fraclo
        cmp ptrl
        bcc nexty

decx1
        sec
        lda x1lo
        sbc #$01
        sta x1lo
        lda x1hi
        sbc #$00
        sta x1hi

        clc
        lda fraclo
        adc yslo
        sta fraclo
        lda frachi
        adc yshi
        sta frachi

nexty
        lda y1       ; y1 == y2 ?
        cmp y2
        bne stepy    ; no - so branch
        lda x2lo     ; reload x1 with x2 just in case
        sta x1lo     ;   to avoid -1
        lda x2hi
        sta x1hi
        rts          ; yes - line is done

stepy:      ; y += inc (+/- 1)
        clc
        lda y1
        adc incr
        sta y1

        jmp fory

; end line

case11_apply_color      ; shape/pattern put with color, after drawing shape, apply color to whole region
        lda case14_xslo
        sta x1lo
        lda case14_xshi
        sta x1hi
        lda case14_yslo
        sta y1
        lda x2lo
        sta case14_x2lo
        lda x2hi
        sta case14_x2hi
        lda y2
        sta case14_y2
        lda #1
        sta case14_arg1
        jsr gethirescolor
        stx case14_arg2
        jmp color_range

draw_rect
        lda rect_x1lo
        sta x1lo
        lda rect_x1hi
        sta x1hi
        lda rect_y1
        sta y1
        sta y2
        lda rect_x2lo
        sta x2lo
        lda rect_x2hi
        sta x2hi
        lda rect_pixel
        asl
        eor #$02
        sta arg4
        jsr line

        lda rect_x2lo
        sta x2lo
        lda rect_x2hi
        sta x2hi
        lda rect_y2
        sta y2
        lda rect_pixel
        jsr line

        lda rect_x1lo
        sta x2lo
        lda rect_x1hi
        sta x2hi
        lda rect_y2
        sta y2
        jsr line

        lda rect_x1lo
        sta x2lo
        lda rect_x1hi
        sta x2hi
        lda rect_y1
        sta y1
        jmp line

screen_swap
        sty option_alternate
        stx ptrl
        sty ptrh
        lda option_plotting
        sta arg2
        lda #0
        sta arg1
        lda #$20 ; 32 pages of 256 bytes
        sta arg3
        jsr swap ; graphic screen

        ; also swap color RAM between screens
        lda option_alternate
        jsr hires_to_color
        sta ptrh
        lda option_plotting
        jsr hires_to_color
        sta arg2
        ldy #0
        sty ptrl
        sty arg1
        lda #0
        lda #$04 ; 4 pages of 256 bytes
        sta arg3
        ; continue to swap for color RAM
swap
        ldy #$00
-       bit arg4
        bvs +           ; ptrl -> arg1 only
        lda (arg1),y
        ldx arg4
        bmi ++          ; arg1 -> ptrl only
        tax
+       lda (ptrl),y
        sta (arg1),y
        bit arg4
        bvs +
        txa             ; swap in progress if here, ptrl <-> arg1
++      sta (ptrl),y
+       iny
        bne -
        inc arg2
        inc ptrh
        dec arg3
        bne -
        rts
finish
