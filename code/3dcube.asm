; 3dcube in acme assembler, requires davervw's hires-c64
; port by David Van Wagner 2025-06-11
; original C code was by John Livdahl: https://pastebin.com/dhaEdtSt

; hires ml API reference for porting

; case 0 () - init variables, set vectors, etc.
; case 1 () - graphics on
; case 2 () - set graphic memory address to predefined
; case 3 (x=fg/bg) - set color ram and initialize fg/bg cell colors
; case 4 () - graphics clear
; case 5 () - graphics off 
; case 6 (x, y, flags [c=x8, z=color) - plot point
; case 7 (x, y, c=x8) - locate
; case 8 (x, y, c=x8, z=color) - drawto
; case 9 - shape size
; case 10- shape get
; case 11- shape/pattern put
; case 12 (x=altlo, y=althi) - screen swap
; case 13 - draw character at pixel location
; case 14 - set fg/bg 8x8 cell color at pixel location
; case 15 - text to graphics

; c014 - current plotting hires address
; c015 - alternate hires address
; c016 - option apply color
; c017 [8] - corresponding color high byte address for possible 8 hires screens in 8K banks (some invalid will have zeros)
; c01f option hiresfg (used by option apply color)
; c020 option hiresbg (used by option apply color)

!macro HIRES_INIT {
    lda #0
    jsr $c000
}

!macro HIRES_GRAPHICS {
    lda #1
    jsr $c000
}

!macro HIRES_PRIMARY {
    lda #2
    jsr $c000
}

!macro HIRES_COLOR_INIT {
    lda bg
    asl
    asl
    asl
    asl
    ora fg
    tax ; x = bg in high 4 bits, fg in lower 4 bits
    lda #3
    jsr $c000
}

!macro HIRES_CLEAR {
    lda #4
    jsr $c000
}

!macro HIRES_OFF {
    lda #5
    jsr $c000
}

!macro HIRES_PLOT .x, .y { ; 9th bit of x coordinate is in C
    ldx .x
    ldy .y
    lda #6 ; force .Z off, so not clear point
    jsr $c000
}

!macro HIRES_LOCATE .x, .y { ; 9th bit of x coordinate is in C
    ldx .x
    ldy .y
    lda #7 ; force .Z off, so not clear point
    jsr $c000
}

!macro HIRES_DRAWTO .x, .y { ; 9th bit of x coordinate is in C
    ldx .x
    ldy .y
    lda #8 ; force .Z off, so not clear point
    jsr $c000
}

!macro HIRES_ADDR .addr {
    lda #>.addr
    sta $c014
}

!macro COLOR_ADDR .addr {
    lda $c014
    lsr
    lsr
    lsr
    lsr
    lsr
    tax
    lda #>.addr
    sta $c017,x
}

!macro HIRES_COLOR .fg, .bg, .border {
    lda #.fg
    and #$f
    sta fg
    lda #.bg
    and #$f
    sta bg
    lda #.border
    sta $d020
}

!macro HIRES_CLEAR_PLOT .addr {
    +HIRES_ADDR .addr
    +HIRES_COLOR_INIT
    +HIRES_CLEAR
}

!macro ASSIGN_TO .mem, .value {
    lda #.value
    sta .mem
}

!macro SUBTRACT_TO .mem, .orig, .diff {
    lda .orig
    sec
    sbc .diff
    sta .mem
}

!macro ADD_TO .mem, .orig, .more {
    lda .orig
    clc
    adc .more
    sta .mem
}

!macro ADD_VALUE_TO .mem, .orig, .value {
    lda .orig
    clc
    adc #.value
    sta .mem
}

!macro TOGGLE_HIGH .mem {
    lda .mem
    eor #$80
    sta .mem
}

!macro locate .x, .y {
    +ADD_VALUE_TO y0, .y, 16
    +ADD_VALUE_TO x0, .x, 100 ; C will have overflow 9th bit
    +HIRES_LOCATE x0, y0
}

!macro draw_to .x, .y {
    +ADD_VALUE_TO y0, .y, 16
    +ADD_VALUE_TO x0, .x, 100 ; C will have overflow 9th bit
    +HIRES_DRAWTO x0, y0
}

!macro draw_one {
    +locate b, a
    +draw_to c, b
    +draw_to d, c
    +draw_to a, d

    +locate c, c
    +draw_to b, d
    +draw_to a, f
    +draw_to d, e

    +locate c, b
    +draw_to c, c
    +draw_to d, e
    +draw_to d, c

    +locate a, f
    +draw_to a, d
    +draw_to b, a
    +draw_to b, d
}

!macro activate_screen {
    +HIRES_GRAPHICS
    +HIRES_PRIMARY
}

!macro next_plot_screen {
    bit p
    bpl +
    +HIRES_CLEAR_PLOT $A000
    jmp ++
+   +HIRES_CLEAR_PLOT $E000
++  ;done
}

!macro NEXT_INCREMENT .addr, .var, .max {
    inc .var
    lda .var
    cmp #.max
    bcs +
    jmp .addr
+
}

!macro exit_on_stop {
    jsr $FFE1
    bne +
    +HIRES_OFF
    rts
+
}

*=$1000

; zero page memory
p = $a3
r = $a4
a = $a5
b = $a6
c = $a7
d = $a8
e = $a9
f = $aa
x0 = $b5
y0 = $b6

start:

+HIRES_ADDR $E000
+COLOR_ADDR $D800
+HIRES_ADDR $A000
+COLOR_ADDR $8C00
+HIRES_COLOR 11, 0, 11
+ASSIGN_TO p, $80
+ASSIGN_TO r, 60

again:
    +ASSIGN_TO a, 0

loop_a:
        +exit_on_stop
        +next_plot_screen
        +SUBTRACT_TO b, r, a
        +ADD_TO c, b, r
        +ADD_TO d, a, r
        +ADD_TO e, c, r
        +ADD_TO f, d, r
        +draw_one
        +activate_screen
        +TOGGLE_HIGH p
    +NEXT_INCREMENT loop_a, a, 60

jmp again

fg: !byte 1 ; 1=white
bg: !byte 0 ; 0=black
