
;;; Top level app to generate primes numbers

    org $fffc
    ;; word nmi
    word reset_main
    word ticks_irq

    org $8000

    include "via.s"
    include "ticks.s"
    include "lcd.s"
    include "sleep.s"
    include "mscreen.s"
    include "decimal.s"
    include "print.s"
    include "panic.s"
    include "macs.s"
    include "primes.s"

;;; bytes
g_ticks = $32
g_selected_screen = $34

hp = $40
fp = $42
cp = $44
clo = $46
ev = $48

g_divisor = $54 ; decimal.s
g_mod10 = $56 ; decimal.s

g_screen_pointers = $80

;;; buffers
g_screens = $200 ; 4*32 bytes


test_divides: macro A, B
    lda #\A
    sta 1
    lda #\B
    sta 2
    jsr show_divides
endmacro

reset_main:
    ldx #$ff
    txs
    jsr init_via
    jsr init_ticks
    jsr init_lcd
    jsr lcd_clear_display
    jsr init_screen

    test_divides 0, 3
    test_divides 1, 3
    test_divides 2, 3
    test_divides 3, 3
    test_divides 4, 3
    test_divides 5, 3
    test_divides 6, 3
    test_divides 7, 3

    print_char "$"
    screen_flush_selected
spin:
    jmp spin

show_divides:
    print_char ','
    print_decimal_byte 1
    print_char '/'
    print_decimal_byte 2
    screen_flush_selected

    copy16_literal_to_var after_divides.static_closure, 3
    copy16_literal_to_var divides.static_closure, fp
    enter_fp

after_divides:
.code:
    print_char '='
    print_decimal_byte 1
    screen_flush_selected
    jsr pause
    rts
.static_closure:
    word .code

pause:
    pha
    lda #100
    jsr sleep_blocking
    pla
    rts
