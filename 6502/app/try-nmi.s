;;; Test Button connect to NMI.

    org $fffa
    word nmi
    word reset_main
    word ticks_irq

    org $8000

    include via.s
    include ticks.s
    include sleep.s
    include lcd.s
    include screen.s
    include decimal.s
    include print.s

;;; bytes
g_ticks = $d0

;;; words
g_divisor = $e0 ; decimal.s
g_mod10 = $e2 ; decimal.s
g_screen_pointer = $e4
g_nmi_count = $e6

;;; buffers
g_screen = $200 ; 32 bytes

nmi:
    print_char '.'
    inc g_nmi_count
    bne .done
    inc g_nmi_count + 1
.done:
    rti

init_nmi:
    lda #0
    sta g_nmi_count
    sta g_nmi_count + 1
    rts

reset_main:
    ldx #$ff
    txs
    jsr init_via
    jsr init_ticks
    jsr init_lcd
    jsr lcd_clear_display
    jsr init_screen
    jsr init_nmi
.loop:
    print_decimal_word g_nmi_count
    jsr pause
    print_char ' '
    jmp .loop

pause:
    pha
    lda #100
    jsr sleep_blocking
    pla
    rts
