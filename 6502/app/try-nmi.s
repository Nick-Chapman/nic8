;;; Test Button connect to NMI.

    org $fffa
    word nmi
    word reset_main
    word irq

    org $8000

    include via.s
    include ticks.s
    include sleep.s
    include lcd.s
    include screen.s
    include decimal.s
    include print.s

;;; bytes
g_nmi_blocked = $d1

;;; words
g_divisor = $e0 ; decimal.s
g_mod10 = $e2 ; decimal.s
g_screen_pointer = $e4
g_nmi_count = $e6
g_ticks = $e8 ; 16 bit tick (10 minutes!)
g_temp = $ea

;;; buffers
g_screen = $200 ; 32 bytes

nmi:
    pha
    lda g_nmi_blocked
    bne .done
    lda #25 ; debounce time
    sta g_nmi_blocked
    inc g_nmi_count
    bne .done
    inc g_nmi_count + 1
.done:
    pla
    rti

irq: ; copy & extend version in ticks.s
    pha
    bit T1CL ; acknowledge interrupt
    inc g_ticks
    bne .unblock
    inc g_ticks + 1
.unblock:
    lda g_nmi_blocked
    beq .done
    dec g_nmi_blocked
.done:
    pla
    rti

init_nmi:
    lda #0
    sta g_nmi_blocked
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
    stz g_ticks + 1
.loop:
    lda g_ticks
    sta g_temp
    lda g_ticks + 1
    sta g_temp + 1
    jsr screen_return_home
    print_decimal_word g_nmi_count
    print_char ' '
    print_decimal_word g_temp
    lda #0
    jsr screen_flush
    jsr pause
    jmp .loop

pause:
    pha
    lda #10
    jsr sleep_blocking
    pla
    rts
