;;; Early (pre task) speed check...
;;; How many times per jiffy (1/100s) do we complete the cyclic_executive

    org $fffa
    word nmi
    word reset_main
    word irq
    org $8000

    include via.s
    include interrupts.s
    include lcd.s
    include screen.s
    include macs.s
    include decimal16.s
    include print.s

;;; bytes
g_ticks = $30
g_selected_screen = $31
g_nmi_count = $32
g_nmi_blocked = $33
g_next_screen_flush = $34
g_last_speed_check_time = $35

;;; words
g_speed = $40 ; #repeats of cyclic_executive per jiffy
g_mptr = $42 ; print.s
g_divisor = $44 ; decimal16.s
g_mod10 = $46 ; decimal16.s
g_putchar = $48 ; decimal16.s

NUM_SCREENS = 1
g_screen_pointers = $80
g_screens = $200 ; page

reset_main:
    ldx #$ff
    txs
    jsr via.init
    jsr init_ticks
    jsr init_nmi_irq
    jsr lcd.init
    jsr lcd.clear_display
    jsr screen.init
    jsr init_speed_check
    jmp cyclic_executive

cyclic_executive:
    jsr check_speed
    jsr screen.flush_when_time
    jmp cyclic_executive

init_speed_check:
    lda g_ticks
    sta g_last_speed_check_time
    stz g_speed
    stz g_speed + 1
    rts

check_speed:
    sec
    lda g_last_speed_check_time
    sbc g_ticks
    bmi .we_have_advanced
    inc g_speed
    bne .skip
    inc g_speed + 1
.skip:
    rts
.we_have_advanced:
    jsr screen.return_home
    print_decimal_word g_speed
    jsr init_speed_check
    rts
