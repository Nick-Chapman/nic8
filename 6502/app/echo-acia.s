    org $fffa
    word nmi
    word main
    word deprecated_ticks_irq
    org $8000

cpu_clks_per_sec = 1 * MHz ; run more slowly for the ACIA chip

    include via.s
    include arith16.s
    include acia.s
    include ticks1.s
    include lcd.s
    include screen.s
    include macs.s
    include decimal16.s
    include print.s
    include sleep.s

g_ticks = $32
g_selected_screen = $34

g_nmi_count = $35
g_next_screen_flush = $37

g_divisor = $54 ; decimal16.s
g_mod10 = $56 ; decimal16.s
g_mptr = $58 ; print.s / acia.put_string
g_putchar = $5a ; decimal16.s

NUM_SCREENS = 2
g_screen_pointers = $80
g_screens = $200

nmi:
    rti

main:
;;; local vars in zero page
.received = 1
    ldx #$ff
    txs
    jsr init_via
    jsr init_ticks
    jsr lcd.init
    jsr lcd.clear_display
    jsr init_screen
    jsr acia.init
    acia_print_string ">"
    print_string ">"
    screen_flush_selected
.again:
    jsr acia.read_blocking
    sta .received
    jsr acia.putchar
    ;debug_hex_byte .received
    lda .received
    jsr screen_putchar
    screen_flush_selected
    jmp .again
