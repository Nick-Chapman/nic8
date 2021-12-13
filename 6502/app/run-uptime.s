    org $fffa
    word nmi
    word reset_main
    word irq
    org $8000

    include via.s
    include ticks.s
    include nmi_irq.s
    include lcd.s
    include screen.s
    include decimal.s
    include print.s
    include panic.s
    include executive.s
    include uptime.s

;;; bytes
g_ticks = $32
g_selected_screen = $34
g_nmi_count = $35
g_nmi_blocked = $36
g_next_screen_flush = $37

;;; words
g_divisor = $54 ; decimal.s
g_mod10 = $56 ; decimal.s
g_mptr = $58 ; print.s

NUM_SCREENS = 1
g_screen_pointers = $80
g_screens = $200

reset_main:
    ldx #$ff
    txs
    jsr init_via
    jsr init_ticks
    jsr init_nmi_irq
    jsr init_lcd
    jsr lcd_clear_display
    jsr init_screen
    jmp uptime.begin
