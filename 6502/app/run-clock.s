    org $fffa
    word nmi
    word reset_main
    word irq
    org $8000

    include via.s
    include ticks.s
    include arith16.s
    include acia.s
    include nmi_irq.s
    include lcd.s
    include screen.s
    include macs.s
    include decimal16.s
    include print.s
    include panic.s
    include macs.s
    include arith16.s
    include heap.s

find_roots:
    panic "find_roots"

task1 = 15

enter_fp: macro
    load16_0 task1, cp
    jsr screen_flush_when_time
    ldx #task1
    jmp (cp)
endmacro

    include clock.s

;;; bytes
heap_end_page = $30
g_ticks = $32
gc_screen = $33
g_selected_screen = $34
g_nmi_count = $35
g_nmi_blocked = $36
g_next_screen_flush = $37

;;; words
g_heap_pointer = $40
heap_start = $4e
gc_count = $50
space_switcher = $52
g_divisor = $54 ; decimal16.s
g_mod10 = $56 ; decimal16.s
g_mptr = $58 ; print.s
g_putchar = $5a ; decimal16.s

NUM_SCREENS = 1
g_screen_pointers = $80
g_screens = $200

reset_main:
    ldx #$ff
    txs
    jsr init_via
    jsr init_ticks
    jsr init_nmi_irq
    jsr acia.init
    jsr lcd.init
    jsr lcd.clear_display
    jsr init_screen
    init_heap 1 ; screen-number

    ldx #task1
    store8i_x 0, clock.screen
    jsr clock.begin
    enter_fp
