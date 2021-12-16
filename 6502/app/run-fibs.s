;;; Example program which runs fib forver on increasing values of N.
;;; Entire program written in the CPS/GC style, with no outer harness.
;;; Purpose is to motivate a clean interface to the GC alloc.
;;; And to understand exactly what invariants are placed on user code.

    org $fffa
    word nmi
    word reset_main
    word irq
    org $8000

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
space_switcher = $4c
gc_count = $50
heap_start = $52
g_divisor = $54 ; decimal.s
g_mod10 = $56 ; decimal.s
g_mptr = $58 ; print.s

;;; trip
g_divisor24 = $60 ; decimal24.s
g_modulus24 = $63 ; decimal24.s

NUM_SCREENS = 2
g_screen_pointers = $80
g_screens = $200 ; 4*32 bytes

    include via.s
    include ticks.s
    include lcd.s
    include screen.s
    include decimal.s
    include decimal24.s
    include print.s
    include sleep.s
    include debug.s
    include panic.s
    include macs.s
    include arith16.s
    include heap.s
    include nmi_irq.s
    include executive.s
    include fib24.s
    include fibs.s

reset_main:
    ldx #$ff
    txs
    jsr init_via
    jsr init_ticks
    jsr init_nmi_irq
    jsr init_lcd
    jsr lcd_clear_display
    jsr init_screen
    init_heap 1 ; screen-number
    jmp start_example

start_example:
    stz arg2
    store16i fib_iter.static_closure, fp
    jmp fib_iter.code
