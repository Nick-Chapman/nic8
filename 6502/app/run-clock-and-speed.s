    org $fffa
    word nmi
    word reset_main
    word irq
    org $8000

    include via.s
    include interrupts.s
    include arith16.s
    include acia.s
    include lcd.s
    include screen.s
    include macs.s
    include decimal16.s
    include print.s
    include panic.s
    include macs.s
    include arith16.s
    include heap.s
    include tasking.s
    include clock.s
    include speed.s
    include null.s

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

switcher = $64 ; switches task, either: 1->2 or 2->1

NUM_SCREENS = 2
g_screen_pointers = $80
g_screens = $200

reset_main:
    ldx #$ff
    txs
    jsr via.init
    jsr init_ticks
    jsr init_nmi_irq
    jsr acia.init
    jsr lcd.init
    jsr lcd.clear_display
    jsr screen.init
    init_heap 2 ; screen-number

    ldx #task1
    store8i_x 0, clock.screen
    jsr clock.begin

    ldx #task2
    store8i_x 1, speed.screen
    jsr speed.begin

    ldx #task3
    jsr null.begin

    ldx #task4
    jsr null.begin

    ldx #task5
    jsr null.begin

    jmp switch_to_1
