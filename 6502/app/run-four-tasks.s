    org $fffa
    word nmi
    word reset_main
    word irq
    org $8000

    include via.s
    include ticks.s
    include nmi_irq.s
    include arith16.s
    include acia.s
    include lcd.s
    include screen.s
    include macs.s
    include decimal.s
    include decimal24.s
    include print.s
    include panic.s
    include macs.s
    include arith16.s
    include heap.s

;;; not in hex for some reason...
task1 = 10
task2 = 20
task3 = 30

find_roots:
    phx
    ldx #task1
    find_roots_from task1
    ldx #task2
    find_roots_from task2
    ldx #task3
    find_roots_from task3
    plx
    rts

panic_if_not_in_rom_sub:
    cmp #$80
    bcc .bad
    rts
.bad:
    panic 'OOR'

panic_if_not_in_rom: macro V
    pha
    lda \V + 1
    jsr panic_if_not_in_rom_sub
    pla
endmacro

enter_fp: macro
    jsr screen_flush_when_time
    jmp (switcher)
endmacro

    include clock.s
    include speed-watch.s
    include primes.s
    include fib24.s
    include fibs.s

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
g_divisor = $54 ; decimal.s
g_mod10 = $56 ; decimal.s
g_mptr = $58 ; print.s
g_putchar = $5a ; decimal.s

g_divisor24 = $60 ; decimal24.s
g_modulus24 = $63 ; decimal24.s

switcher = $66 ; switches task, either: 1->2, 2->3, 3->1

NUM_SCREENS = 4
g_screen_pointers = $80
g_screens = $200

task1_screen = 0
task2_screen = 1
task3_screen = 3

reset_main:
    ldx #$ff
    txs
    jsr init_via
    jsr init_ticks
    jsr init_nmi_irq
    jsr init_acia
    jsr init_lcd
    jsr lcd_clear_display
    jsr init_screen

    acia_print_string "\n\nRESET...\n"

    init_heap 2 ; gc_screen

    ldx #task1
    jsr fib_iter.begin

    ldx #task2
    jsr clock.begin

    ldx #task3
    jsr primes.begin

    ;ldx #task4 ; TODO: make this work!
    ;jsr speed_watch.begin

    jmp switch_to_1

switch_to_1:
    store16i switch_to_2, switcher
    store8i task1_screen, g_selected_screen
    ldx #task1
    load16_0 task1, cp
    panic_if_not_in_rom cp
    jmp (cp)

switch_to_2:
    store16i switch_to_3, switcher
    store8i task2_screen, g_selected_screen
    ldx #task2
    load16_0 task2, cp
    panic_if_not_in_rom cp
    jmp (cp)

switch_to_3:
    store16i switch_to_1, switcher
    store8i task3_screen, g_selected_screen
    ldx #task3
    load16_0 task3, cp
    panic_if_not_in_rom cp
    jmp (cp)
