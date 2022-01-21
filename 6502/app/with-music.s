
;;; Play music concurrently with four other tasks

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
    include decimal24.s
    include print.s
    include panic.s
    include macs.s
    include arith16.s
    include heap.s
    include sound.s
    include tasking.s
    include clock.s
    include speed.s
    include music.s
    include fib24.s
    include fibs.s
    include primes.s

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

g_divisor24 = $60 ; decimal24.s
g_modulus24 = $63 ; decimal24.s

switcher = $66

NUM_SCREENS = 8
g_screen_pointers = $80
g_screens = $200

reset_main:
    ldx #$ff
    txs
    jsr via.init
    jsr init_ticks
    jsr init_nmi_irq
    jsr sound.init
    jsr acia.init
    jsr lcd.init
    jsr lcd.clear_display
    jsr screen.init
    acia_print_string "\n\nRESET...\n"
    init_heap 5 ; gc_screen

    ldx #task1
    store8i_x 0, music.screen
    jsr music.begin

    ldx #task2
    store8i_x 1, speed.screen
    jsr speed.begin

    ldx #task3
    store8i_x 2, clock.screen
    jsr clock.begin

    ldx #task4
    store8i_x 3, primes.screen
    jsr primes.begin

    ldx #task5
    store8i_x 4, fibs.screen
    jsr fibs.begin

    jmp switch_to_1
