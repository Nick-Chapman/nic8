
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

g_first_task = $68 ; byte
g_task = $70 ; word

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
    jsr tasking.init
    acia_print_string "\n\nRESET...\n"
    init_heap 6 ; gc_screen

    lda #delay_music.size_locals+1
    jsr tasking.create
    store8i_x 0, delay_music.screen
    jsr delay_music.begin

    lda #speed.size_locals+1
    jsr tasking.create
    store8i_x 2, speed.screen
    jsr speed.begin

    lda #clock.size_locals+1
    jsr tasking.create
    store8i_x 3, clock.screen
    jsr clock.begin

    lda #primes.size_locals+1
    jsr tasking.create
    store8i_x 4, primes.screen
    jsr primes.begin

    lda #fibs.size_locals+1
    jsr tasking.create
    store8i_x 5, fibs.screen
    jsr fibs.begin

    jmp tasking.start


;;; Experiment with dynamic task creation...
;;; This task is created at initialization.
;;; Then after 1 second it creates the actual music task
delay_music:
.fp = 0
.jiffy = 2
.screen = 3
.size_locals = 4
.begin:
    store16i_x .start_closure, .fp
    lda g_ticks
    sta .jiffy, x
    lda #100
    jsr .set_wait
    copyFrom8_x .screen, g_selected_screen
    print_string "Waiting..."
    rts
.roots:
    rts ; no roots
.evac:
    rts ; static
.scav:
    impossible_scavenge_because_static

.start_closure:
    word .start
    word .roots, .evac, .scav
.start:
    lda g_ticks
    cmp .jiffy, x
    bpl .spawn_music
    yield

.spawn_music:
    copyFrom8_x .screen, g_selected_screen
    print_string "\nPLAY"
    phx
    lda #music.size_locals+1
    jsr tasking.create
    store8i_x 1, music.screen
    jsr music.begin
    plx
    store16i_x .null_closure, .fp
    yield

.null_closure:
    word .null
    word .roots, .evac, .scav
.null:
    yield

.set_wait:
    clc
    adc .jiffy, x
    sta .jiffy, x
    rts
