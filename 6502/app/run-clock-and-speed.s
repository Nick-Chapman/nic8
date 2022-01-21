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
    include decimal.s
    include print.s
    include panic.s
    include macs.s
    include arith16.s
    include heap.s

find_roots:
    panic "find_roots"

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

switcher = $64 ; switches task, either: 1->2 or 2->1

NUM_SCREENS = 2
g_screen_pointers = $80
g_screens = $200

task1 = 15
task2 = 25

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
    init_heap 2 ; screen-number

    ldx #task1
    store8i_x 0, clock.screen
    jsr clock.begin

    ldx #task2
    store8i_x 1, speed_watch.screen
    jsr speed_watch.begin

    jmp switch_to_1

switch_to_1:
    store16i switch_to_2, switcher
    ldx #task1
    load16_0 task1, cp
    panic_if_not_in_rom cp
    jmp (cp)

switch_to_2:
    store16i switch_to_1, switcher
    ldx #task2
    load16_0 task2, cp
    panic_if_not_in_rom cp
    jmp (cp)
