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
    include print.s
    include panic.s
    include macs.s
    include arith16.s
    include heap.s

;; NEXT: macro A ; OLD, REMOVE
;;     jsr screen_flush_when_time
;;     store16i \A, continue_code
;;     jmp (switcher)
;; endmacro

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

;;; First stab to round-robin between two tasks
;;; Local vars must not conflict - have temp hack (base = 10/20)
task1 = $60
task2 = $62
task3 = $64
switcher = $66 ; switches task, either: 1->2, 2->3, 3->1

NUM_SCREENS = 4
g_screen_pointers = $80
g_screens = $200

task1_screen = 0
task2_screen = 1
task3_screen = 2

;;; not in hex for some reason...
task1_vars_offset = 10
task2_vars_offset = 20
task3_vars_offset = 30

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
    init_heap 3 ; gc_screen

    ldx #task1_vars_offset
    jsr clock.begin
    copy16 fp, task1

    ldx #task2_vars_offset
    jsr primes.begin
    copy16 fp, task2

    ldx #task3_vars_offset
    jsr speed_watch.begin
    copy16 fp, task3

    copy16 task1, fp
    store16i switch_1_2, switcher

    store8i task1_screen, g_selected_screen
    ldx #task1_vars_offset
    load16_0 fp, cp
    panic_if_not_in_rom cp
    jmp (cp)

switch_1_2:
    copy16 fp, task1
    copy16 task2, fp
    store16i switch_2_3, switcher
    store8i task2_screen, g_selected_screen
    ldx #task2_vars_offset
    load16_0 fp, cp
    panic_if_not_in_rom cp
    jmp (cp)

switch_2_3:
    copy16 fp, task2
    copy16 task3, fp
    store16i switch_3_1, switcher
    store8i task3_screen, g_selected_screen
    ldx #task3_vars_offset
    load16_0 fp, cp
    panic_if_not_in_rom cp
    jmp (cp)

switch_3_1:
    copy16 fp, task3
    copy16 task1, fp
    store16i switch_1_2, switcher
    store8i task1_screen, g_selected_screen
    ldx #task1_vars_offset
    load16_0 fp, cp
    panic_if_not_in_rom cp
    jmp (cp)
