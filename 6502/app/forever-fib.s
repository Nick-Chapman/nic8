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
n_bytes = $31
g_ticks = $32
gc_debug = $33
g_selected_screen = $34
g_nmi_count = $35
g_nmi_blocked = $36
g_next_screen_flush = $37

;;; words
hp = $40
fp = $42
cp = $44
clo = $46
ev = $48
lw = $4a
space_switcher = $4c
temp = $4e
gc_count = $50
heap_start = $52
g_divisor = $54 ; decimal.s
g_mod10 = $56 ; decimal.s

;;; trip
g_divisor24 = $60 ; decimal24.s
g_modulus24 = $63 ; decimal24.s

;;; quad
g_screen_pointers = $80

;;; buffers
g_screens = $200 ; 4*32 bytes

    include via.s
    include ticks.s
    include lcd.s
    include screen.s
    include decimal.s
    include decimal24.s
    include print.s
    include panic.s
    include macs.s
    include gc.s
    include nmi_irq.s

reset_main:
    ldx #$ff
    txs
    jsr init_via
    jsr init_ticks
    jsr init_nmi
    jsr init_lcd
    jsr lcd_clear_display
    jsr init_screen
    jsr init_gc

    ;; send GC debug to screen #1
    lda #1
    sta gc_debug

    jsr screen_flush_now

    jmp start_example


screen_flush_when_time:
    lda g_next_screen_flush
    sec
    sbc g_ticks
    beq screen_flush_now
    rts
screen_flush_now:
    lda g_nmi_count
    and #%1 ; use nmi-count to pick screen #0 or #1
    jsr screen_flush
    lda g_ticks
    clc
    adc #5 ; 20 times/sec
    sta g_next_screen_flush
    rts


start_example:
    lda #0
    sta 0
    copy_code_pointer_to_local fib_iter.static_closure, fp
    jmp fib_iter.code

    include gen-fibs.s ; TODO: move to top
    include fib24.s
