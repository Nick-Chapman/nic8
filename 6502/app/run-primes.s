
;;; Top level app to generate primes numbers

    org $fffa
    word nmi
    word reset_main
    word irq

    org $8000

    include via.s
    include ticks.s
    include nmi_irq.s
    include lcd.s
    include sleep.s
    include screen.s
    include decimal.s
    include print.s
    include debug.s
    include panic.s
    include macs.s
    include gc.s
    include primes.s

;;; bytes
heap_end_page = $30
n_bytes = $31
g_ticks = $32
gc_screen = $33
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
temp = $4c
heap_start = $4e
gc_count = $50
space_switcher = $52

g_divisor = $54 ; decimal.s
g_mod10 = $56 ; decimal.s
g_mptr = $58 ; print.s

data_pointer = $60 ; whenin a cons cell, which we DONT enter

g_screen_pointers = $80

;;; buffers
g_screens = $200 ; 4*32 bytes


screen_flush_when_time: ; TODO: copied form forever-fib -- share!
    lda g_next_screen_flush
    sec
    sbc g_ticks
    beq screen_flush_now ; TODO: fix so we cant miss the time
    rts
screen_flush_now:
    lda g_nmi_count
    and #%1 ; use nmi-count to pick screen #0 or #1
    jsr screen_flush_sub
    lda g_ticks
    clc
    adc #5 ; 20 times/sec
    sta g_next_screen_flush
    rts

;; flush: macro
;;     jsr screen_flush_when_time
;; endmacro

;; pause:
;;     pha
;;     jsr screen_flush_now
;;     lda #20
;;     jsr sleep_blocking
;;     pla
;;     rts

reset_main:
    ldx #$ff
    txs
    jsr init_via
    jsr init_ticks
    jsr init_nmi_irq
    jsr init_lcd
    jsr lcd_clear_display
    jsr init_screen
    init_gc 1 ; screen-number
    jsr screen_flush_now ; sets the next(first) time to flush

    ldy #0
    ldx #2
.loop:
    phx
    phy
    stx 1 ; lo
    sty 2 ; hi
    copy16_literal_to_var list_7532, 3
    jsr show_candidate
    ply
    plx
    inx
    bne .loop
    iny
    bne .loop
    ;; get here when we loop around the whole 16 bit range
    print_string "$"
    jsr screen_flush_now
spin:
    jmp spin

push_word: macro V
    lda \V+1
    pha
    lda \V
    pha
endmacro

pull_word: macro V
    pla
    sta \V
    pla
    sta \V+1
endmacro

show_candidate:
    push_word 1
    copy16_literal_to_var after_candidate.static_closure, 5
    copy16_literal_to_var candidate.static_closure, fp
    enter_fp

after_candidate:
    byte 'Z'
    word .roots, .evac, .scav
.code:
    lda 1
    beq .false
    pull_word 1
;print_char '.'
    newline
    print_decimal_word 1
    jsr screen_flush_now
    rts
.false:
    pull_word 1
    rts
.roots:
    impossible_roots
.evac:
    no_evacuate_because_static
.scav:
    impossible_scavenge_because_static
.static_closure:
    word .code


;;; static lists
list_2:
    word cons_cell_i16.code
    word 2
    word nil_cell_i16.static_closure

list_32:
    word cons_cell_i16.code
    word 3
    word list_2

list_532:
    word cons_cell_i16.code
    word 5
    word list_32

list_7532:
    word cons_cell_i16.code
    word 7
    word list_532
