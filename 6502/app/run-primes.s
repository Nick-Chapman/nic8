
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

;;; words
hp = $40
fp = $42
cp = $44
clo = $46
ev = $48
lw = $4a
temp = $4c
space_switcher = $4c
heap_start = $4e
gc_count = $50

g_divisor = $54 ; decimal.s
g_mod10 = $56 ; decimal.s
g_mptr = $58 ; print.s

g_screen_pointers = $80

;;; buffers
g_screens = $200 ; 4*32 bytes


flush: macro
    pha
    lda g_nmi_count
    and #%1 ; use nmi-count to pick screen #0 or #1
    jsr screen_flush
    pla
endmacro

screen_flush_when_time: ; TODO
    rts

pause:
    pha
    lda #1
    jsr sleep_blocking
    pla
    rts

;;; ----------------------------------------------------------------------
;;; show/test divides
;; show_divides:
;;     print_char ','
;;     print_decimal_word 1
;;     print_char '/'
;;     print_decimal_word 3
;;     flush

;;     copy16_literal_to_var after_divides.static_closure, 5
;;     copy16_literal_to_var divides.static_closure, fp
;;     enter_fp

;; after_divides:
;; .code:
;;     print_char '='
;;     print_decimal_byte 1
;;     flush
;;     jsr pause
;;     rts
;; .static_closure:
;;     word .code

;; test_divides: macro A, B
;;     lda #<\A
;;     sta 1
;;     lda #>\A
;;     sta 2
;;     lda #<\B
;;     sta 3
;;     lda #>\B
;;     sta 4
;;     jsr show_divides
;; endmacro

;;; ----------------------------------------------------------------------
;;; show/test candidate

show_candidate:
    ;print_char ','
    print_decimal_word 1
    flush

    copy16_literal_to_var after_candidate.static_closure, 5
    copy16_literal_to_var candidate.static_closure, fp
    enter_fp


print_bool:
    beq .dot
    print_char 'x'
    rts
.dot:
    print_char '.'
    rts

;;; should there be the roots/evac/scav table here? -yes, but the easy static thing
after_candidate: ; same as after divides - just displays a bool
.code:
    ;print_char '='
;print_decimal_byte 1
    lda 1
    jsr print_bool
    flush
    jsr pause
    rts
.static_closure:
    word .code

test_candidate: macro A, PS
    lda #<\A
    sta 1
    lda #>\A
    sta 2
    copy16_literal_to_var \PS, 3 ; nil
    jsr show_candidate
endmacro

;;; ----------------------------------------------------------------------
;;; main

reset_main:
    ldx #$ff
    txs
    jsr init_via
    jsr init_ticks
    jsr init_nmi_irq
    jsr init_lcd
    jsr lcd_clear_display
    jsr init_screen
    jsr init_gc_sub

    ;; test_divides 0, 3
    ;; test_divides 1, 3
    ;; test_divides 2, 3
    ;; test_divides 3, 3
    ;; test_divides 4, 3
    ;; test_divides 5, 3
    ;; test_divides 6, 3
    ;; test_divides 7, 3

    ;; test_divides 2000, 399
    ;; test_divides 2000, 400
    ;; test_divides 2000, 401

    ;; test_divides 10000, 5
    ;; test_divides 10001, 5

    ;; test_divides 99, 33

    ;; test_candidate 6, nil_cell_i16.static_closure
    ;; test_candidate 7, nil_cell_i16.static_closure

    ;; test_candidate 6, list_32
    ;; test_candidate 7, list_32
    ;; test_candidate 8, list_32
    ;; test_candidate 9, list_32

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
    ;; dont expect to everer get here!
    print_string "$"
    flush
spin:
    jmp spin


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
