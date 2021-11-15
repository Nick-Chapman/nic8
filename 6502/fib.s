;;; Explore fib example...
;;; (1) coded using normal control stack
;;; (2) coded in CPS style, using heap

    .org $fffc
    .word reset_main
    .word irq

    .org $8000


;;; TODO: move these defs to lib/via.s
PORTB = $6000
DDRB = $6002
init_via:
    lda #%11111111
    sta DDRB
    rts


g_divisor = $51 ; 2 bytes
g_mod10 = $53 ; 2 bytes


g_arg = $70 ; BYTE n
g_res = $71 ; WORD (fib n)
ticks = $73 ; TODO: rename g_tick & fixup all files
g_screen_pointer = $74
goal_ticks = $75

g_screen = $200 ; 32 bytes

;;; TODO: use prefixes consistently for library code
    include ticks.s ; requires: ticks; provides: irq; init_timer
    include lcd.s   ; requires: DDRB, PORTB; provides: init_display, clear_display, print_char
    include screen.s ; requires: g_screen{,_pointer}; provides: init_screen, print_screen, putchar

reset_main:
    jsr init_via
    jsr init_timer
    jsr init_display
    jsr clear_display
    jsr init_screen
    jmp example

example:
    lda #0
example_loop:
    jsr pause
    pha
    sta g_arg
    jsr put_arg
    jsr print_screen ;hmm
    jsr fib
    jsr put_res
    jsr print_screen ;hmm
    pla
    clc
    adc #1
    jmp example_loop

pause:
    pha
    lda #50
    jsr sleep
    pla
    rts

fib:
    lda g_arg
    sec
    cmp #2
    bcc fib_base
    pha ; n
    sec
    sbc #1
    sta g_arg
    ;; compute: fib (n-1)...
    jsr fib
    pla ; n
    sta g_arg

    lda g_res + 1
    pha ; fib (n-1) HI
    lda g_res
    pha ; fib (n-1) LO

    lda g_arg
    sec
    sbc #2
    sta g_arg
    ;; compute: fib (n-2)...
    jsr fib
    pla ; fib (n-1) LO
    clc
    adc g_res
    sta g_res

    pla ; fib (n-1) HI
    adc g_res + 1
    sta g_res + 1

    rts

fib_base:
    sta g_res
    lda #0
    sta g_res + 1
    rts

put_arg:
    lda g_arg
    jsr put_byte_a_in_decimal
    lda #'-'
    jsr scrolling_putchar
    rts

put_res:
    lda g_res
    ldx g_res + 1
    jsr put_word_ax_in_decimal
    lda #' '
    jsr scrolling_putchar
    rts

put_byte_a_in_decimal:
    ldx #0
    jmp put_word_ax_in_decimal

;; ;;; put byte passed in accumulator as 2 digit hex number
;; put_hex:
;;     pha
;;     lsr
;;     lsr
;;     lsr
;;     lsr
;;     tax
;;     lda digits,x
;;     jsr scrolling_putchar
;;     pla
;;     and #%1111
;;     tax
;;     lda digits,x
;;     jsr scrolling_putchar
;;     rts

digits: .ascii "0123456789abcdef"


scrolling_putchar:
    jsr maybe_scroll
    jmp putchar


;;; --------------------
;;; copied from sonic.s.. TODO: share
;;; blocking sleep for N (in accumulator) 1/50s ticks (so max time 2.5secs)
sleep:
    clc
    adc ticks
    sta goal_ticks
sleep_wait:
    sec
    lda goal_ticks
    sbc ticks
    bne sleep_wait
    rts

;;; --------------------
;;; Copied from pre-serial.. TODO: share

maybe_scroll:
    pha
    lda g_screen_pointer
    cmp #32
    bne maybe_scroll_done
    jsr scroll
    jsr screen_return_to_start_line2
maybe_scroll_done:
    pla
    rts

scroll:
    ldx #0
each_scroll_char:
    lda g_screen+16,x
    sta g_screen,x
    lda #' '
    sta g_screen+16,x
    inx
    sec
    cpx #16
    bne each_scroll_char
    rts

screen_return_to_start_line2:
    lda #16
    sta g_screen_pointer
    rts



;;;--------------------
;;; COPIED from computation.s (TODO: share)
;;; print word passed in a/x as 1-5 digit decimal number
put_word_ax_in_decimal:
    sta g_divisor ;lo
    stx g_divisor + 1 ;hi
    lda #0
    pha ; marker for print
each_digit:
    lda #0
    sta g_mod10
    sta g_mod10 + 1
    clc
    ldx #16
each_bit:
    rol g_divisor
    rol g_divisor + 1
    rol g_mod10
    rol g_mod10 + 1
    sec
    lda g_mod10
    sbc #10
    pha ; save
    lda g_mod10 + 1
    sbc #0
    bcc ignore_result
    sta g_mod10 + 1
    pla
    pha
    sta g_mod10
ignore_result:
    pla ;drop
    dex
    bne each_bit
    rol g_divisor
    rol g_divisor + 1
    clc
    lda g_mod10
    adc #'0'
    pha ;save on stack for reverse print
    lda g_divisor
    ora g_divisor + 1
    bne each_digit
put_from_stack:
    pla
    beq done
    jsr scrolling_putchar
    jmp put_from_stack
done:
    rts
