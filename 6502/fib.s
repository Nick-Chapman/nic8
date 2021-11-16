;;; Explore fib example...
;;; (1) coded using normal control stack
;;; (2) coded in CPS style, using heap

    .org $fffc
    .word reset_main
    .word ticks_irq

    .org $8000

g_divisor = $51 ; 2 bytes
g_mod10 = $53 ; 2 bytes

g_arg = $70 ; BYTE n
g_res = $71 ; WORD (fib n)
g_ticks = $73
g_screen_pointer = $74
g_sleep_ticks = $75

g_screen = $200 ; 32 bytes

    include via.s
    include ticks.s
    include lcd.s
    include screen.s

reset_main:
    jsr init_via
    jsr init_ticks
    jsr init_lcd
    jsr lcd_clear_display
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
    jsr fib1 ; code under test!
    jsr put_res
    jsr print_screen ;hmm
    pla
    clc
    adc #1
    jmp example_loop

pause:
    pha
    lda #50
    jsr sleep_blocking
    pla
    rts

    include fib1.s ; TODO: move to top

put_arg:
    lda g_arg
    jsr put_byte_a_in_decimal
    lda #'-'
    jsr scrolling_putchar
    rts

put_res:
    lda g_res
    ldx g_res + 1
    jsr decimal_put_word
    lda #' '
    jsr scrolling_putchar
    rts

put_byte_a_in_decimal:
    ldx #0
    jmp decimal_put_word


digits: .ascii "0123456789abcdef" ; TODO: remove

;;; TODO: move to screen.s & rename screen_putchar
scrolling_putchar:
    jsr maybe_scroll
    jmp screen_putchar_raw

    include sleep.s ; TODO: move to top

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

    include decimal.s  ; TODO: move to top
