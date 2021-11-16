;;; Explore fib example...
;;; (1) coded using normal control stack -- DONE
;;; (2) coded in CPS style, using heap -- TODO

    .org $fffc
    .word reset_main
    .word ticks_irq

    .org $8000

;;; bytes
g_arg = $50
g_ticks = $51
g_sleep_ticks = $52
g_screen_pointer = $53

;;; words
g_divisor = $70
g_mod10 = $72
g_res = $74

;;; buffers
g_screen = $200 ; 32 bytes

    include via.s
    include ticks.s
    include lcd.s
    include screen.s
    include sleep.s
    include decimal.s
    include fib1.s

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
    pha
    sta g_arg
    jsr put_arg
    jsr print_screen
    jsr fib1 ; code under test!
    jsr put_res
    jsr print_screen
    jsr pause
;    pla
;    pha
;    and #1
;    beq after_newline ; no newline when n = 0,2,4...
    jsr screen_newline
;after_newline:
    jsr print_screen
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

put_arg:
    lda g_arg
    jsr decimal_put_byte
    lda #'-'
    jsr screen_putchar
    rts

put_res:
    lda g_res
    ldx g_res + 1
    jsr decimal_put_word
    lda #' '
    jsr screen_putchar
    rts
