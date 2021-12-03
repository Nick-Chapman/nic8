
;;; Example of using VIA timer & interrupts

    org $fffc
    word main_reset
    word deprecated_ticks_irq

    org $8000

g_number = $A0
g_ticks = $A1
g_last_message_ticks = $A2

    include via.s
    include lcd.s
    include ticks.s

main_reset:
    jsr init_ticks
    jsr init_lcd
    lda #$42
    sta g_number
    jsr print_number
loop:
    jsr wait_for_half_second
    inc g_number
    jsr print_number
    jmp loop

wait_for_half_second:
    lda g_ticks
    sta g_last_message_ticks
keep_waiting:
    sec
    lda g_ticks
    sbc g_last_message_ticks
    cmp #50
    bcc keep_waiting
    rts

print_number:
    jsr lcd_clear_display
    lda #"x"
    jsr lcd_putchar
    lda g_number
    lsr
    lsr
    lsr
    lsr
    tax
    lda digits,x
    jsr lcd_putchar
    lda g_number
    and #%1111
    tax
    lda digits,x
    jsr lcd_putchar
    rts

digits: ascii "0123456789abcdef"
