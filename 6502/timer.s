
;;; Example of using VIA timer & interrupts

    .org $fffc
    .word reset
    .word ticks_irq

number = $A1 ; maintained in main loop; +1 every 1/2s
last_message_ticks = $A2

    .org $8000

PORTB = $6000 ; 7 MSBs for lcd
DDRB = $6002

    include lcd.s

g_ticks = $A0 ; maintained by irq; +1 every 10ms
    include ticks.s

reset:
    jsr init_ticks
    jsr init_lcd
    lda #$42
    sta number
    jsr print_number
loop:
    jsr wait_for_half_second
    inc number
    jsr print_number
    jmp loop

wait_for_half_second:
    lda g_ticks
    sta last_message_ticks
keep_waiting:
    sec
    lda g_ticks
    sbc last_message_ticks
    cmp #50
    bcc keep_waiting
    rts

print_number:
    jsr lcd_clear_display
    lda #"x"
    jsr lcd_putchar
    lda number
    lsr
    lsr
    lsr
    lsr
    tax
    lda digits,x
    jsr lcd_putchar
    lda number
    and #%1111
    tax
    lda digits,x
    jsr lcd_putchar
    rts

digits: .ascii "0123456789abcdef"
