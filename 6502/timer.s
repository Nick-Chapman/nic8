
;;; Example of using VIA timer & interrupts

    .org $fffc
    .word reset
    .word irq

number = $A1                    ; maintained in main loop; +1 every 1/2s
last_message_ticks = $A2

    .org $8000

    include lcd.s
    include ticks.s

reset:
    jsr init_timer
    jsr init_display
    lda #$42
    sta number
    jsr print_number
loop:
    jsr wait_for_half_second
    inc number
    jsr print_number
    jmp loop

wait_for_half_second:
    lda ticks
    sta last_message_ticks
keep_waiting:
    sec
    lda ticks
    sbc last_message_ticks
    cmp #50
    bcc keep_waiting
    rts

print_number:
    jsr clear_display
    lda #"x"
    jsr print_char
    lda number
    lsr
    lsr
    lsr
    lsr
    tax
    lda digits,x
    jsr print_char
    lda number
    and #%1111
    tax
    lda digits,x
    jsr print_char
    rts

digits: .ascii "0123456789abcdef"
