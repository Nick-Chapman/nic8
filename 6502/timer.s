
;;; Example of using VIA timer & interrupts

    .org $fffc
    .word reset
    .word irq

;;; VIA
PORTB = $6000                   ; LCD data
PORTA = $6001                   ; LCD control; using 3 bits
DDRB = $6002
DDRA = $6003

T1CL = $6004
T1CH = $6005

ACR = $600B
IER = $600E

;;; LCD control bits on port A
E  = %10000000
RW = %01000000                  ; 1-read
RS = %00100000                  ; 1-data

ticks = $A0                     ; maintained by irq; +1 every 10ms
number = $A1                    ; maintained in main loop; +1 every 1/2s
last_message_ticks = $A2

    .org $8000

irq:
    bit T1CL ; acknowledge interrupt
    inc ticks
    rti

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

init_timer:
    cli
    lda #0
    sta ticks
    lda #%01000000              ; continuous interrupts from Timer1
    sta ACR
    lda #$0e                    ; 2 + 270E hex = 10000 decimal (10ms)
    sta T1CL
    lda #$27
    sta T1CH
    lda #%11000000              ; enable Timer1 interrupt
    sta IER
    rts

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

print_char:
    pha
    and #%11110000
    jsr wait_lcd
    sta PORTB
    lda #(RS)
    sta PORTA
    lda #(RS | E)
    sta PORTA
    lda #(RS)
    sta PORTA
    pla
    asl
    asl
    asl
    asl
    sta PORTB
    lda #(RS)
    sta PORTA
    lda #(RS | E)
    sta PORTA
    lda #(RS)
    sta PORTA
    rts

clear_display:
    jsr wait_lcd
    lda #%00000000
    jsr send_lcd_command_nibble
    lda #%00010000
    jsr send_lcd_command_nibble
    rts

init_display:
    lda #%11111111
    sta DDRB
    sta DDRA
    ;; (from 8 bit mode) function set: 4 bit
    jsr wait_lcd
    lda #%00100000
    jsr send_lcd_command_nibble

    ;; function set: 4 bit; 2 lines, 5x8
    jsr wait_lcd
    lda #%00100000
    jsr send_lcd_command_nibble
    lda #%10000000
    jsr send_lcd_command_nibble
    ;; turn on display; cursor; no blinking
    jsr wait_lcd
    lda #%00000000
    jsr send_lcd_command_nibble
    lda #%11100000
    jsr send_lcd_command_nibble
    ;; entry mode: increment; no shift
    jsr wait_lcd
    lda #%00000000
    jsr send_lcd_command_nibble
    lda #%01100000
    jsr send_lcd_command_nibble
    rts

send_lcd_command_nibble:
    sta PORTB
    lda #0
    sta PORTA
    lda #(E)
    sta PORTA
    lda #0
    sta PORTA
    rts

wait_lcd:
    pha
    lda #%00000000              ; temp set read
    sta DDRB
wait_lcd_loop:
    lda #(RW)
    sta PORTA
    lda #(RW | E)
    sta PORTA
    lda PORTB
    pha
    lda #(RW)
    sta PORTA
    lda #(RW | E)
    sta PORTA
    lda PORTB
    pla
    and #%10000000
    bne wait_lcd_loop
    lda #%11111111              ; revert to write
    sta DDRB
    pla
    rts
