
;;; routines to initialize and print to the LCD screen, in 4 bit mode

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
