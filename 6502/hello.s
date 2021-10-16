
    .org $fffc
    .word reset
    .word 0

;;; VIA
PORTB = $6000                   ; LCD data
PORTA = $6001                   ; LCD control; using 3 bits
DDRB = $6002
DDRA = $6003

;;; LCD control bits on port A
E  = %10000000
RW = %01000000                  ; 1-read
RS = %00100000                  ; 1-data

    .org $8000

reset:
    jsr init_display
    jsr clear_display
    jsr send_message1
    jsr pause
    jsr clear_display
    jsr send_message2
spin:
    jmp spin

send_message1:
    lda #'H'
    jsr print_char
    lda #'e'
    jsr print_char
    lda #'l'
    jsr print_char
    lda #'l'
    jsr print_char
    lda #'o'
    jsr print_char
    lda #','
    jsr print_char
    lda #' '
    jsr print_char
    lda #'W'
    jsr print_char
    lda #'o'
    jsr print_char
    lda #'r'
    jsr print_char
    lda #'l'
    jsr print_char
    lda #'d'
    jsr print_char
    lda #'!'
    jsr print_char
    rts

send_message2:
    lda #'T'
    jsr print_char
    lda #'h'
    jsr print_char
    lda #'i'
    jsr print_char
    lda #'s'
    jsr print_char
    lda #' '
    jsr print_char
    lda #'i'
    jsr print_char
    lda #'s'
    jsr print_char
    lda #' '
    jsr print_char
    lda #'f'
    jsr print_char
    lda #'u'
    jsr print_char
    lda #'n'
    jsr print_char
    lda #'!'
    jsr print_char
    rts


pause:
    ldy #100                    ; 1 second
pause_loop:
    jsr pause_10000             ; for fast clock
    dey
    bne pause_loop
    rts

pause_10000:                    ; .01sec with the 1MHz clock
    pha
    txa
    pha
    ldx #250
pause_10000_loop:
    jsr pause_40
    dex
    bne pause_10000_loop
    pla
    tax
    pla
    rts

pause_40:                       ; 40 clocks .01sec with the 4KHz clock
    nop                         ; #clocks: nop:2, jsr:6, rts:6
    nop                         ; so we need 14 nops: 14*2 + 6 + 6 = 40
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    rts

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
