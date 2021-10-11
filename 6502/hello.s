
;;; First attempt to drive LCD screen...

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
    lda #$ff
    sta DDRB
    sta DDRA

    lda #%00111000               ; function set: 8 bit; 2 lines, 5x8
    sta PORTB
    lda #0
    sta PORTA
    lda #(E)
    sta PORTA
    lda #0
    sta PORTA

    lda #%00001111               ; turn on display and cursor blinking
    sta PORTB
    lda #0
    sta PORTA
    lda #(E)
    sta PORTA
    lda #0
    sta PORTA

    lda #%00000110               ; entry mode: increment; no shift
    sta PORTB
    lda #0
    sta PORTA
    lda #(E)
    sta PORTA
    lda #0
    sta PORTA

    lda #%00000001               ; clear display
    sta PORTB
    lda #0
    sta PORTA
    lda #(E)
    sta PORTA
    lda #0
    sta PORTA

    lda #'H'                    ; message starts here...
    sta PORTB
    lda #(RS)
    sta PORTA
    lda #(RS | E)
    sta PORTA
    lda #(RS)
    sta PORTA

    lda #'e'
    jsr send_lcd_data
    lda #'l'
    jsr send_lcd_data
    lda #'l'
    jsr send_lcd_data
    lda #'o'
    jsr send_lcd_data
    lda #','
    jsr send_lcd_data
    lda #' '
    jsr send_lcd_data
    lda #'W'
    jsr send_lcd_data
    lda #'o'
    jsr send_lcd_data
    lda #'r'
    jsr send_lcd_data
    lda #'l'
    jsr send_lcd_data
    lda #'d'
    jsr send_lcd_data
    lda #'!'
    jsr send_lcd_data

spin:
    jmp spin


send_lcd_data:
    sta PORTB
    lda #(RS)
    sta PORTA
    lda #(RS | E)
    sta PORTA
    lda #(RS)
    sta PORTA
    rts

    .org $fffc
    .word reset
    .word 0
