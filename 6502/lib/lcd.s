;;; REQUIRES: DDRB, PORTB
;;; PROVIDES: init_lcd, lcd_clear_display, lcd_return_home, lcd_putchar

;;; routines to initialize and print to the LCD screen, in 4 bit mode

;;; LCD control bits on port B
E  = %00001000
RW = %00000100                  ; 1-read
RS = %00000010                  ; 1-data

NOT_E = %11110111

;;; PortB bits: ddddEccX -- 4 data bits; enable bit; 2 control bits

;;; The final bit is not for the LCD. It's the active-low enable for
;;; the sound chip. We make sure it is always set high here.

SOUND_CLOCK_INACTIVE = 1

send_portB:
    ora #(SOUND_CLOCK_INACTIVE)
    sta PORTB
    rts

lcd_putchar:
    pha
    and #%11110000
    jsr wait_lcd
    ora #(RS)
    jsr send_portB                   ; E lo
    ora #(E)
    jsr send_portB                   ; E hi
    and #(NOT_E)
    jsr send_portB                   ; E lo again (necessary?)
    pla
    asl
    asl
    asl
    asl
    ora #(RS)
    jsr send_portB                   ; E lo
    ora #(E)
    jsr send_portB                   ; E hi
    and #(NOT_E)
    jsr send_portB                   ; E lo again
    rts

lcd_clear_display: ;(0000 0001)
    pha
    jsr wait_lcd
    lda #%00000000
    jsr send_lcd_command_nibble
    lda #%00010000
    jsr send_lcd_command_nibble
    pla
    rts

lcd_return_home: ;(0000 001x)
    pha
    jsr wait_lcd
    lda #%00000000
    jsr send_lcd_command_nibble
    lda #%00100000
    jsr send_lcd_command_nibble
    pla
    rts

init_lcd:
    lda #%11111111
    sta DDRB
    ;; (from 8 bit mode) function set: 4 bit
    jsr wait_lcd
    lda #%00100000
    jsr send_lcd_command_nibble

    ;; function set: 4 bit(L=0); 2 lines(N=1), 5x8 (001L NFxx)
    jsr wait_lcd
    lda #%00100000
    jsr send_lcd_command_nibble
    lda #%10000000
    jsr send_lcd_command_nibble
    ;; turn on display(D=1); no cursor(C=0); no blinking(B=0) (0000 1DCB)
    jsr wait_lcd
    lda #%00000000
    jsr send_lcd_command_nibble
    lda #%11000000
    jsr send_lcd_command_nibble
    ;; entry mode: increment(I=1); no shift(S=0) (0000 01IS)
    jsr wait_lcd
    lda #%00000000
    jsr send_lcd_command_nibble
    lda #%01100000
    jsr send_lcd_command_nibble
    rts

;;; TODO: subroutine to make 2x call of send_lcd_command_nibble, given a byte in acc
send_lcd_command_nibble:
    jsr send_portB
    ora #(E)
    jsr send_portB
    and #(NOT_E)
    jsr send_portB
    rts

wait_lcd:
    pha
    lda #%00001111              ; temp set read for D-4,5,6,7
    sta DDRB
wait_lcd_loop:
    lda #(RW)
    jsr send_portB
    lda #(RW | E)
    jsr send_portB
    lda PORTB
    pha
    lda #(RW)
    jsr send_portB
    lda #(RW | E)
    jsr send_portB
    lda PORTB
    pla
    and #%10000000
    bne wait_lcd_loop
    lda #%11111111              ; revert to write
    sta DDRB
    pla
    rts
