
;;; Initialize and print to the LCD screen, in 4 bit mode.

lcd:

;;; LCD control bits on port B.
;;; PortB bits: ddddEccX -- 4 data bits; enable bit; 2 control bits.
;;; The final bit is not for the LCD. It's the active-low enable for
;;; the sound chip. We make sure it is always set high here.

.enable  = %00001000
.RW = %00000100                  ; 1-read
.RS = %00000010                  ; 1-data

.sound_clock_inactive = 1

.send_portB: ; TODO: better as macro?
    ora #(.sound_clock_inactive)
    sta PORTB
    rts

.putchar:
    pha
    and #%11110000
    jsr .wait
    ora #(.RS)
    jsr .send_portB                   ; E lo
    ora #(.enable)
    jsr .send_portB                   ; E hi
    and #(~ .enable)
    jsr .send_portB                   ; E lo again (necessary?)
    pla
    asl
    asl
    asl
    asl
    ora #(.RS)
    jsr .send_portB                   ; E lo
    ora #(.enable)
    jsr .send_portB                   ; E hi
    and #(~ .enable)
    jsr .send_portB                   ; E lo again
    rts

.clear_display: ;(0000 0001)
    pha
    jsr .wait
    lda #%00000000
    jsr .send_command_nibble
    lda #%00010000
    jsr .send_command_nibble
    pla
    rts

.return_home: ;(0000 001x)
    pha
    jsr .wait
    lda #%00000000
    jsr .send_command_nibble
    lda #%00100000
    jsr .send_command_nibble
    pla
    rts

.init: ; TODO: move to top!
    lda #%11111111
    sta DDRB
    ;; (from 8 bit mode) function set: 4 bit
    jsr .wait
    lda #%00100000
    jsr .send_command_nibble
    ;; function set: 4 bit(L=0); 2 lines(N=1), 5x8 (001L NFxx)
    jsr .wait
    lda #%00100000
    jsr .send_command_nibble
    lda #%10000000
    jsr .send_command_nibble
    ;; turn on display(D=1); no cursor(C=0); no blinking(B=0) (0000 1DCB)
    jsr .wait
    lda #%00000000
    jsr .send_command_nibble
    lda #%11000000
    jsr .send_command_nibble
    ;; entry mode: increment(I=1); no shift(S=0) (0000 01IS)
    jsr .wait
    lda #%00000000
    jsr .send_command_nibble
    lda #%01100000
    jsr .send_command_nibble
    rts

;;; TODO: macro to make 2x call of .send_command_nibble, given a literal byte

.send_command_nibble:
    jsr .send_portB
    ora #(.enable)
    jsr .send_portB
    and #(~ .enable)
    jsr .send_portB
    rts

.wait:
    pha
    lda #%00001111              ; temp set read for D-4,5,6,7
    sta DDRB
.wait_loop:
    lda #(.RW)
    jsr .send_portB
    lda #(.RW | .enable)
    jsr .send_portB
    lda PORTB
    pha
    lda #(.RW)
    jsr .send_portB
    lda #(.RW | .enable)
    jsr .send_portB
    lda PORTB
    pla
    and #%10000000
    bne .wait_loop
    lda #%11111111              ; revert to write
    sta DDRB
    pla
    rts
