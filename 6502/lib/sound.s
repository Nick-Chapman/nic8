
;;; Control the 76489 sound chip

sound:

.init:
    lda #1
    sta via.PORTB
    jsr .silence
    rts

.silence:
    lda #$9f
    jsr .send_byte
    lda #$bf
    jsr .send_byte
    lda #$df
    jsr .send_byte
    lda #$ff
    jsr .send_byte
    rts

.send_byte:
    sta via.PORTA ; set the data, now strobe WE-bar
    ;; We need some nops to delay between the negative and positive edge of the strobe on WE-bar.
    ;; The datasheet states:
    ;;   "The SN76489AN requires approximately 32 clock cycles to load the data into the control register"
    ;; Experimentally I have found that just 10 nops are sufficient (while 9 is not enough).
    ;; However, the numbers don't quite add up!
    ;; -  9 nop, #cycles between edges = 6 +  9*2 = 24
    ;; - 10 nop, #cycles between edges = 6 + 10*2 = 26
    lda #0
    sta via.PORTB ; (negative edge)
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
    lda #1    ; 2 cycles
    sta via.PORTB ; 4 cycles (positive edge)
    rts
