;;; REQUIRES: PORTA, PORTB
;;; PROVIDES: init_sound, sound_send_data

;;; Send a data byte to the 76489 sound chip

init_sound:
    lda #1
    sta PORTB
    jsr sound_silence
    rts

sound_silence:
    lda #$9f
    jsr sound_send_data
    lda #$bf
    jsr sound_send_data
    lda #$df
    jsr sound_send_data
    lda #$ff
    jsr sound_send_data
    rts

sound_send_data:
    sta PORTA ; set the data, now strobe WE-bar

    ;; We need to add some nops to delay between the negative and positive edge of strobe on WE-bar.
    ;; The datasheet states:
    ;;   "The SN76489AN requires approximately 32 clock cycles to load the data into the control register"
    ;; Experimentally I have found that just 10 nops are sufficient (while 9 is not enough).
    ;; However, the numbers don't quite add up!
    ;; -  9 nop, #cycles between edges = 6 +  9*2 = 24
    ;; - 10 nop, #cycles between edges = 6 + 10*2 = 26
    ;; odd

    lda #0
    sta PORTB ; (negative edge)
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
    sta PORTB ; 4 cycles (positive edge)
    rts
