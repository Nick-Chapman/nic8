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
    ;; experiment to see what nops are needed
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
    nop
    nop
    nop
    nop
    sta PORTA
    lda #0
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
    nop
    nop
    nop
    nop
    sta PORTB
    lda #1
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
    nop
    nop
    nop
    nop
    sta PORTB
    rts
