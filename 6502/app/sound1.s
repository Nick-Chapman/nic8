
;;; first attempts to drive 76489 sound chip

    org $fffc
    word main_reset
    word deprecated_ticks_irq

    org $8000

g_ticks = $50

    include via.s
    include ticks.s
    include lcd.s
    include sound.s
    include sleep.s

main_reset:
    jsr init_via
    jsr lcd.init
    jsr sound.init
    jsr init_ticks

    lda #'!'
    jsr message
    jsr pause

    lda #'X'
    jsr message
    jsr silence_all
loop:
    jsr pause

    lda #'D'
    jsr message
    jsr loud_0
    jsr tone_d
    jsr pause

    lda #'E'
    jsr message
    jsr tone_e
    jsr pause

    lda #'-'
    jsr message
    jsr silence_0
    jmp loop

pause:
    lda #100
    jmp sleep_blocking

message:
    pha
    jsr lcd.clear_display
    pla
    jsr lcd.putchar
    rts

tone_d:
    lda #$8a
    jsr sound.send_byte
    lda #$06
    jsr sound.send_byte
    rts

tone_e:
    lda #$8f
    jsr sound.send_byte
    lda #$05
    jsr sound.send_byte
    rts

silence_all:
    jsr silence_0
    jsr silence_1
    jsr silence_2
    jsr silence_3
    rts

silence_0:
    lda #$9f
    jsr sound.send_byte
    rts

silence_1:
    lda #$bf
    jsr sound.send_byte
    rts

silence_2:
    lda #$df
    jsr sound.send_byte
    rts

silence_3:
    lda #$ff
    jsr sound.send_byte
    rts

loud_0:
    lda #$90
    jsr sound.send_byte
    rts
