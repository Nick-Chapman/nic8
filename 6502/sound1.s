
;;; first attempts to drive 76489 sound chip

    .org $fffc
    .word reset
    .word ticks_irq

    .org $8000

g_ticks = $A0 ; maintained by irq; +1 every 10ms
    include ticks.s
    include lcd.s

PORTB = $6000 ; LSB bit is 76489 control
PORTA = $6001 ; 76489 data
DDRB = $6002
DDRA = $6003

    include sound.s

message:
    pha
    jsr lcd_clear_display
    pla
    jsr lcd_putchar
    rts

reset:
    lda #%11111111
    sta DDRA
    lda #%11111111
    sta DDRB

    jsr init_lcd
    jsr init_sound
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

tone_d:
    lda #$8a
    jsr sound_send_data
    lda #$06
    jsr sound_send_data
    rts

tone_e:
    lda #$8f
    jsr sound_send_data
    lda #$05
    jsr sound_send_data
    rts

silence_all:
    jsr silence_0
    jsr silence_1
    jsr silence_2
    jsr silence_3
    rts

silence_0:
    lda #$9f
    jsr sound_send_data
    rts

silence_1:
    lda #$bf
    jsr sound_send_data
    rts

silence_2:
    lda #$df
    jsr sound_send_data
    rts

silence_3:
    lda #$ff
    jsr sound_send_data
    rts

loud_0:
    lda #$90
    jsr sound_send_data
    rts

last_message_ticks = $A2
pause:
    lda g_ticks
    sta last_message_ticks
keep_waiting:
    sec
    lda g_ticks
    sbc last_message_ticks
    cmp #100
    bcc keep_waiting
    rts
