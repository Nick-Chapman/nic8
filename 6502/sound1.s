
;;; first attempts to drive 76489 sound chip

    .org $fffc
    .word reset
    .word irq

    .org $8000

ticks = $A0                     ; maintained by irq; +1 every 10ms
    include ticks.s
    include lcd.s

PORTB = $6000                   ; LSB bit is 76489 control
PORTA = $6001                   ; 76489 data
DDRB = $6002
DDRA = $6003

    include 76489.s

message:
    pha
    jsr clear_display
    pla
    jsr print_char
    rts

reset:
    lda #%11111111
    sta DDRA
    lda #%11111111
    sta DDRB

    jsr init_display

    jsr init_sound

    jsr init_timer
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
    lda #%01010001
    jsr send_sound_data
    lda #%01100000
    jsr send_sound_data
    rts

tone_e:
    lda #%11110001
    jsr send_sound_data
    lda #%10100000
    jsr send_sound_data
    rts

silence_all:
    jsr silence_0
    jsr silence_1
    jsr silence_2
    jsr silence_3
    rts

silence_0:
    lda #%11111001
    jsr send_sound_data
    rts

silence_1:
    lda #%11111101
    jsr send_sound_data
    rts

silence_2:
    lda #%11111011
    jsr send_sound_data
    rts

silence_3:
    lda #%11111111
    jsr send_sound_data
    rts

loud_0:
    lda #%00001001
    jsr send_sound_data
    rts

last_message_ticks = $A2
pause:
    lda ticks
    sta last_message_ticks
keep_waiting:
    sec
    lda ticks
    sbc last_message_ticks
    cmp #100
    bcc keep_waiting
    rts
