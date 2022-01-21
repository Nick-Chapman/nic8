
    org $fffc
    word reset
    word 0

    org $8000

via.PORTB = $6000 ; 7 MSBs for lcd
via.DDRB = $6002

    include lcd.s

reset:
    jsr lcd.init
    jsr lcd.clear_display
    jsr send_message1
    jsr pause
    jsr lcd.clear_display
    jsr send_message2
spin:
    jmp spin

send_message1:
    lda #'H'
    jsr lcd.putchar
    lda #'e'
    jsr lcd.putchar
    lda #'l'
    jsr lcd.putchar
    lda #'l'
    jsr lcd.putchar
    lda #'o'
    jsr lcd.putchar
    lda #','
    jsr lcd.putchar
    lda #' '
    jsr lcd.putchar
    lda #'W'
    jsr lcd.putchar
    lda #'o'
    jsr lcd.putchar
    lda #'r'
    jsr lcd.putchar
    lda #'l'
    jsr lcd.putchar
    lda #'d'
    jsr lcd.putchar
    lda #'!'
    jsr lcd.putchar
    rts

send_message2:
    lda #'T'
    jsr lcd.putchar
    lda #'h'
    jsr lcd.putchar
    lda #'i'
    jsr lcd.putchar
    lda #'s'
    jsr lcd.putchar
    lda #' '
    jsr lcd.putchar
    lda #'i'
    jsr lcd.putchar
    lda #'s'
    jsr lcd.putchar
    lda #' '
    jsr lcd.putchar
    lda #'f'
    jsr lcd.putchar
    lda #'u'
    jsr lcd.putchar
    lda #'n'
    jsr lcd.putchar
    lda #'!'
    jsr lcd.putchar
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
