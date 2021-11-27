
    org $fffc
    word reset
    word 0

    org $8000

PORTB = $6000 ; 7 MSBs for lcd
DDRB = $6002

    include lcd.s

reset:
    jsr init_lcd
    jsr lcd_clear_display
    jsr send_message1
    jsr pause
    jsr lcd_clear_display
    jsr send_message2
spin:
    jmp spin

send_message1:
    lda #'H'
    jsr lcd_putchar
    lda #'e'
    jsr lcd_putchar
    lda #'l'
    jsr lcd_putchar
    lda #'l'
    jsr lcd_putchar
    lda #'o'
    jsr lcd_putchar
    lda #','
    jsr lcd_putchar
    lda #' '
    jsr lcd_putchar
    lda #'W'
    jsr lcd_putchar
    lda #'o'
    jsr lcd_putchar
    lda #'r'
    jsr lcd_putchar
    lda #'l'
    jsr lcd_putchar
    lda #'d'
    jsr lcd_putchar
    lda #'!'
    jsr lcd_putchar
    rts

send_message2:
    lda #'T'
    jsr lcd_putchar
    lda #'h'
    jsr lcd_putchar
    lda #'i'
    jsr lcd_putchar
    lda #'s'
    jsr lcd_putchar
    lda #' '
    jsr lcd_putchar
    lda #'i'
    jsr lcd_putchar
    lda #'s'
    jsr lcd_putchar
    lda #' '
    jsr lcd_putchar
    lda #'f'
    jsr lcd_putchar
    lda #'u'
    jsr lcd_putchar
    lda #'n'
    jsr lcd_putchar
    lda #'!'
    jsr lcd_putchar
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
