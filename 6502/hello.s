
    .org $fffc
    .word reset
    .word 0

    .org $8000

PORTB = $6000 ; 7 MSBs for lcd
DDRB = $6002

    include lcd.s

reset:
    jsr init_display
    jsr clear_display
    jsr send_message1
    jsr pause
    jsr clear_display
    jsr send_message2
spin:
    jmp spin

send_message1:
    lda #'H'
    jsr print_char
    lda #'e'
    jsr print_char
    lda #'l'
    jsr print_char
    lda #'l'
    jsr print_char
    lda #'o'
    jsr print_char
    lda #','
    jsr print_char
    lda #' '
    jsr print_char
    lda #'W'
    jsr print_char
    lda #'o'
    jsr print_char
    lda #'r'
    jsr print_char
    lda #'l'
    jsr print_char
    lda #'d'
    jsr print_char
    lda #'!'
    jsr print_char
    rts

send_message2:
    lda #'T'
    jsr print_char
    lda #'h'
    jsr print_char
    lda #'i'
    jsr print_char
    lda #'s'
    jsr print_char
    lda #' '
    jsr print_char
    lda #'i'
    jsr print_char
    lda #'s'
    jsr print_char
    lda #' '
    jsr print_char
    lda #'f'
    jsr print_char
    lda #'u'
    jsr print_char
    lda #'n'
    jsr print_char
    lda #'!'
    jsr print_char
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
