
    ;; first 6502 code for my Ben Eater style 6502 machine

via.PORTB = $6000
via.DDRB = $6002

    org $8000

reset:
    lda #$ff
    sta via.DDRB
    lda #7
loop:
    sta via.PORTB
    ror
    jmp loop


    org $fffc
    word reset
    word 0
