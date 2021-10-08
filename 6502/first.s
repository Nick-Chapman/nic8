
    ;; first 6502 code for my Ben Eater style 6502 machine

PORTB = $6000
DDRB = $6002

    .org $8000

reset:
    lda $ff
    sta DDRB
    lda #7
loop:
    sta PORTB
    ror
    jmp loop


    .org $fffc
    .word reset
    .word 0
