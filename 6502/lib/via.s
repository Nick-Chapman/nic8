;;; PROVIDES: PORT{A,B}, DDR{A,B}, init_via

PORTB = $6000 ; (7 MSBs for lcd); LSB is sound control bit
PORTA = $6001 ; 76489 data
DDRB = $6002
DDRA = $6003

init_via:
    lda #%11111111
    sta DDRB
    sta DDRA
    rts
