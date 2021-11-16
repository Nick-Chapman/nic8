;;; PROVIDES: PORTB, DDRB, init_via

;;; TODO: also define here and init PORTA/DDRB

PORTB = $6000
DDRB = $6002
init_via:
    lda #%11111111
    sta DDRB
    rts
