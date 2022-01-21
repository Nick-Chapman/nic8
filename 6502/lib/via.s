
;;; Versatile interface adaptor (VIA) 6522

via:

.PORTB = $6000 ; (7 MSBs for lcd); LSB is sound control bit
.PORTA = $6001 ; 76489 data
.DDRB  = $6002
.DDRA  = $6003
.T1CL  = $6004
.T1CH  = $6005
;;;    = $6006
;;;    = $6007
.T2CL  = $6008
.T2CH  = $6009
;;;    = $600A
.ACR   = $600B
;;;    = $600C
.IFR   = $600D
.IER   = $600E
;;;    = $600F

.timer2_bit_mask = %00100000

.init:
    lda #%11111111
    sta .DDRB
    sta .DDRA
    rts
