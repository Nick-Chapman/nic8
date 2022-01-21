
T1CL = $6004
T1CH = $6005
ACR = $600B
IER = $600E

PORTB = $6000 ; (7 MSBs for lcd); LSB is sound control bit
PORTA = $6001 ; 76489 data
DDRB = $6002
DDRA = $6003

T2CL = $6008
T2CH = $6009
IFR = $600D

timer2_bit_mask = %00100000

init_via:
    lda #%11111111
    sta DDRB
    sta DDRA
    rts
