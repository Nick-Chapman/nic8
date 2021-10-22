
;;; routines to initialize timer on 6522, and maintain ticks in irq

T1CL = $6004
T1CH = $6005

ACR = $600B
IER = $600E



irq:
    bit T1CL ; acknowledge interrupt
    inc ticks
    rti

init_timer:
    cli
    lda #0
    sta ticks
    lda #%01000000              ; continuous interrupts from Timer1
    sta ACR
    lda #$1e                    ; 2 + 4E1E hex = 20000 decimal (10ms on 2Mhx clock)
    sta T1CL
    lda #$4e
    sta T1CH
    lda #%11000000              ; enable Timer1 interrupt
    sta IER
    rts
