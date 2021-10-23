
;;; routines to initialize timer on 6522, and maintain ticks in irq

T1CL = $6004
T1CH = $6005

ACR = $600B
IER = $600E

mHz = 1000000

clks_per_sec = 4 * mHz
ticks_per_sec = 100

clks_per_tick = (clks_per_sec / ticks_per_sec - 2)

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
    lda #(clks_per_tick & $ff)
    sta T1CL
    lda #(clks_per_tick >> 8)
    sta T1CH
    lda #%11000000              ; enable Timer1 interrupt
    sta IER
    rts
