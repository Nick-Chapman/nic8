;;; REQUIRES: g_ticks
;;; PROVIDES: init_ticks, ticks_irq

;;; TODO: deprecate/merge this into nmi_irq.s

;;; TODO: move these defs to via.s ; better still, merge this file into via.s
T1CL = $6004
T1CH = $6005

ACR = $600B
IER = $600E

mHz = 1000000

clks_per_sec = 4 * mHz
ticks_per_sec = 100

clks_per_tick = (clks_per_sec / ticks_per_sec - 2)

ticks_irq:
    bit T1CL ; acknowledge interrupt
    inc g_ticks
    rti

init_ticks:
    cli
    lda #0
    sta g_ticks
    lda #%01000000 ; continuous interrupts from Timer1
    sta ACR
    lda #<clks_per_tick
    sta T1CL
    lda #>clks_per_tick
    sta T1CH
    lda #%11000000 ; enable Timer1 interrupt
    sta IER
    rts
