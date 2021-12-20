;;; REQUIRES: g_ticks
;;; PROVIDES: init_ticks, ticks_irq
;;; TODO: deprecate/merge this into nmi_irq.s

T1CL = $6004
T1CH = $6005
ACR = $600B
IER = $600E

MHz = 1000000

ticks_per_sec = 100

clks_per_tick = (cpu_clks_per_sec / ticks_per_sec - 2)

deprecated_ticks_irq: ; TODO: switch callers to use nmi-aware irq (in nmi_irq.s)
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
