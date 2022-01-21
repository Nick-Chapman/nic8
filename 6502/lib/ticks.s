
;;; TODO: deprecate/merge this into nmi_irq.s
;;; TODO: rename ticks -> jiffy
;;; TODO: move into via.s

MHz = 1000000

cpu_clks_per_sec = 4 * MHz

ticks_per_sec = 100

clks_per_tick = (cpu_clks_per_sec / ticks_per_sec - 2)

deprecated_ticks_irq: ; TODO: switch callers to use nmi-aware irq (in nmi_irq.s)
    bit via.T1CL ; acknowledge interrupt
    inc g_ticks
    rti

init_ticks:
    cli
    lda #0
    sta g_ticks
    lda #%01000000 ; continuous interrupts from Timer1
    sta via.ACR
    lda #<clks_per_tick
    sta via.T1CL
    lda #>clks_per_tick
    sta via.T1CH
    lda #%11000000 ; enable Timer1 interrupt
    sta via.IER
    rts
