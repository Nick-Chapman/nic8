
;;; This file is deprecated in favour of interrupts.s

MHz = 1000000

cpu_clks_per_sec = 4 * MHz

ticks_per_sec = 100

clks_per_tick = (cpu_clks_per_sec / ticks_per_sec - 2)

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
