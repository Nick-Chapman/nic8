
;;; TODO: deprecate/merge this into nmi_irq.s
;;; TODO: rename ticks -> jiffy
;;; TODO: move into via.s

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

nmi:
    pha
    lda g_nmi_blocked
    bne .done
    lda #25 ; debounce time
    sta g_nmi_blocked
    inc g_nmi_count
.done:
    pla
    rti

irq: ; copy & extend version in ticks.s
    pha
    bit via.T1CL ; acknowledge interrupt
    inc g_ticks
    lda g_nmi_blocked
    beq .done
    dec g_nmi_blocked
.done:
    pla
    rti

init_nmi_irq: ; TODO: combine with (ticks)init_ticks
    stz g_nmi_blocked
    stz g_nmi_count
    rts
