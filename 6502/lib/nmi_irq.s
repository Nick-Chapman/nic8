;;; REQUIRES: g_ticks, g_nmi_blocked, g_nmi_count
;;; PROVIDES: nmi, irq, init_nmi

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
