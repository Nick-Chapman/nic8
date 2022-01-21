
decimal16:

;;; decode 16 bit word passed in A/X as digit decimal number
;;; put string to screen

.screen_put:
    pha
    store16i screen.putchar, g_putchar
    pla
    jmp .generic_put

;;; decode 16 bit word passed in A/X as digit decimal number
;;; put string dispatching via g_putchar

.generic_put:
    sta g_divisor ;lo
    stx g_divisor + 1 ;hi
    lda #0
    pha ; marker for print
.each_digit:
    lda #0
    sta g_mod10
    sta g_mod10 + 1
    clc
    ldx #16
.each_bit:
    rol g_divisor
    rol g_divisor + 1
    rol g_mod10
    rol g_mod10 + 1
    sec
    lda g_mod10
    sbc #10
    pha ; save
    lda g_mod10 + 1
    sbc #0
    bcc .ignore_result
    sta g_mod10 + 1
    pla
    pha
    sta g_mod10
.ignore_result:
    pla ;drop
    dex
    bne .each_bit
    rol g_divisor
    rol g_divisor + 1
    clc
    lda g_mod10
    adc #'0'
    pha ;save on stack for reverse print
    lda g_divisor
    ora g_divisor + 1
    bne .each_digit
.put_from_stack:
    pla
    beq .done
    jsr .put
    jmp .put_from_stack
.done:
    rts
.put:
    jmp (g_putchar)
