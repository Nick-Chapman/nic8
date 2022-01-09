;;; REQUIRES: g_divisor24 (trip), g_modulus24 (trip)
;;; PROVIDES: decimal_put_trip

;;; decode 24 bit triple-byte pased in A/X/Y as decimal number
;;; put string to screen

generic_decimal_put_trip:
    sta g_divisor24 ;lo
    stx g_divisor24 + 1 ;med
    sty g_divisor24 + 2 ;hi
    lda #0
    pha ; marker for print
.each_digit:
    lda #0
    sta g_modulus24
    sta g_modulus24 + 1
    sta g_modulus24 + 2
    clc
    ldx #24
.each_bit:
    rol g_divisor24
    rol g_divisor24 + 1
    rol g_divisor24 + 2
    rol g_modulus24
    rol g_modulus24 + 1
    rol g_modulus24 + 2
    sec
    lda g_modulus24
    sbc #10
    pha ;save
    lda g_modulus24 + 1
    sbc #0
    pha ;save
    lda g_modulus24 + 2
    sbc #0
    bcc .ignore_result
    sta g_modulus24 + 2
    pla
    sta g_modulus24 + 1
    pla
    sta g_modulus24
    bra .after_ignore
.ignore_result:
    pla ;drop
    pla ;drop
.after_ignore
    dex
    bne .each_bit
    rol g_divisor24
    rol g_divisor24 + 1
    rol g_divisor24 + 2
    clc
    lda g_modulus24
    adc #'0'
    pha ;save on stack for reverse print
    lda g_divisor24
    ora g_divisor24 + 1
    ora g_divisor24 + 2
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
