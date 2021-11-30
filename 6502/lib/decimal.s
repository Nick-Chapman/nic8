;;; REQUIRES: g_divisor (word), g_mod10 (word)
;;; PROVIDES: decimal_put_word, decimal_put_byte

;;; decode 16 bit word pased in A/X as 1-5 digit decimal number
;;; put string to screen

decimal_put_word:
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
    jsr screen_putchar
    jmp .put_from_stack
.done:
    rts

decimal_put_byte:
    ldx #0
    jmp decimal_put_word
