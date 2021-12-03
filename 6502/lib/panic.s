
;;; TODO: take a string, not just a char

panic: macro CHAR
    lda #\CHAR
    jmp panic_sub
endmacro

panic_sub:
    pha
    lda #'!'
    jsr screen_putchar
    pla
    jsr screen_putchar
    screen_flush_selected
.spin:
    jmp .spin
