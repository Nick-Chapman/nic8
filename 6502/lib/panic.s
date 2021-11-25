
;;; TODO: move this panic macro to a new file; take a string, not just a char

panic_stop:
    pha
    lda #'!'
    jsr screen_putchar
    pla
    jsr screen_putchar
    jsr print_screen
panic_spin:
    jmp panic_spin

panic: macro CHAR
    lda #\CHAR
    jmp panic_stop
endmac
