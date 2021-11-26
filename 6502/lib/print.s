
;;; macros for screen printing

print_char: macro CHAR
    pha
    lda #\CHAR
    jsr screen_putchar
    jsr print_screen ; TODO: move this flush call to a sep task
    pla
endmacro

print_decimal_word: macro L
    pha
    phx
    lda \L
    ldx \L + 1
    jsr decimal_put_word
    jsr print_screen ; TODO: move this flush call to a sep task
    plx
    pla
endmacro

