print_char: macro CHAR
    pha
    lda #\CHAR
    jsr screen_putchar
    jsr print_screen
    ;jsr tiny_pause
    pla
endmac

print_decimal_word: macro L
    pha
    phx
    lda \L
    ldx \L + 1
    jsr decimal_put_word
    jsr print_screen
    plx
    pla
endmacro
