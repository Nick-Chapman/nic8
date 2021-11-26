
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


put_hex_byte: ; TODO: review this code. wrote it a long time ago!
    pha
    lsr
    lsr
    lsr
    lsr
    tax
    lda digits,x
    jsr screen_putchar
    pla
    and #%1111
    tax
    lda digits,x
    jsr screen_putchar
    rts

digits: ascii "0123456789abcdef"

print_hex_word: macro L
    lda #'['
    jsr screen_putchar
    lda \L + 1
    jsr put_hex_byte
    lda \L
    jsr put_hex_byte
    lda #']'
    jsr screen_putchar
    jsr print_screen
endmac
