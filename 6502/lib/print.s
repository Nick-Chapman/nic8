
;;; macros for screen printing

print_char: macro CHAR
    pha
    lda #\CHAR
    jsr screen_putchar
    pla
endmacro

print_decimal_trip: macro L ; TODO: should these macros be in same file as the subs they call?
    pha
    phx
    phy
    lda \L
    ldx \L + 1
    ldy \L + 2
    jsr decimal_put_trip
    ply
    plx
    pla
endmacro

print_decimal_word: macro L
    pha
    phx
    lda \L
    ldx \L + 1
    jsr decimal_put_word
    plx
    pla
endmacro

print_decimal_byte: macro L
    pha
    phx
    lda \L
    ldx #0
    jsr decimal_put_word
    plx
    pla
endmacro


put_hex_byte: ; TODO: review this code. wrote it a long time ago! - should it save/restore x/a?
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
endmac

print_hex_byte: macro L
    lda #'['
    jsr screen_putchar
    lda \L
    jsr put_hex_byte
    lda #']'
    jsr screen_putchar
endmac
