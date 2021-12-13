
newline: macro
    phy
    jsr screen_newline
    ply
endmacro

print_char: macro CHAR
    pha
    lda #\CHAR
    jsr screen_putchar
    pla
endmacro

print_decimal_trip: macro L ; TODO: move macros to same file as the subs they call
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

print_decimal_word_x: macro L
    pha
    ldy \L, x
    lda \L + 1, x
    phx
    tax
    tya
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

print_decimal_byte_x: macro L
    pha
    lda \L, x
    phx
    ldx #0
    jsr decimal_put_word
    plx
    pla
endmacro

put_hex_byte: ; TODO: save/restore x
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


put_string:
    phx
    tsx
    lda $104,x ; string-pointer-word (under return-address-word, and saved x)
    sta g_mptr
    lda $105,x
    sta g_mptr + 1
    ldy #0
.loop:
    lda (g_mptr),y
    beq .done
    phy
      jsr screen_putchar ; changes y
    ply
    iny
    jmp .loop
.done:
    plx
    rts

print_string: macro S
    jmp .skip\@
.embedded\@:
    string \S
.skip\@:
    pha
      lda #>.embedded\@
      pha
      lda #<.embedded\@
      pha
      jsr put_string
      pla
      pla
    pla
endmacro
