
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

generic_print_decimal_trip_x: macro P, L
    pha
    phx
    phy
    store16i \P, g_putchar
    lda \L + 1, x
    pha
    ldy \L + 2, x
    lda \L, x
    plx
    jsr generic_decimal_put_trip
    ply
    plx
    pla
endmacro

acia_print_decimal_trip_x: macro L
    generic_print_decimal_trip_x acia_putchar, \L
endmacro

print_decimal_trip_x: macro L
    generic_print_decimal_trip_x screen_putchar, \L
endmacro

generic_print_decimal_word: macro P, L
    pha
    phx
    store16i \P, g_putchar
    lda \L
    ldx \L + 1
    jsr generic_decimal_put_word
    plx
    pla
endmacro

acia_print_decimal_word: macro L
    generic_print_decimal_word acia_putchar, \L
endmacro

print_decimal_word: macro L
    generic_print_decimal_word screen_putchar, \L
endmacro

generic_print_decimal_word_x: macro P, L
    pha
    store16i \P, g_putchar
    ldy \L, x
    lda \L + 1, x
    phx
    tax
    tya
    jsr generic_decimal_put_word
    plx
    pla
endmacro

acia_print_decimal_word_x: macro L
    generic_print_decimal_word_x acia_putchar, \L
endmacro

print_decimal_word_x: macro L
    generic_print_decimal_word_x screen_putchar, \L
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

put_hex_byte:
    phx
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
    plx
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


;;; TODO: move these macros to screen; prefix with screen_

put_string: ; TODO: make this work for long strings (like acia_put_string)
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

print_string: macro S ; unify with acia version
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

print_string_variable: macro V
    pha
      lda \V + 1
      pha
      lda \V
      pha
      jsr put_string
      pla
      pla
    pla
endmacro

print_string_variable_x: macro V
    pha
      lda \V + 1, x
      pha
      lda \V, x
      pha
      jsr put_string
      pla
      pla
    pla
endmacro
