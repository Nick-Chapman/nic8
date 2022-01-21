
;;; TODO: refactoring and deduping!

newline: macro
    phy
    jsr screen.newline
    ply
endmacro

print_char: macro CHAR
    pha
    lda #\CHAR
    jsr screen.putchar
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
    jsr decimal24.generic_put
    ply
    plx
    pla
endmacro

acia_print_decimal_trip_x: macro L
    generic_print_decimal_trip_x acia.putchar, \L
endmacro

print_decimal_trip_x: macro L
    generic_print_decimal_trip_x screen.putchar, \L
endmacro

generic_print_decimal_word: macro P, L
    pha
    phx
    store16i \P, g_putchar
    lda \L
    ldx \L + 1
    jsr decimal16.generic_put
    plx
    pla
endmacro

acia_print_decimal_word: macro L
    generic_print_decimal_word acia.putchar, \L
endmacro

print_decimal_word: macro L
    generic_print_decimal_word screen.putchar, \L
endmacro

generic_print_decimal_word_x: macro P, L
    pha
    store16i \P, g_putchar
    ldy \L, x
    lda \L + 1, x
    phx
    tax
    tya
    jsr decimal16.generic_put
    plx
    pla
endmacro

acia_print_decimal_word_x: macro L
    generic_print_decimal_word_x acia.putchar, \L
endmacro

print_decimal_word_x: macro L
    generic_print_decimal_word_x screen.putchar, \L
endmacro

print_decimal_byte: macro L
    pha
    phx
    lda \L
    ldx #0
    jsr decimal16.screen_put
    plx
    pla
endmacro

print_decimal_byte_x: macro L
    pha
    lda \L, x
    phx
    ldx #0
    jsr decimal16.screen_put
    plx
    pla
endmacro

put_hex_byte: ; TODO: dedup
    phx
    pha
    lsr
    lsr
    lsr
    lsr
    tax
    lda screen.digits,x ; TODO: avoid odd nonlocal reference
    jsr screen.putchar
    pla
    and #%1111
    tax
    lda screen.digits,x
    jsr screen.putchar
    plx
    rts

print_hex_word: macro L
    lda #'['
    jsr screen.putchar
    lda \L + 1
    jsr put_hex_byte
    lda \L
    jsr put_hex_byte
    lda #']'
    jsr screen.putchar
endmac

print_hex_byte: macro L
    lda #'['
    jsr screen.putchar
    lda \L
    jsr put_hex_byte
    lda #']'
    jsr screen.putchar
endmac


;;; TODO: move these macros to screen; prefix with screen_

put_string: ; TODO: make this work for long strings (like acia.put_string)
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
      jsr screen.putchar ; changes y
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


acia_print_string: macro S ; TODO: dedup with similar (wrapping other put_string)
    jmp .skip\@
.embedded\@:
    string \S
.skip\@:
    pha
      lda #>.embedded\@
      pha
      lda #<.embedded\@
      pha
      jsr acia.put_string
      pla
      pla
    pla
endmacro
