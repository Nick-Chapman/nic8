
debug: macro C
    print_char \C
    screen_flush_selected
endmacro

debug_hex_word: macro L
    print_hex_word \L
    screen_flush_selected
endmacro

debug_hex_byte: macro L
    print_hex_byte \L
    screen_flush_selected
endmacro

debug_decimal_word: macro L
    print_decimal_word \L
    screen_flush_selected
endmacro
