
panic1: macro CHAR
    print_char '!'
    print_char \CHAR
    jmp panic_flush_spin
endmacro

panic: macro STRING
    print_char '!'
    newline
    print_string \STRING
    jmp panic_flush_spin
endmacro

panic_flush_spin:
    lda g_selected_screen
    jsr screen_flush_sub
.spin
    jmp .spin
