
;;; panic macro

panic: macro STRING
    print_char '!'
    print_string \STRING
    jmp panic.flush_spin
endmacro

panic:
.flush_spin:
    lda g_selected_screen
    jsr screen_flush_sub
.spin
    jmp .spin
