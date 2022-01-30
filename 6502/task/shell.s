;;; beginning of shell task

spawn_shell: macro
    lda #shell.size_locals+1
    jsr tasking.create
    store16i_x shell.static_closure, shell.fp
endmacro

shell:
.fp = 0
.size_locals = 2
.static_closure:
    word .code
    word roots_none, evac_static, scav_impossible
.code:
    lda g_new_command
    bne .execute
    yield
.execute:
    stz g_new_command
    acia_print_string "shell: "
    copy16 g_last_line, temp
    ;; very inefficient way of incrementing the pointer by 3
    increment16 temp
    increment16 temp
    increment16 temp
    acia_print_string_variable temp
    acia_print_char '\n'
    yield
