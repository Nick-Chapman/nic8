;;; speed-watch task, in cps style

;;; speed_watch 23 4
;;; [..]       (c  j)
speed:
.fp = 0
.c = 2 ; count (16 bits) executions this jiffy
.screen = 4
.j = 5 ; this jiffy
.size_locals = 6
.roots:
    rts ; no roots
.evac:
    rts ; static
.scav:
    impossible_scavenge_because_static
.begin:
    store16i_x .static_closure, .fp
.again:
    stz .c, x
    stz .c + 1, x
    clc
    lda g_ticks
    sta .j, x
    rts
.static_closure:
    word .code
    word .roots, .evac, .scav
.code
    lda .j, x
    cmp g_ticks
    bmi .display
    inc .c, x
    bne .skip
    inc .c + 1, x
.skip:
    enter_fp
.display:
    copyFrom8_x .screen, g_selected_screen
    jsr screen.return_home
    print_decimal_word_x .c
    print_string "    "
    newline
    print_string "speed"
    jsr .again
    enter_fp
