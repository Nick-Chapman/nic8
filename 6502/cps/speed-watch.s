;;; speed-watch task, in cps style

;;; speed_watch 23 4
;;; [..]       (c  j)
speed_watch:
.c = 2 ; count executions this jiffy
.j = 4 ; this jiffy
.roots:
    rts ; no roots
.evac:
    rts ; static
.scav:
    impossible_scavenge_because_static
.begin:
    store16i .static_closure, fp
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
    jsr screen_return_home
    print_decimal_word_x .c
    print_string "    "
    newline
    print_string "speed"
    jsr .again
    enter_fp
