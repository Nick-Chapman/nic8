;;; speed-watch task, in cps style

;;; speed_watch 23 4
;;; [..]       (c  j)
speed_watch:
.c = 2 ; count executions this jiffy
.j = 4 ; this jiffy
.begin:
.reset:
    stz .c, x
    stz .c + 1, x
    clc
    lda g_ticks
    sta .j, x
    NEXT .wait
.wait: ; TODO: rename code; be closure code (i.e. follow: word .roots, .evac, .scav)
    lda .j, x
    cmp g_ticks
    bmi .display
    inc .c, x
    bne .skip
    inc .c + 1, x
.skip:
    NEXT .wait
.display:
    jsr screen_return_home
    print_decimal_word_x .c
    print_string "    "
    newline
    print_string "speed"
    NEXT .reset ; TODO: jmp
