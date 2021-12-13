
;;; duplicated from heap
BASE = 10
arg2 = BASE + 2
arg3 = BASE + 3
arg4 = BASE + 4
arg5 = BASE + 5
arg6 = BASE + 6

;;; speed-watch task, in cps style

;;; TODO: how can we make these names be local?
c = arg2 ; count times this jiffy
j = arg4 ; this jiffy

;;; speed_watch 23 4
;;; [..]       (c  j)

speed_watch:
.begin:
.reset:
    stz c
    stz c + 1
    clc
    lda g_ticks
    sta j
    NEXT .wait
.wait:
    lda j
    cmp g_ticks
    bmi .display
    inc c
    bne .skip
    inc c + 1
.skip:
    NEXT .wait
.display:
    jsr screen_return_home
    print_decimal_word c
    newline
    print_string "speed"
    NEXT .reset
