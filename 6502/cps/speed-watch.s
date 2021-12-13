;;; speed-watch task, in cps style

;;; prefix with 's' to avoid clash with uptime, and set base to 10 ; TODO: de-hack!
sBASE = 10
sarg2 = sBASE + 2
sarg3 = sBASE + 3
sarg4 = sBASE + 4
sarg5 = sBASE + 5
sarg6 = sBASE + 6

;;; speed_watch 23 4
;;; [..]       (c  j)
speed_watch:
.c = sarg2 ; count executions this jiffy
.j = sarg4 ; this jiffy
.begin:
.reset:
    stz .c
    stz .c + 1
    clc
    lda g_ticks
    sta .j
    NEXT .wait
.wait:
    lda .j
    cmp g_ticks
    bmi .display
    inc .c
    bne .skip
    inc .c + 1
.skip:
    NEXT .wait
.display:
    jsr screen_return_home
    print_decimal_word .c
    newline
    print_string "speed"
    NEXT .reset
