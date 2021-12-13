;;; uptime task, in cps style

;;; prefix with 'u' to avoid clash with speed-watch, and set base to 20 ; TODO: de-hack!
uBASE = 20
uarg2 = uBASE + 2
uarg3 = uBASE + 3
uarg4 = uBASE + 4
uarg5 = uBASE + 5
uarg6 = uBASE + 6

;;; uptime 2 3 4 5
;;; [..]  (h m s j)
uptime:
.h = uarg2 ; hours
.m = uarg3 ; minutes
.s = uarg4 ; seconds
.j = uarg5 ; jiffy to tick
.begin:
    stz .s
    stz .m
    stz .h
    ;; fallthrough
.show:
    jsr .display
    NEXT .init_wait
.init_wait
    jsr .tick
    clc
    lda g_ticks
    adc #100
    sta .j
    ;; fallthrough
.wait
    lda g_ticks
    sec
    sbc .j
    bpl .show
    NEXT .wait

print_leading_zero: macro V
    lda \V
    cmp #10
    bpl .skip\@
    print_char '0'
.skip\@:
endmacro

.display:
    jsr screen_return_home
    print_leading_zero .h
    print_decimal_byte .h
    print_char ':'
    print_leading_zero .m
    print_decimal_byte .m
    print_char ':'
    print_leading_zero .s
    print_decimal_byte .s
    newline
    print_string "uptime"
    rts

increment_modulo: macro V, N
    inc \V
    lda \V
    cmp #\N
    bne .done
    stz \V
endmacro

.tick:
    increment_modulo .s, 60
    increment_modulo .m, 60
    increment_modulo .h, 24
.done:
    rts
