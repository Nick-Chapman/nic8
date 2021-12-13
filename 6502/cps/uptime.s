;;; uptime task, in cps style

;;; uptime 2 3 4 5
;;; [..]  (h m s j)
uptime:
.h = 2 ; hours
.m = 3 ; minutes
.s = 4 ; seconds
.j = 5 ; jiffy to tick
.begin:
    stz .s, x
    stz .m, x
    stz .h, x
.show:
    jsr .display
.init_wait
    jsr .tick
    clc
    lda g_ticks
    adc #100
    sta .j, x
.wait
    lda g_ticks
    cmp .j, x
    bpl .show
    NEXT .wait

print_leading_zero: macro V
    lda \V, x
    cmp #10
    bcs .skip\@
    print_char '0'
.skip\@:
endmacro

.display:
    jsr screen_return_home
    print_leading_zero .h
    print_decimal_byte_x .h
    print_char ':'
    print_leading_zero .m
    print_decimal_byte_x .m
    print_char ':'
    print_leading_zero .s
    print_decimal_byte_x .s
    print_string "   " ; just in case we get corrupton; better chance to see the data
    newline
    print_string "uptime"
    rts

increment_modulo: macro V, N
    inc \V, x
    lda \V, x
    cmp #\N
    bne .done
    stz \V, x
endmacro

.tick:
    increment_modulo .s, 60
    increment_modulo .m, 60
    increment_modulo .h, 24
.done:
    rts
