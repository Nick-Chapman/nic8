
;;; duplicated from heap
BASE = 10
arg2 = BASE + 2
arg3 = BASE + 3
arg4 = BASE + 4
arg5 = BASE + 5
arg6 = BASE + 6

;;; uptime task, in cps style

;;; hours/minutes/seconds (h/m/s) are each a single byte
;;; we have an extra byte (j) for the next jiffy at which we tick
;;; uptime 2 3 4 5
;;; [..]  (h m s j)

;;; TODO: how can we make these names be local?
h = arg2
m = arg3
s = arg4
j = arg5

uptime:
.begin:
    stz s
    stz m
    stz h
    ;; fallthrough
.show:
    jsr .display
    NEXT .init_wait
.init_wait
    jsr .tick
    clc
    lda g_ticks
    adc #100
    sta j
    ;; fallthrough
.wait
    lda g_ticks
    sec
    sbc j
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
    print_leading_zero h
    print_decimal_byte h
    print_char ':'
    print_leading_zero m
    print_decimal_byte m
    print_char ':'
    print_leading_zero s
    print_decimal_byte s
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
    increment_modulo s, 60
    increment_modulo m, 60
    increment_modulo h, 24
.done:
    rts
