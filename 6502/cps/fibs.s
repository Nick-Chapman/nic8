
;;; fp    2  3
;;; [..] (I, Time)
fib_iter:
.fp = 0
.i = 2
.jif = 3 ; time after pause
.begin:
    stz .i, x
.again:
    clc
    lda g_ticks
    adc #10 ; wait 1/10s
    sta .jif, x
    store16i_x fib_iter.static_closure, .fp
    rts
.roots:
    rts
.evac:
    rts
.scav:
    impossible_scavenge_because_static
.static_closure:
    word .code
    word .roots, .evac, .scav
.code
    lda g_ticks
    sec
    sbc .jif, x
    bpl .go
    enter_fp
.go:
    lda .i, x
    heap_alloc 4 ; every heap alloc must be at least size 4 to allow the broken-heart forwarding pointer
    save16i_0 fib_iter2.code, clo
    save8_x .i, clo,2
    ;; setup args
    ;; arg2 already contains I
    copyTo16_x clo, fib_recurse.k
    store16i_x fib_recurse.static_closure, .fp
    enter_fp

;;; fp         234
;;;    .2 .3
;;; [.. I xx] (R)
fib_iter2:
.fp = 0
.i = 2
    word .roots, .evac, .scav
.code:
    print_char ' '
    print_decimal_trip_x .i
    ;acia_print_string " f-"
    ;acia_print_decimal_trip_x .i
    copyFrom16_x .fp, temp
    loadA temp, .i
    inc
    sta .i, x
    jsr fib_iter.again
    enter_fp
.roots:
    rts
.evac:
    evacuate 4
.scav:
    scavenge_done 4
