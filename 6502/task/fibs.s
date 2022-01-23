
;;; fp    2  3
;;; [..] (I, Time)
fibs:
.fp = 0
.i = 2
.jif = 3 ; time after pause
.screen = 5 ; (following 5 bytes of args needed for core fib computation routines in fib24.s)
.size_locals = 6
.begin:
    stz .i, x
.again:
    clc
    lda g_ticks
    adc #10 ; wait 1/10s
    sta .jif, x
    store16i_x fibs.static_closure, .fp
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
    yield
.go:
    lda .i, x
    heap_alloc 4 ; every heap alloc must be at least size 4 to allow the broken-heart forwarding pointer
    save16i_0 fib_iter2.code, clo
    save8_x .i, clo,2
    ;; setup args
    ;; arg2 already contains I
    copyTo16_x clo, fib_recurse.k
    store16i_x fib_recurse.static_closure, .fp
    yield

;;; fp         234
;;;    .2 .3
;;; [.. I xx] (R)
fib_iter2:
.fp = 0
.i = 2
    word .roots, .evac, .scav
.code:
    copyFrom8_x fibs.screen, g_selected_screen
    print_char ' '
    print_decimal_trip_x .i
    copyFrom16_x .fp, temp
    loadA temp, .i
    inc
    sta .i, x
    jsr fibs.again
    yield
.roots:
    rts
.evac:
    evacuate 4
.scav:
    scavenge_done 4
