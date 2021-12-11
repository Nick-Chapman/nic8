
;;; fp    2  3
;;; [..] (I, Time)
fib_iter:
    word .roots, .evac, .scav
.code:
    clc
    lda g_ticks
    adc #10 ; wait 1/10s
    sta arg3
.wait
    lda g_ticks
    sec
    sbc arg3 ; temp in arg3 - the time to continue
    bpl .go
    NEXT .wait
.roots:
    rts
.evac:
    rts
.scav:
    impossible_scavenge_because_static
.static_closure:
    word fib_iter.code

.go:
    lda arg2
    ;; allocate & fill in closure
    heap_alloc 'g', 3
    save16i_0 fib_iter2.code, clo
    save8 arg2, clo,2 ; I
    ;; setup args & fp
    ;; arg2 already contains I
    copy16 clo, arg3 ; K
    store16i fib_recurse.static_closure, fp
    NEXT fib_recurse.code


;;; fp     234
;;;    .2
;;; [.. I] (R)
fib_iter2:
    word .roots, .evac, .scav
.code:
    print_decimal_trip arg2 ; RL,RM,RH (24 bit result)
    lda #' '
    jsr screen_putchar
    loadA fp, 2 ; I
    inc
    sta arg2 ; I+1
    store16i fib_iter.static_closure, fp
    NEXT fib_iter.code
.roots:
    rts
.evac:
    evacuate 3
.scav:
    scavenge_done 3
