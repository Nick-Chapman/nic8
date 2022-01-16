;;; Compute fib with 24-bit results

;;; fp     2 34           2   34             .01       .2 .34
;;; [..] ( N K ) --> fib (N-1 J ) where J is [fib_cont1 N  K ]
fib_recurse:
.n = 2
.k = 3
.fp = 0
.res = 2
    word .roots, .evac, .scav
.code:
    lda .n, x
    sec
    cmp #2
    bcc .base ; N<2 ?
    heap_alloc 5
    save16i_0 fib_cont1.code, clo
    save8_x .n, clo, .n
    save8_x .k, clo, .k
    save8_x .k+1, clo, .k+1
    ;; setup args
    lda .n, x
    sec
    sbc #1
    sta .n, x
    copyTo16_x clo, .k
    store16i_x fib_recurse.static_closure, .fp
    enter_fp

;;;        2 3  4
;;; --> K (N #0 #0)
.base:
    copy16_x .k, .fp
    ;; N (low-byte of result) is already in .res
    stz .res+1, x
    stz .res+2, x
    enter_fp
.roots:
    gc_root_at_x .k
    rts
.evac:
    rts
.scav:
    impossible_scavenge_because_static
.static_closure:
    word .code


;;; fp          234          2   34            .01        .23 .456
;;; .01 .2 .34
;;; [..  N  K ] (A) --> fib (N-2 J ) where J is [fib_cont2 K   A]
fib_cont1:
.fp = 0
.frame_n = 2
.a = 2
.j = 3
    word .roots, .evac, .scav
.code:
    heap_alloc 7
    copyFrom16_x .fp, temp
    save16i_0 fib_cont2.code, clo
    transfer16 temp, 3, clo, 2 ; K
    save8_x .a, clo,4 ; AL
    save8_x .a+1, clo,5 ; AM
    save8_x .a+2, clo,6 ; AH
    ;; setup args
    loadA temp, .frame_n
    sec
    sbc #2 ; N-2
    sta .a, x
    copyTo16_x clo, .j
    store16i_x fib_recurse.static_closure, .fp
    enter_fp
.roots:
    rts
.evac:
    evacuate 5
.scav:
    scavenge_cell_at 3
    scavenge_done 5

;;; fp             234 56      234
;;; .01 .23 .456
;;; [..  K    A  ] (B  Tmp) --> R (where R = A + B)
fib_cont2:
.fp = 0
.frame_k = 2
.frame_a = 4
.b = 2
.res = 2
    word .roots, .evac, .scav
.code:
    copyFrom16_x .fp, temp
    clc
    loadA temp, .frame_a
    adc .b, x
    sta .res, x
    loadA temp, .frame_a+1
    adc .b+1, x
    sta .res+1, x
    loadA temp, .frame_a+2
    adc .b+2, x
    sta .res+2, x
    ;; return to caller
    load16_x temp,.frame_k, .fp
    enter_fp
.roots:
    ;impossible_roots
    rts
.evac:
    evacuate 7
.scav:
    scavenge_cell_at 2
    scavenge_done 7
