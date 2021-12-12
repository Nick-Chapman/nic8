;;; Compute fib with 24-bit results

;;; fp     2 34           2   34             .01       .2 .34
;;; [..] ( N K ) --> fib (N-1 J ) where J is [fib_cont1 N  K ]
fib_recurse:
    word .roots, .evac, .scav
.code:
    ;; access N
    lda arg2
    sec
    cmp #2
    bcc .base ; N<2 ?
;;     bcs .rec
;;     jmp .base
;; .rec:
    ;; allocate cont1
    heap_alloc 5
    ;; fill in closure
    save16i_0 fib_cont1.code, clo
    save8 arg2, clo,2
    save8 arg3, clo,3
    save8 arg4, clo,4
    ;; setup args
    lda arg2 ; N
    sec
    sbc #1 ; N-1
    sta arg2
    copy16 clo, arg3
    store16i fib_recurse.static_closure, fp
    NEXT fib_recurse.code

;;;        2 3  4
;;; --> K (N #0 #0)
.base:
    copy16 arg3,fp ; K
    ;; N (low-byte of result) is already in arg2
    stz arg3 ; zero medium byte of result
    stz arg4 ; zero high byte of result
    load16_0 fp, cp
    NEXT (cp)
.roots:
    gc_root_at arg3
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
    word .roots, .evac, .scav
.code:
    ;; allocate cont2
    heap_alloc 7
    ;; fill in closure
    save16i_0 fib_cont2.code, clo
    transfer16 fp, 3, clo, 2 ; K
    save8 arg2, clo,4 ; AL
    save8 arg3, clo,5 ; AM
    save8 arg4, clo,6 ; AH
    ;; setup args
    loadA fp, 2 ; N
    sec
    sbc #2 ; N-2
    sta arg2
    copy16 clo,arg3
    store16i fib_recurse.static_closure, fp
    NEXT fib_recurse.code
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
    word .roots, .evac, .scav
.code:
    clc
    loadA fp, 4 ; AL
    adc arg2 ; BL
    sta arg2 ; RL
    loadA fp, 5 ; AM
    adc arg3 ; BM
    sta arg3 ; RM
    loadA fp, 6 ; AH
    adc arg4 ; BH
    sta arg4 ; RH
    ;; return to caller
    load16 fp,2, arg5 ; K (using arg5 as a temp)
    copy16 arg5, fp
    load16_0 fp, cp
    NEXT (cp)
.roots:
    impossible_roots
.evac:
    evacuate 7
.scav:
    scavenge_cell_at 2
    scavenge_done 7
