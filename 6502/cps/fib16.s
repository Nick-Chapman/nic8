;;; Original version of (cps) fib -- computes 16 bit results

;;; [] N KL KH --> fib [N-1 JL JH] where J is fib_cont1 [N KL KH]
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
    heap_alloc 'a', 5
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
;;; N KL KH --> K [N #0]
.base:
    copy16 arg3,fp ; K
    ;; N (low-byte of result) is already in arg2
    lda #0
    sta arg3 ; zero high-byte of result
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
    word fib_recurse.code



;;; [. . N KL KH] AL AH -->  fib [N-2 JL JH] where J is fib_cont2 [KL KH AL AH]
fib_cont1:
    word .roots, .evac, .scav
.code:
    ;; allocate cont2
    heap_alloc 'b', 6
    ;; fill in closure
    save16i_0 fib_cont2.code, clo
    transfer16 fp, 3, clo, 2 ; K
    save8 arg2, clo,4 ; AL
    save8 arg3, clo,5 ; AH
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


;;; [. . KL HL AL AH] BL BH (TmpL TmpH) --> RL RH (where R = A + B)
fib_cont2:
    word .roots, .evac, .scav
.code:
    clc
    ;; How can we use the macro for 16 bit addition without using a temp?
    loadA fp, 4 ; AL
    adc arg2 ; BL
    sta arg2 ; RL
    loadA fp, 5 ; AH
    adc arg3 ; BH
    sta arg3 ; RH
    ;; return to caller
    load16 fp,2, arg4 ; K
    copy16 arg4, fp
    load16_0 fp, cp
    NEXT (cp)
.roots:
    impossible_roots
.evac:
    evacuate 6
.scav:
    scavenge_cell_at 2
    scavenge_done 6
