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
    copy_code_pointer_to_heap0 fib_cont1.code
    copy_byte_local_to_heap arg2, 2
    copy_byte_local_to_heap arg3, 3
    copy_byte_local_to_heap arg4, 4
    ;; setup args
    lda arg2 ; N
    sec
    sbc #1 ; N-1
    sta arg2
    copy_word clo, arg3
    copy_code_pointer_to_local fib_recurse.static_closure, fp
    NEXT fib_recurse.code
;;; N KL KH --> K [N #0]
.base:
    copy_word arg3,fp ; K
    ;; N (low-byte of result) is already in arg2
    lda #0
    sta arg3 ; zero high-byte of result
    copy_word_from_frame0 cp
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
    copy_code_pointer_to_heap0 fib_cont2.code
    copy_word_frame_to_heap 3, 2 ; K
    copy_byte_local_to_heap arg2, 4 ; AL
    copy_byte_local_to_heap arg3, 5 ; AH
    ;; setup args
    load_frame_var 2 ; N
    sec
    sbc #2 ; N-2
    sta arg2
    copy_word clo,arg3
    copy_code_pointer_to_local fib_recurse.static_closure, fp
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
    ;; TODO: use macro for 16 bit addition - already written!
    load_frame_var 4 ; AL
    adc arg2 ; BL
    sta arg2 ; RL
    load_frame_var 5 ; AH
    adc arg3 ; BH
    sta arg3 ; RH
    ;; return to caller
    copy_word_from_frame 2, arg4 ; K
    copy_word arg4, fp
    copy_word_from_frame0 cp
    NEXT (cp)
.roots:
    impossible_roots
.evac:
    evacuate 6
.scav:
    scavenge_cell_at 2
    scavenge_done 6
