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
    ;; allocate cont1
    heap_alloc 'e', 5
    ;; fill in closure
    copy_code_pointer_to_heap0 fib_cont1.code
    copy_byte_local_to_heap arg2, 2
    ;copy_word_local_to_heap arg3, 3 ; replace word with 2xbyte ops
    copy_byte_local_to_heap arg3, 3
    copy_byte_local_to_heap arg4, 4
    ;; setup args
    lda arg2 ; N
    sec
    sbc #1 ; N-1
    sta arg2
    copy_word clo, arg3
    copy_code_pointer_to_local fib_recurse.static_closure, fp
    jmp fib_recurse.code

;;;        2 3  4
;;; --> K (N #0 #0)
.base:
    copy_word arg3,fp ; K
    ;; N (low-byte of result) is already in arg2
    stz arg3 ; zero medium byte of result
    stz arg4 ; zero high byte of result
    copy_word_from_frame0 cp
    jmp (cp)
.roots:
    gc_root_at arg3
    rts
.evac:
    copy_word ev, clo
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
    heap_alloc 'f', 7
    ;; fill in closure
    copy_code_pointer_to_heap0 fib_cont2.code
    copy_word_frame_to_heap 3, 2 ; K
    copy_byte_local_to_heap arg2, 4 ; AL
    copy_byte_local_to_heap arg3, 5 ; AM
    copy_byte_local_to_heap arg4, 6 ; AH
    ;; setup args
    load_frame_var 2 ; N
    sec
    sbc #2 ; N-2
    sta arg2
    copy_word clo,arg3
    copy_code_pointer_to_local fib_recurse.static_closure, fp
    jmp fib_recurse.code
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
    ;; TODO: use macro for 16 bit addition - already written!
    load_frame_var 4 ; AL
    adc arg2 ; BL
    sta arg2 ; RL
    load_frame_var 5 ; AM
    adc arg3 ; BM
    sta arg3 ; RM
    load_frame_var 6 ; AH
    adc arg4 ; BH
    sta arg4 ; RH
    ;; return to caller
    copy_word_from_frame 2, arg5 ; K (using arg5 as a temp)
    copy_word arg5, fp
    copy_word_from_frame0 cp
    jmp (cp)
.roots:
    impossible_roots
.evac:
    evacuate 7
.scav:
    scavenge_cell_at 2
    scavenge_done 7
