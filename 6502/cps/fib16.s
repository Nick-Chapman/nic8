;;; Original version of (cps) fib -- computes 16 bit results

;;; [] N KL KH --> fib [N-1 JL JH] where J is fib_cont1 [N KL KH]
fib_recurse:
    word .roots, .evac, .scav
.code:
    ;; access N
    lda 0
    sec
    cmp #2
    bcc fib_base ; N<2 ?
    ;; allocate cont1
    lda #5
    jsr alloc
    ;; fill in closure
    copy_code_pointer_to_heap0 fib_cont1.code
    copy_byte_local_to_heap 0, 2
    copy_word_local_to_heap 1, 3
    ;; setup args
    lda 0 ; N
    sec
    sbc #1 ; N-1
    sta 0
    copy_word clo, 1
    ;; TODO: extract standard entry protocol, 'enter'
    copy_code_pointer_to_local fib_recurse.static_closure, fp
    jmp fib_recurse.code
.roots:
    gc_root_at 1
    rts
.evac:
    copy_word ev, clo
    rts
.scav:
    impossible_scavenge_because_static
.static_closure:
    word fib_recurse.code


;;; N KL KH --> K [N #0]
fib_base:
    copy_word 1,fp ; K
    ;; N (low-byte of result) is already in 0
    lda #0
    sta 1 ; zero high-byte of result
    copy_word_from_frame0 cp ; TODO: avoid cp; using pha/pha/rts
    jmp (cp)


;;; [. . N KL KH] AL AH -->  fib [N-2 JL JH] where J is fib_cont2 [KL KH AL AH]
fib_cont1:
    word .roots, .evac, .scav
.code:
    ;; allocate cont2
    lda #6
    jsr alloc
    ;; fill in closure
    copy_code_pointer_to_heap0 fib_cont2.code ; TODO: alloc/fill via macro?
    copy_word_frame_to_heap 3, 2 ; K
    copy_word_local_to_heap 0, 4 ; A
    ;; setup args
    load_frame_var 2 ; N
    sec
    sbc #2 ; N-2
    sta 0
    copy_word clo,1
    copy_code_pointer_to_local fib_recurse.static_closure, fp
    jmp fib_recurse.code
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
    adc 0 ; BL
    sta 0 ; RL
    load_frame_var 5 ; AH
    adc 1 ; BH
    sta 1 ; RH
    ;; return to caller
    copy_word_from_frame 2, 2 ; K
    copy_word 2, fp
    copy_word_from_frame0 cp ; TODO: avoid cp; using pha/pha/rts
    jmp (cp)
.roots:
    impossible_roots
.evac:
    evacuate 6
.scav:
    scavenge_cell_at 2
    scavenge_done 6
