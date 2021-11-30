;;; Compute fib with 24-bit results

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
    panic 'S'
.static_closure:
    word .code


;;; N KL KH --> K [N #0 #0]
fib_base:
    copy_word 1,fp ; K
    ;; N (low-byte of result) is already in 0
    stz 1 ; zero medium byte of result
    stz 2 ; zero high byte of result
    copy_word_from_frame0 cp ; TODO: avoid cp; using pha/pha/rts
    jmp (cp)


;;; [. . N KL KH] AL AM AH -->  fib [N-2 JL JH] where J is fib_cont2 [KL KH AL AM AH]
fib_cont1:
    word .roots, .evac, .scav
.code:
    ;; allocate cont2
    lda #7
    jsr alloc
    ;; fill in closure
    copy_code_pointer_to_heap0 fib_cont2.code ; TODO: alloc/fill via macro?
    copy_word_frame_to_heap 3, 2 ; K
    ;; TODO: want triple-byte ops!
    copy_word_local_to_heap 0, 4 ; AL,AM
    copy_byte_local_to_heap 2, 6 ; AH
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


;;; [. . KL HL AL AM AH] BL BM BH (TmpL TmpH) --> RL RM RH (where R = A + B)
fib_cont2:
    word .roots, .evac, .scav
.code:
    clc
    ;; TODO: use macro for 16 bit addition - already written!
    load_frame_var 4 ; AL
    adc 0 ; BL
    sta 0 ; RL
    load_frame_var 5 ; AM
    adc 1 ; BM
    sta 1 ; RM
    load_frame_var 6 ; AH
    adc 2 ; BH
    sta 2 ; RH
    ;; return to caller
    copy_word_from_frame 2, 3 ; K (using 3 as a temp)
    copy_word 3, fp
    copy_word_from_frame0 cp ; TODO: avoid cp; using pha/pha/rts
    jmp (cp)
.roots:
    panic 'R' ; impossible because no allocation in this code
.evac:
    evacuate 7
.scav:
    scavenge_cell_at 2
    scavenge_done 7
