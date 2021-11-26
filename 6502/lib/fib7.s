
fib7_name:
    string "7: CPS/Heap"
    word fib7_name
fib7_entry:
    ;; N(acc) --> fib7 [N KL KH] where K is fib7_done []
    sta 0 ; N
    copy16_literal_to_var 0, gc_count

    ;;jsr init_gc ; TODO: move to general init code
    jsr gc.set_heap_space_a
    copy_word hp, heap_start


    ;; allocate final continuation -- TODO: no need for this to be heap allocated
    lda #2
    jsr alloc
    ;; fill in closure
    copy_code_pointer_to_heap0 fib7_done.code
    ;; setup args
    copy_word clo, 1
    copy_code_pointer_to_local fib7_recurse.static_closure, fp
    jmp fib7_recurse.code


;;; RL RH -->
fib7_done:
    word .roots, .evac, .scav ; TODO capture common pattern for def/.code
    byte 2
.code:
    ;; move final result to pre-allocated space on stack
    tsx
    lda 0
    sta $103,x
    lda 1
    sta $104,x
    rts ; return to original caller
.roots:
    panic 'R'
.evac:
    lda #2
    jsr alloc.again ; TODO: stop using private entry point
    evacuate_byte 0
    evacuate_byte 1
    rts
.scav:
    shift_low_water 2
    ; TODO: move call to scavenge_loop into shift_low_water
    jmp gc.scavenge_loop ; TODO: stop using private entry



;;; fib7 is a top level function
;;; and so we have a static closure
;;; and so, we do nothing if asked to evacuate - just return the same static clo
;;; and because we didn't evacuate (we never were in the heap, and we still aren't)
;;; it is impossible that this static closure will be subject to scavenging
;;; (i.e. that process where we walk along the new-heap
;;; using the low-water 'lw' pointer.. until catches up with 'hp')
;;; [] N KL KH --> fib7 [N-1 JL JH] where J is fib7_cont1 [N KL KH]
fib7_recurse:
    word .roots, .evac, .scav
    byte 3 ; TODO: This 'arg-count' byte is used nowhere!
.code:
    ;; access N
    lda 0
    sec
    cmp #2
    bcc fib7_base ; N<2 ?
    ;; allocate cont1
    lda #5
    jsr alloc
    ;; fill in closure
    copy_code_pointer_to_heap0 fib7_cont1.code
    copy_byte_local_to_heap 0, 2
    copy_word_local_to_heap 1, 3
    ;; setup args
    lda 0 ; N
    sec
    sbc #1 ; N-1
    sta 0
    copy_word clo, 1
    copy_code_pointer_to_local fib7_recurse.static_closure, fp
    jmp fib7_recurse.code ; AGGH, was another bug here
.roots:
    copy_word 1, ev ; TODO capture this pattern: ev->evac->clo
    jsr gc_evacuate
    copy_word clo, 1
    rts
.evac:
    copy_word ev, clo
    rts
.scav:
    panic 'S'
.static_closure:
    word fib7_recurse.code


;;; N KL KH --> K [N #0]
fib7_base:
    ;print_char "."
    ;; move K into fp
    copy_word 1,fp
    ;; RL is N (already in 0)
    lda #0
    sta 1 ; setup RH
    copy_word_from_frame0 cp ; TODO: avoid cp; using pha/pha/rts
    jmp (cp)


;;; TODO: switch N/K to standard order
;;; (doesn't matter while we have specific scavenge routines for each-shape)
;;; [. . N KL KH] AL AH -->  fib7 [N-2 JL JH] where J is fib7_cont2 [KL KH AL AH]
fib7_cont1:
    word .roots, .evac, .scav
    byte 2
.code:
    ;; allocate cont2
    lda #6
    jsr alloc
    ;; fill in closure
    copy_code_pointer_to_heap0 fib7_cont2.code ; TODO: alloc/fill via macro?
    copy_word_frame_to_heap 3, 2 ; K
    copy_word_local_to_heap 0, 4 ; A
    ;; setup args
    load_frame_var 2 ; N
    sec
    sbc #2 ; N-2
    sta 0
    copy_word clo,1
    copy_code_pointer_to_local fib7_recurse.static_closure, fp
    jmp fib7_recurse.code ; AGGH, and here.. and that the final one!
.roots:
    rts
.evac:
    lda #5
    jsr alloc.again ; TODO: stop using private entry point
    evacuate_byte 0
    evacuate_byte 1
    evacuate_byte 2
    evacuate_byte 3
    evacuate_byte 4
    rts
.scav:
    scavenge_cell_at 3
    shift_low_water 5
    jmp gc.scavenge_loop ; TODO: stop using private entry

;;; We say rootargs_impossible here rather than rootargs_none
;;; because GC should never be initiated whilst the closure is set as 'fp'
;;; because no allocation occurs here!
;;;
;;; [. . KL HL AL AH] BL BH (TmpL TmpH) --> RL RH (where R = A + B)
fib7_cont2:
    word .roots, .evac, .scav
    byte 2
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
    panic 'R'
.evac:
    lda #6
    jsr alloc.again ; TODO: stop using private entry point
    evacuate_byte 0
    evacuate_byte 1
    evacuate_byte 2
    evacuate_byte 3
    evacuate_byte 4
    evacuate_byte 5
    rts
.scav:
    scavenge_cell_at 2
    shift_low_water 6
    jmp gc.scavenge_loop ; TODO: stop using private entry
