
fib7_name:
    string "7: CPS/Heap"
    word fib7_name
fib7_entry:
    ;; N(acc) --> fib7 [N KL KH] where K is fib7_done []
    sta 0

    ;; ;; How deep is the page-1 stack?
    ;; lda #'<'
    ;; jsr screen_putchar
    ;; tsx
    ;; txa
    ;; jsr put_hex_byte
    ;; lda #'>'
    ;; jsr screen_putchar
    ;; jsr print_screen

    copy16_literal_to_var 0, gc_count ; TODO: 0

    ;; initialize heap
    ;jsr wipe_space_a
    ;jsr wipe_space_b
	;; TODO: extract gc_init
    jsr gc.set_heap_space_a
    copy_word hp, heap_start
    ;; allocate final continuation -- TODO: no need for this to be heap allocated
    lda #2
    jsr alloc
    ;; fill in closure
    copy_code_pointer_to_heap0 fib7_done
    ;; setup args
    copy_word clo, 1
    copy_code_pointer_to_local fib7_recurse_static_closure, fp
    jmp fib7_recurse


;;; RL RH -->
    text "fib7_done"
    word rootargs_impossible, evacuate2, scavenge_nothing_of2
    byte 2
fib7_done:
    ;; move final result to pre-allocated space on stack
    tsx
    lda 0
    sta $103,x
    lda 1
    sta $104,x
    rts ; return to original caller

rootargs_impossible:
    panic 'R'

    text "evacuate2"
evacuate2:
    lda #2
    jsr alloc.again
    evacuate_byte 0
    evacuate_byte 1
    rts

    text "scavenge_nothing_of2"
scavenge_nothing_of2:
    shift_low_water 2
    jmp gc.scavenge_loop




fib7_recurse_static_closure:
    word fib7_recurse

;;; fib7 is a top level function
;;; and so we have a static closure
;;; and so, we do nothing if asked to evacuate - just return the same static clo
;;; and because we didn't evacuate (we never were in the heap, and we still aren't)
;;; it is impossible that this static closure will be subject to scavenging
;;; (i.e. that process where we walk along the new-heap
;;; using the low-water 'lw' pointer.. until catches up with 'hp')
;;; [] N KL KH --> fib7 [N-1 JL JH] where J is fib7_cont1 [N KL KH]
    text "fib7_recurse"
    word rootargs_at1, evacuate_do_nothing, scavenge_impossible
    byte 3 ; TODO: This 'arg-count' byte is used nowhere!
fib7_recurse:
    ;; access N
    lda 0
    sec
    cmp #2
    bcc fib7_base ; N<2 ?
    ;; allocate cont1
    lda #5
    jsr alloc
    ;; fill in closure
    copy_code_pointer_to_heap0 fib7_cont1
    copy_byte_local_to_heap 0, 2
    copy_word_local_to_heap 1, 3
    ;; setup args
    lda 0 ; N
    sec
    sbc #1 ; N-1
    sta 0
    copy_word clo, 1
    copy_code_pointer_to_local fib7_recurse_static_closure, fp
    jmp fib7_recurse

rootargs_at1:
    copy_word 1, ev
    jsr gc_evacuate
    copy_word clo, 1
    rts

    text "evacuate_do_nothing"
evacuate_do_nothing:
    copy_word ev, clo
    rts

scavenge_impossible:
    panic 'S'

;;; N KL KH --> K [N #0]
fib7_base:
    ;print_char "."
    ;; move K into fp
    copy_word 1,fp
    ;; RL is N (already in 0)
    lda #0
    sta 1 ; setup RH
    copy_word_from_frame0 cp ; TODO: avoid cp; using pha/pha/rts
    ;print_hex_word 0
    jmp (cp) ; enter_fp


;;; TODO: switch N/K to standard order
;;; (doesn't matter while we have specific scavenge routines for each-shape)
;;; [. . N KL KH] AL AH -->  fib7 [N-2 JL JH] where J is fib7_cont2 [KL KH AL AH]
    text "fib7_cont1"
    word rootargs_none, evacuate5, scavenge_at3_of5
    byte 2
fib7_cont1:
    ;; allocate cont2
    lda #6
    jsr alloc
    ;; fill in closure
    copy_code_pointer_to_heap0 fib7_cont2
    copy_word_frame_to_heap 3, 2 ; K
    copy_word_local_to_heap 0, 4 ; A
    ;; setup args
    load_frame_var 2 ; N
    sec
    sbc #2 ; N-2
    sta 0
    copy_word clo,1
    copy_code_pointer_to_local fib7_recurse_static_closure, fp
    jmp fib7_recurse
    
rootargs_none:
    rts

    text "evacuate5"
evacuate5:
    lda #5
    jsr alloc.again
    evacuate_byte 0
    evacuate_byte 1
    evacuate_byte 2
    evacuate_byte 3
    evacuate_byte 4
    rts

    text "scavenge_at3_of5"
scavenge_at3_of5:
    scavenge_cell_at 3
    shift_low_water 5
    jmp gc.scavenge_loop


;;; We say rootargs_impossible here rather than rootargs_none
;;; because GC should never be initiated whilst the closure is set as 'fp'
;;; because no allocation occurs here!
;;;
;;; [. . KL HL AL AH] BL BH (TmpL TmpH) --> RL RH (where R = A + B)
    text "fib7_cont2"
    word rootargs_impossible2, evacuate6, scavenge_at2_of6
    byte 2
fib7_cont2:
    ;; jsr screen_newline
    ;; print_hex_word 0
    ;; print_char '+'
    ;; copy_word_from_frame 4, temp
    ;; print_hex_word temp
    ;; 16-bit addition
    clc
    ;; macro for 16 bit addition?
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
    ;print_char '='
    ;print_hex_word 0
    jmp (cp) ; enter_fp

rootargs_impossible2:
    panic 'R'


    text "evacuate6"
evacuate6:
    lda #6
    jsr alloc.again
    evacuate_byte 0
    evacuate_byte 1
    evacuate_byte 2
    evacuate_byte 3
    evacuate_byte 4
    evacuate_byte 5
    rts



    text "scavenge_at2_of6"
scavenge_at2_of6:
    scavenge_cell_at 2
    shift_low_water 6
    jmp gc.scavenge_loop
    
