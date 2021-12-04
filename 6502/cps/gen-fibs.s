
;;; fp    2
;;; [..] (I)
fib_iter:
    word .roots, .evac, .scav
.code:
    lda arg2
    ;; allocate & fill in closure
    heap_alloc 'g', 3
    copy_code_pointer_to_heap0 fib_iter2.code
    copy_byte_local_to_heap arg2, 2 ; I
    ;; setup args & fp
    ;; arg2 already contains I
    copy_word clo, arg3 ; K
    copy_code_pointer_to_local fib_recurse.static_closure, fp
    jmp fib_recurse.code
.roots:
    rts ; no roots
.evac:
    copy_word ev, clo ; TODO: change evac inteface to be ev->ev, then this line wont be needed
    rts
.scav:
    impossible_scavenge_because_static
.static_closure:
    word fib_iter.code


;;; fp     234
;;;    .2
;;; [.. I] (R)
fib_iter2:
    word .roots, .evac, .scav
.code:
    print_decimal_trip arg2 ; RL,RM,RH (24 bit result)
    lda #' '
    jsr screen_putchar
    load_frame_var 2 ; I
    inc
    sta arg2 ; I+1
    copy_code_pointer_to_local fib_iter.static_closure, fp
    jmp fib_iter.code
.roots:
    rts
.evac:
    evacuate 3
.scav:
    scavenge_done 3
