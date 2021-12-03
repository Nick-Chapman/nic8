
    include fib16.s

fib7_name:
    string "7: Heap"
    word fib7_name
fib7_entry:
    init_gc 1
    jmp fib7_entry_shared

fib7_entry_shared:
    ;; N(acc) --> fib7 [N KL KH] where K is fib7_done []
    sta 0 ; N
    copy16_literal_to_var 0, gc_count
    ;; allocate final continuation -- TODO: no need for this to be heap allocated
    lda #2
    jsr alloc
    ;; fill in closure
    copy_code_pointer_to_heap0 fib7_done.code
    ;; setup args
    copy_word clo, 1
    copy_code_pointer_to_local fib_recurse.static_closure, fp
    jmp fib_recurse.code ; TODO: Use a standard entry protocol, 'enter'


;;; RL RH -->
fib7_done:
    word .roots, .evac, .scav ; TODO capture common pattern for def/.code
.code:
    ;; move final result to pre-allocated space on stack
    tsx
    lda 0
    sta $103,x
    lda 1
    sta $104,x
    rts ; return to original caller
.roots:
    impossible_roots
.evac:
    evacuate 2
.scav:
    scavenge_done 2
