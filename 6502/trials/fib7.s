
    include fib16.s

fib7_name:
    string "7: Heap"
    word fib7_name
fib7_entry:
    init_heap 1
    jmp fib7_entry_shared

fib7_entry_shared:
    ;; N(acc) --> fib7 [N KL KH] where K is fib7_done []
    sta arg2 ; N
    store16i 0, gc_count
    ;; allocate final continuation -- aside: there is no need for this to be heap allocated
    heap_alloc 2
    ;; fill in closure
    save16i_0 fib7_done.code, clo
    ;; setup args
    copy16 clo, arg3
    store16i fib_recurse.static_closure, fp
    jmp fib_recurse.code


;;; RL RH -->
fib7_done:
    word .roots, .evac, .scav
.code:
    ;; move final result to pre-allocated space on stack
    tsx
    lda arg2
    sta $103,x
    lda arg3
    sta $104,x
    rts ; return to original caller
.roots:
    impossible_roots
.evac:
    evacuate 2
.scav:
    scavenge_done 2
