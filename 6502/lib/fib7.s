
;;; CPS version of fib. Allocates stack frames on a heap. No GC yet. But soon!

load_frame_var0: .macro
    lda (fp)
.endmacro

load_frame_var: .macro N
    ldy #\N
    lda (fp),y
.endmacro

store_heap0: .macro
    sta (hp)
.endmacro

store_heap: .macro N
    ldy #\N
    sta (hp),y
.endmacro

copy_code_pointer_to_heap0: .macro code
    lda #<\code
    store_heap0
    lda #>\code
    store_heap 1
.endmacro

copy_word: .macro source, dest
    lda \source
    sta \dest
    lda \source + 1
    sta \dest + 1
.endmacro

copy_word_from_frame0: .macro dest
    load_frame_var0
    sta \dest
    load_frame_var 1
    sta \dest + 1
.endmacro

copy_word_frame_to_heap: .macro F, H
    load_frame_var \F
    store_heap \H
    load_frame_var \F+1
    store_heap \H+1
.endmacro

copy_word_local_to_heap: .macro L, H
    lda \L
    store_heap \H
    lda \L+1
    store_heap \H+1
.endmacro

copy_byte_local_to_heap: .macro L, H
    lda \L
    store_heap \H
.endmacro

copy_word_local_to_heap: .macro L, H
    lda \L
    store_heap \H
    lda \L+1
    store_heap \H+1
.endmacro

;;; arguments/results to functions/continutaion are in ZP vars: 0,1,...
;;; code pointers and 16 bit values are little endian lo;hi

;;; Heap at top of available memory - only have 16k available of my 32k SRAM :(
;;; heap grows downwards. TODO: must be upwards for GC-scavenge
HEAP_TOP = $4000

;;; Heap pointer and frame pointer in ZP
hp = $f0
fp = $f2
cp = $f4
n =  $f6 ; number of words. easy to avoid (passing N is acc) when heap grows upwards

;;; TODO: alloc_check
;;; allocate [n] words in the heap; adjusting hp -- TODO: better a macro?
alloc:
    sec
    lda hp
    sbc n
    sta hp
    bcs alloc_done
    dec hp + 1
alloc_done:
    rts

fib7_name:
    .string "7: CPS/Heap"
    .word fib7_name
fib7_entry:
    ;; N(acc) --> fib7 [N KL KH] where K is fib7_done []
    sta 0
    ;; initialize heap
    lda #<HEAP_TOP
    sta hp
    lda #>HEAP_TOP
    sta hp + 1
    ;; allocate final continuation -- TODO: no need for this to be heap allocated
    lda #2
    sta n
    jsr alloc
    ;; fill in closure
    copy_code_pointer_to_heap0 fib7_done
    ;; setup args
    copy_word hp, 1
    jmp fib7_recurse ; TODO: setup fp to static closure to allow GC

;;; No descriptor needed here, because we are done!
;;; RL RH -->
fib7_done:
    ;; move final result to pre-allocated space on stack
    tsx
    lda 0
    sta $103,x
    lda 1
    sta $104,x
    rts ; return to original caller

;;; [] N KL KH --> fib7 [N-1 JL JH] where J is fib7_cont1 [N KL KH]
;;; TODO: static closure can go here
;;; TODO: descriptor will go here to allow GC
    .text "fib7_recurse"
    .word 0, 0
    .byte 3
fib7_recurse:
    ;; access N
    lda 0
    sec
    cmp #2
    bcc fib7_base ; N<2 ?
    ;; allocate cont1
    lda #5
    sta n
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
    copy_word hp, 1
    jmp fib7_recurse

;;; N KL KH --> K [N #0]
fib7_base:
    ;; move K into fp
    copy_word 1,fp
    ;; RL is N (already in 0)
    lda #0
    sta 1 ; setup RH
    jmp enter_fp

;;; THE closure calling convention (better as a macro?) TODO: move up to top
enter_fp:
    copy_word_from_frame0 cp ; TODO: avoid cp; using pha/pha/rts
    jmp (cp)

;;; [. . N KL KH] AL AH -->  fib7 [N-2 JL JH] where J is fib7_cont2 [AL AH KL KH]
;;; TODO: descriptor will go here to allow GC
    .text "fib7_cont1"
    .word 0, 0
    .byte 2
fib7_cont1:
    ;; allocate cont2
    lda #6
    sta n
    jsr alloc
    ;; fill in closure
    copy_code_pointer_to_heap0 fib7_cont2
    copy_word_local_to_heap 0, 2 ; A
    copy_word_frame_to_heap 3, 4 ; K
    ;; setup args
    load_frame_var 2 ; N
    sec
    sbc #2 ; N-2
    sta 0
    copy_word hp,1
    jmp fib7_recurse

;;; TODO: need to swap A and K so heap-pointer word K comes first
;;; [. . AL AH KL KH] BL BH --> RL RH (where R = A + B)
;;; TODO: descriptor will go here to allow GC
    .text "fib7_cont2"; help human inspection of machine code!
    .word 0, 0
    .byte 2
fib7_cont2:
    ;; 16-bit addition
    clc
    ;; TODO: macro for 16 bit addition?
    load_frame_var 2 ; AL
    adc 0 ; BL
    sta 0 ; RL
    load_frame_var 3 ; AH
    adc 1 ; BH
    sta 1 ; RH
    ;; return to caller
    load_frame_var 4 ; KL
    pha ; TODO: use temp instead of pha for more regular code
    load_frame_var 5 ; KH
    sta fp + 1
    pla
    sta fp
    jmp enter_fp

    .text "fib7 *end*"
