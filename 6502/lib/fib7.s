
;;; So, finally. CPS version... which allocates stack frames on a heap.
;;; I'm worried this will be really hard to get working
;;; Even before attemping to write the GC!

;;; arguments/results of functions/continutaion are in ZP vars: 0,1,...
;;; L/H are lo/hi bytes of a 16 bit value

;;; Heap at top of available memory - only have 16k available of my 32k SRAM :(
HEAP_TOP = $4000

;;; Heap pointer and frame pointer in ZP
hp = 200
fp = 202
cp = 204

;;; allocate [n] words in the heap; adjusting HP -- TODO: this should be a macro!
n = 206
alloc:
    ;lda #'a'
    ;jsr debug
    sec
    lda hp
    sbc n
    sta hp
    bcs alloc_done
    dec hp + 1
alloc_done:
    rts

;debug:
;    jsr screen_putchar
;    jsr print_screen
;    rts

fib7_name:
    .string "7: CPS/Heap"
    .word fib7_name
fib7_entry:
    ;; N(acc) --> fib7 [N KL KH] where K is fib7_done []
    sta 0
    ;lda #'e'
    ;jsr debug
    ;; initialize heap
    lda #<HEAP_TOP
    sta hp
    lda #>HEAP_TOP
    sta hp + 1
    ;; allocate final continuation
    lda #2
    sta n
    jsr alloc
    ;; fill in the allocated structure
    lda #<fib7_done
    sta (hp)
    lda #>fib7_done
    ldy #1
    sta (hp),y
    ;; setup args
    lda hp
    sta 1
    lda hp + 1
    sta 2
    jmp fib7_recurse

;;; RL RH -->
fib7_done:
    ;lda #'d'
    ;jsr debug
    ;; move final results from ZP vars to pre-allocated space on stack
    tsx
    lda 0
    sta $103,x
    lda 1
    sta $104,x
    rts ; return to original caller

;;; [] N KL KH --> fib7 [N-1 JL JH] where J is fib7_cont1 [N KL KH]
fib7_recurse:
    ;lda #'r'
    ;jsr debug
    ;; access N
    lda 0
    sec
    cmp #2
    bcc fib7_base
    ;; allocate cont1
    lda #5
    sta n
    jsr alloc
    ;; fill in closure
    lda #<fib7_cont1
    sta (hp)
    lda #>fib7_cont1
    ldy #1
    sta (hp),y
    lda 0
    iny
    sta (hp),y
    lda 1
    iny
    sta (hp),y
    lda 2
    iny
    sta (hp),y
    ;; setup args
    ;; decrement N --> 0
    lda 0
    sec
    sbc #1
    sta 0
    lda hp
    sta 1
    lda hp + 1
    sta 2
    jmp fib7_recurse


;;; N(acc) KL KH --> K [N 0]
fib7_base:
    ;lda #'b'
    ;jsr debug
    ;; move K into fp
    lda 1
    sta fp
    lda 2
    sta fp + 1
    ;; RL is N (already in 0)
    lda #0
    sta 1 ; setup RH
    jmp enter_fp

enter_fp:
    ;lda #'E'
    ;jsr debug
    lda (fp)
    sta cp
    ldy #1
    lda (fp),y
    sta cp + 1
    jmp (cp)

;;; [. . N KL KH] AL AH -->  fib7 [N-2 JL JH] where J is fib7_cont2 [AL AH KL KH]
fib7_cont1:
    ;; allocate cont2
    lda #6
    sta n
    jsr alloc
    ;; fill in closure
    lda #<fib7_cont2
    sta (hp)
    lda #>fib7_cont2
    ldy #1
    sta (hp),y
    lda 0 ; AL
    ldy #2
    sta (hp),y
    lda 1 ; AH
    ldy #3
    sta (hp),y
    ldy #3
    lda (fp),y ; KL
    ldy #4
    sta (hp),y
    ldy #4
    lda (fp),y ; KH
    ldy #5
    sta (hp),y
    ;; setup args
    ldy #2
    lda (fp),y ; N
    sec
    sbc #2
    sta 0
    lda hp
    sta 1
    lda hp + 1
    sta 2
    jmp fib7_recurse

;;; [. . AL AH KL KH] BL BH --> RL RH (where R = A + B)
fib7_cont2:
    ;; 16-bit addition
    clc
    ldy #2
    lda (fp),y ; AL
    adc 0 ; BL
    sta 0 ; RL
    ldy #3
    lda (fp),y ; AH
    adc 1 ; BH
    sta 1 ; RH
    ;; return to caller
    ldy #4
    lda (fp),y ; KL
    pha
    ldy #5
    lda (fp),y ; KH
    sta fp + 1
    pla
    sta fp
    jmp enter_fp
