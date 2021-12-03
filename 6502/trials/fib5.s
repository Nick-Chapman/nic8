;;; PROVIDES: fib5
;;; Like fib4, except we keep the stack not in ZP but in Higher Mem
;;; Although we still use y to index it,so it can only have 256 bytes

STACK = $700

fib5_name:
    string "5: Forth MEM" ; TODO: BUG: this message is not being shown ?!?
    word fib5_name
fib5_entry:

    ;; Stack in ZP, starting at top ($ff) and growing downwards
    ;; y register contains address of next empty cell

    ;; PUSH   : sta 0+STACK,y ; dey
    ;; POP    : iny; lda 0+STACK,y
    ;; GET(N) : lda N+STACK,y

    ;; 16-bit quantities are pushed: push-HI, push-LO
    ;; so the bytes are ordered LO;HI in standard little endian order

    ldy #$ff ; initialize stack

    ;;         0
    sta 0+STACK,y
    dey
    ;;      0  1 ...
    ;;         N
    jsr fib5_recurse
    ;;      RL RH       -- RL;RH = fib(N)
    ;;   0  1  2 ...

    tsx
    lda 1+STACK,y
    sta $103,x
    lda 2+STACK,y
    sta $104,x

    rts


fib5_recurse:

    ;;                 N
    ;;             N-1 N
    ;;         BL  BH  N
    ;;     N-2 BL  BH  N
    ;; AL  AH  BL  BH  N
    ;;             RL  RH

    lda 1+STACK,y

    sec
    cmp #2
    bcc fib5_base

    sbc #1
    sta 0+STACK,y
    dey
    jsr fib5_recurse

    ;; copy over
    lda 3+STACK,y
    sec
    sbc #2
    sta 0+STACK,y
    dey
    jsr fib5_recurse

    ;;   0  1  2  3  4  5 ...
    ;;      AL AH BL BH,
    ;; -->           RL RH
    ;;            0  1  2 ...

    clc
    lda 1+STACK,y ; AL
    adc 3+STACK,y ; BL
    tax ; save RL until we are done BH
    lda 2+STACK,y ; AH
    adc 4+STACK,y ; BH
    sta 5+STACK,y ; RH
    txa
    sta 4+STACK,y ; RL
    iny
    iny
    iny

    rts

fib5_base:
    ;; correct lo is on stack
    ;; must push 0-hi under it

    ;;      0  1 ...
    ;;         X
    ;;      X  #0
    ;;   0  1  2 ...
    dey ; first (confusing!)

    lda 2+STACK,y ; X (0/1) ->
    sta 1+STACK,y ; LO-res
    lda #0
    sta 2+STACK,y ; HI-res is 0
    rts
