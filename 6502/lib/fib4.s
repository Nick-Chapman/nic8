;;; PROVIDES: fib4

fib4_name:
    .string "4: Forth ZP"
    .word fib4_name
fib4_entry:

    ;; Stack in ZP, starting at top ($ff) and growing downwards
    ;; y register contains address of next empty cell

    ;; PUSH   : sta 0,y ; dey
    ;; POP    : iny; lda 0,y
    ;; GET(N) : lda N,y

    ;; 16-bit quantities are pushed: push-HI, push-LO
    ;; so the bytes are ordered LO;HI in standard little endian order

    ldy #$ff ; initialize stack

    ;;         0
    sta 0,y
    dey
    ;;      0  1 ...
    ;;         N
    jsr fib4_recurse
    ;;      RL RH       -- RL;RH = fib(N)
    ;;   0  1  2 ...

    tsx
    lda 1,y
    sta $103,x
    lda 2,y
    sta $104,x

    rts


fib4_recurse:

    ;;                 N
    ;;             N-1 N
    ;;         BL  BH  N
    ;;     N-2 BL  BH  N
    ;; AL  AH  BL  BH  N
    ;;             RL  RH

    lda 1,y

    sec
    cmp #2
    bcc fib4_base

    sbc #1
    sta 0,y
    dey
    jsr fib4_recurse

    ;; copy over
    lda 3,y
    sec
    sbc #2
    sta 0,y
    dey
    jsr fib4_recurse

    ;;   0  1  2  3  4  5 ...
    ;;      AL AH BL BH,
    ;; -->           RL RH
    ;;            0  1  2 ...

    clc
    lda 1,y ; AL
    adc 3,y ; BL
    tax ; save RL until we are done BH
    lda 2,y ; AH
    adc 4,y ; BH
    stx 4,y ; RL
    sta 5,y ; RH
    iny
    iny
    iny

    rts

fib4_base:
    ;; correct lo is on stack
    ;; must push 0-hi under it

    ;;      0  1 ...
    ;;         X
    ;;      X  #0
    ;;   0  1  2 ...
    dey ; first (confusing!)

    lda 2,y ; X (0/1) ->
    sta 1,y ; LO-res
    lda #0
    sta 2,y ; HI-res is 0
    rts
