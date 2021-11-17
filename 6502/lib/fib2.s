;;; REQUIRES: g_arg (byte), g_res (word)
;;; PROVIDES: fib1

;;; compute fib purely on stack; don't use globals
;;; caller reserves 2 byte space for result (push hi; lo)
;;; then pushes the 1 byte argument

fib2:
    tsx
    lda $103,x ; arg (under 2 byte ret address)

    sec
    cmp #2
    bcc fib2_base

    pha ; space for 1st recursive call result: hi
    pha ; then lo
    sec
    sbc #1
    pha
    jsr fib2
    pla

    tsx
    lda $105,x ; same arg (also under 1st recursive result)
    pha ; space for 2nd recursive call result: hi
    pha ; then lo
    sec
    sbc #2
    pha
    jsr fib2
    pla

    tsx
    clc
    lda $101,x
    adc $103,x
    sta $108,x ; my-res-lo, under 4 bytes sub-results; 2 byte return address; 1 byte my-arg
    lda $102,x
    adc $104,x
    sta $109,x ; my-res-hi, 1 byte higher
    pla
    pla
    pla
    pla
    rts

fib2_base:
    sta $104,x ; lo
    lda #0
    sta $105,x ; hi
    rts