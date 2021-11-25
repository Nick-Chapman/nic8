;;; PROVIDES: fib3

;;; This version of fib uses the exact same calling convention as chosen for the standard interface:
;;; - Caller reserves 2 byte space for result in the page-1 stack (push hi; lo).
;;; - Then passes the argument N in the accumulator.
;;; (So the entry shim does nothing)

fib3_name:
    asciiz "3: A -> ST"
    word fib3_name
fib3_entry:
    pha ; res-hi
    pha ; res-lo
    jsr fib3_recurse ; code under test!
    tsx
    pla ; res-lo
    sta $105,x
    pla ; res-hi
    sta $106,x
    rts

fib3_recurse:
    sec
    cmp #2
    bcc fib3_base

    pha ; save my-arg as stack-temp

    pha ; space for 1st recursive call result: hi
    pha ; then lo
    sec
    sbc #1 ; N-1 in Acc
    jsr fib3_recurse

    tsx
    lda $103,x ; access my-arg as stack-temp

    pha ; space for 2nd recursive call result: hi
    pha ; then lo
    sec
    sbc #2 ; N-2 in Acc
    jsr fib3_recurse

    tsx
    clc
    lda $101,x
    adc $103,x
    sta $108,x ; my-res-lo, under 4 bytes sub-results; 1 byte my-arg saved as temp; 2 byte return address
    lda $102,x
    adc $104,x
    sta $109,x ; my-res-hi, 1 byte higher
    pla
    pla
    pla
    pla
    pla ; pop my-arg stack-temp
    rts

fib3_base:
    tsx
    sta $103,x ; lo
    lda #0
    sta $104,x ; hi
    rts
