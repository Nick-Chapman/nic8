;;; REQUIRES: g_arg (byte), g_res (word)
;;; PROVIDES: fib1

;;; TODO: use local lables!
fib1_name:
    .string "ZP arg/res"
    .word fib1_name
fib1_entry:
    sta g_arg
    jsr fib1_recurse
    lda g_res
    tsx
    sta $103,x
    lda g_res + 1
    sta $104,x
    rts

;;; compute fib on stack; arg/result passed in globals
fib1_recurse:
    lda g_arg
    sec
    cmp #2
    bcc fib1_base
    pha ; n
    sec
    sbc #1
    sta g_arg
    ;; compute: fib (n-1)...
    jsr fib1_recurse
    pla ; n
    sta g_arg

    lda g_res + 1
    pha ; fib (n-1) HI
    lda g_res
    pha ; fib (n-1) LO

    lda g_arg
    sec
    sbc #2
    sta g_arg
    ;; compute: fib (n-2)...
    jsr fib1_recurse
    pla ; fib (n-1) LO
    clc
    adc g_res
    sta g_res

    pla ; fib (n-1) HI
    adc g_res + 1
    sta g_res + 1

    rts

fib1_base:
    sta g_res
    lda #0
    sta g_res + 1
    rts
