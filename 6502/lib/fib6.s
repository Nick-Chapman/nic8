;;; PROVIDES: fib6
;;; Another FORTH style version (Like fib4/5)
;;; This time we keep the stack contents in High Mem
;;; But access it using indirect indexing from a ZP pointer
;;; We can still access cells under the top -- by setting y
;;; But pushing and popping requies we update the pointer
;;; Upside, stack can be as big as we like. And y can be used elsewhere

STACK_TOP = $6ff

;;; word in ZP
g_stack = 0 ; Really we should define the "g_" vars in the calling program


push: ; only moves the pointer; copied NO data
    sec
    lda g_stack
    sbc #1
    sta g_stack
    bcs push2
    dec g_stack + 1
push2:
    rts

pop: ; only moves the pointer; copies NO data
    clc
    lda g_stack
    adc #1
    sta g_stack
    bcc pop2
    inc g_stack + 1
pop2:
    rts


fib6_name:
    string "6: Forth: (sp)"
    word fib6_name
fib6_entry:

    ;; 16-bit quantities are pushed: push-HI, push-LO
    ;; so the bytes are ordered LO;HI in standard little endian order

    pha

    lda #<STACK_TOP
    sta g_stack
    lda #>STACK_TOP
    sta g_stack + 1

    pla
    sta (g_stack)
    jsr push ; TODO: use a macro!

    ;;      0  1 ...
    ;;         N
    jsr fib6_recurse
    ;;      RL RH       -- RL;RH = fib(N)
    ;;   0  1  2 ...

    tsx
    ldy #1
    lda (g_stack),y
    sta $103,x
    iny
    lda (g_stack),y
    sta $104,x

    rts


fib6_recurse:

    ;;                 N
    ;;             N-1 N
    ;;         BL  BH  N
    ;;     N-2 BL  BH  N
    ;; AL  AH  BL  BH  N
    ;;             RL  RH

    ldy #1
    lda (g_stack),y

    sec
    cmp #2
    bcc fib6_base

    sbc #1
    sta (g_stack)
    jsr push
    jsr fib6_recurse

    ;; copy over
    ldy #3
    lda (g_stack),y
    sec
    sbc #2
    sta (g_stack)
    jsr push
    jsr fib6_recurse

    ;;   0  1  2  3  4  5 ...
    ;;      AL AH BL BH,
    ;; -->           RL RH
    ;;            0  1  2 ...

    clc
    ldy #1
    lda (g_stack),y ; AL
    iny
    iny
    adc (g_stack),y ; BL
    tax ; save RL until we are done BH
    dey
    lda (g_stack),y ; AH
    iny
    iny
    adc (g_stack),y ; BH
    iny
    sta (g_stack),y ; RH
    txa
    dey
    sta (g_stack),y ; RL
    jsr pop
    jsr pop
    jsr pop

    rts

fib6_base:
    ;; correct lo is on stack
    ;; must push 0-hi under it

    ;;      0  1 ...
    ;;         X
    ;;      X  #0
    ;;   0  1  2 ...

    ldy #1
    lda (g_stack),y ; X (0/1) ->
    sta (g_stack)   ; LO-res
    lda #0
    sta (g_stack),y ; HI-res is 0

    jsr push
    rts
