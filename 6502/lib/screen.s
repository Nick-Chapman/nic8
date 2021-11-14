
;; put* functions write to the in-memory screen
putchar:
    ldx g_screen_pointer
    sta g_screen,x
    inc g_screen_pointer
    rts

return_home:
    lda #0
    sta g_screen_pointer
    rts

init_screen:
    jsr return_home
    ldx #0
    lda #' '
each_pat_char:
    sta g_screen,x
    inx
    sec
    cpx #32
    bne each_pat_char
    rts

;; print_* functions write to the underlying lcd
print_screen:
    ;; copy screen to lcd
    jsr lcd_return_home
    ldx #0
each_line1_char:
    lda g_screen,x
    jsr print_char
    inx
    sec
    cpx #16
    bne each_line1_char
    ;; reposition to line2. do 24 dummy prints
    lda #'+' ;dont expect to see this
    ldx #24
each_dummy_print:
    jsr print_char
    dex
    bne each_dummy_print
    ldx #16
each_line2_char:
    lda g_screen,x
    jsr print_char
    inx
    sec
    cpx #32
    bne each_line2_char
    rts
