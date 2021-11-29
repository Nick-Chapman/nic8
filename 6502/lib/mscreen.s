;;; Multi screens

;;; REQUIRES: g_screen, g_screen_pointer
;;; PROVIDES: init_screen, screen_flush, screen_putchar, screen_newline, screen_return_home


;; 4 screens

init_screen_pointers:
    lda #0
    sta g_screen_pointers
    lda #32
    sta g_screen_pointers + 1
    lda #(2*32)
    sta g_screen_pointers + 2
    lda #(3*32)
    sta g_screen_pointers + 3
    rts

init_mscreen:
    stz g_selected_screen
    jsr init_screen_pointers
    ldx #0
    lda #' '
.each_char:
    sta g_screens,x
    inx
    sec
    cpx #(4 * 32)
    bne .each_char
    rts

;; print screen to the underlying lcd
screen_flush:
    pha
    ;; copy screen to lcd
    jsr lcd_return_home
    ldx #0
    tay
    lda starts,y
    tay
.each_line1_char:
    lda g_screens,y
    jsr lcd_putchar
    inx
    iny
    sec
    cpx #16
    bne .each_line1_char
    ;; reposition to line2. do 24 dummy prints
    lda #'+' ;dont expect to see this
    ldx #24
.each_dummy_print:
    jsr lcd_putchar
    dex
    bne .each_dummy_print
    ldx #16
.each_line2_char:
    lda g_screens,y
    jsr lcd_putchar
    inx
    iny
    sec
    cpx #32
    bne .each_line2_char
    pla
    rts

screen_putchar:
    cmp #13 ; carriage return (ASCII 13) as added by str directive
    beq screen_newline
    jsr maybe_scroll

    ldy g_selected_screen
    ldx g_screen_pointers,y
    sta g_screens,x
    inx
    stx g_screen_pointers,y
    rts

screen_newline:
    pha
    ldy g_selected_screen
    lda g_screen_pointers,y
    cmp eol1s,y
    bmi _$
    jsr scrollup
_$:
    jsr return_to_start_line2
    pla
    rts

maybe_scroll:
    pha
    ldy g_selected_screen
    lda g_screen_pointers,y
    cmp eol2s,y
    bne _$
    jsr scrollup
    jsr return_to_start_line2
_$:
    pla
    rts

scrollup:
    ldx #0
    ldy g_selected_screen
    lda starts,y
    tay
.each_char:
    lda g_screens+16,y
    sta g_screens,y
    lda #' '
    sta g_screens+16,y
    inx
    iny
    sec
    cpx #16
    bne .each_char
    rts

return_to_start_line2:
    ldy g_selected_screen
    lda eol1s,y
    sta g_screen_pointers,y
    rts

screen_return_home:
    ldy g_selected_screen
    lda starts,y
    sta g_screen_pointers,y
    rts


starts:
    byte 0,32,64,96
eol1s:
    byte 16,48,80,112
eol2s:
    byte 32,64,96,128
