;;; REQUIRES: g_screen, g_screen_pointer
;;; PROVIDES: init_screen, print_screen, screen_putchar, screen_newline, screen_return_home

screen_return_home:
    lda #0
    sta g_screen_pointer
    rts

init_screen:
    jsr screen_return_home
    ldx #0
    lda #' '
each_pat_char:
    sta g_screen,x
    inx
    sec
    cpx #32
    bne each_pat_char
    rts

;; print screen to the underlying lcd
print_screen:
    pha
    ;; copy screen to lcd
    jsr lcd_return_home
    ldx #0
each_line1_char:
    lda g_screen,x
    jsr lcd_putchar
    inx
    sec
    cpx #16
    bne each_line1_char
    ;; reposition to line2. do 24 dummy prints
    lda #'+' ;dont expect to see this
    ldx #24
each_dummy_print:
    jsr lcd_putchar
    dex
    bne each_dummy_print
    ldx #16
each_line2_char:
    lda g_screen,x
    jsr lcd_putchar
    inx
    sec
    cpx #32
    bne each_line2_char
    pla
    rts

screen_putchar:
    cmp #13 ; carriage return (ASCII 13) as added by str directive
    beq screen_newline
    jsr maybe_scroll
    jmp local_screen_putchar_raw

local_screen_putchar_raw:
    ldx g_screen_pointer
    sta g_screen,x
    inc g_screen_pointer
    rts

screen_newline:
    pha
    lda g_screen_pointer
    cmp #16
    bmi after_scrollup
    jsr scrollup
after_scrollup:
    jsr local_screen_return_to_start_line2
    pla
    rts

maybe_scroll:
    pha
    lda g_screen_pointer
    cmp #32
    bne maybe_scroll_done
    jsr scrollup
    jsr local_screen_return_to_start_line2
maybe_scroll_done:
    pla
    rts

scrollup:
    ldx #0
each_scroll_char: ; TODO: local label
    lda g_screen+16,x
    sta g_screen,x
    lda #' '
    sta g_screen+16,x
    inx
    sec
    cpx #16
    bne each_scroll_char
    rts

local_screen_return_to_start_line2:
    lda #16
    sta g_screen_pointer
    rts
