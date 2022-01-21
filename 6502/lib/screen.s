;;; Support for multiple virtual screens

;;; REQUIRES: g_screen, g_screen_pointer
;;; PROVIDES: init_screen, screen_flush, screen_putchar, screen_newline, screen_return_home

;; 4 screens -- TODO: 8 screens!

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

init_screen:
    jsr screen_flush_init
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

screen_flush_selected: macro ; whatever screen is selected for writing
    pha
    lda g_selected_screen
    jsr screen_flush_sub
    pla
endmacro

;; flush screen (# passed in acc) to the underlying lcd
screen_flush_sub:
    phx
    pha
    ;jsr show_screen_number_in_corner
    pla
    ;; copy screen to lcd
    jsr lcd.return_home
    ldx #0
    tay
    lda starts,y
    tay
.each_line1_char:
    lda g_screens,y
    jsr lcd.putchar
    inx
    iny
    sec
    cpx #16
    bne .each_line1_char
    ;; reposition to line2. do 24 dummy prints ; TODO: avoid the dummy prints
    lda #'+' ;dont expect to see this
    ldx #24
.each_dummy_print:
    jsr lcd.putchar
    dex
    bne .each_dummy_print
    ldx #16
.each_line2_char:
    lda g_screens,y
    jsr lcd.putchar
    inx
    iny
    sec
    cpx #32
    bne .each_line2_char
    plx
    rts

show_screen_number_in_corner:
    tay
    lda digits,y
    ldx eol2s,y
    dex
    sta g_screens,x
    ldx eol1s,y
    dex
    sta g_screens,x
    rts

screen_putchar:
    cmp #13 ; carriage return (ASCII 13) as added by str directive
    beq screen_newline
    cmp #10 ; carriage return (ASCII 13) as added by str directive
    beq screen_newline
    phx
    jsr maybe_scroll
    ldy g_selected_screen
    ldx g_screen_pointers,y
    sta g_screens,x
    inx
    stx g_screen_pointers,y
    plx
    rts

screen_newline:
    phx
    pha
    ldy g_selected_screen
    lda g_screen_pointers,y
    cmp eol1s,y
    bmi .skip
    jsr scrollup
.skip:
    jsr return_to_start_line2
    pla
    plx
    rts

maybe_scroll:
    pha
    ldy g_selected_screen
    lda g_screen_pointers,y
    cmp eol2s,y
    bne .skip
    jsr scrollup
    jsr return_to_start_line2
.skip:
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

digits: ascii "0123456789abcdef"

screen_flush_when_time: ; should be called every jiffy
    lda g_ticks
    sec
    sbc g_next_screen_flush
    bpl .now
    rts
.now:
    lda g_nmi_count
    and #(NUM_SCREENS - 1)
    jsr screen_flush_sub
    jsr screen_flush_init
    rts

screen_flush_init:
    lda g_ticks
    clc
    adc #5 ; 20 times/sec ; TODO: bit slower, say 10/sec to avoid waiting on lcd
    sta g_next_screen_flush
    rts
