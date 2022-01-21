
;;; Support for multiple virtual screens
;;; 4 screens -- TODO: 8 screens!

screen:

.init_pointers: ; TODO: use loop
    lda #0
    sta g_screen_pointers
    lda #32
    sta g_screen_pointers + 1
    lda #(2*32)
    sta g_screen_pointers + 2
    lda #(3*32)
    sta g_screen_pointers + 3
    rts

.init:
    jsr screen.flush_init
    stz g_selected_screen
    jsr .init_pointers
    ldx #0
    lda #' '
.init_each_char:
    sta g_screens,x
    inx
    sec
    cpx #(4 * 32)
    bne .init_each_char
    rts

;;; flush screen (screen# passed in acc) to the underlying lcd
.flush:
    phx
    pha
    ;jsr screen.show_number_in_corner
    pla
    ;; copy screen to lcd
    jsr lcd.return_home
    ldx #0
    tay
    lda screen.starts,y
    tay
.flush_each_line1_char:
    lda g_screens,y
    jsr lcd.putchar
    inx
    iny
    sec
    cpx #16
    bne .flush_each_line1_char
    ;; reposition to line2. do 24 dummy prints ; TODO: avoid the dummy prints
    lda #'+' ;dont expect to see this
    ldx #24
.flush_each_dummy_print:
    jsr lcd.putchar
    dex
    bne .flush_each_dummy_print
    ldx #16
.flush_each_line2_char:
    lda g_screens,y
    jsr lcd.putchar
    inx
    iny
    sec
    cpx #32
    bne .flush_each_line2_char
    plx
    rts

.show_number_in_corner:
    tay
    lda screen.digits,y
    ldx screen.eol2s,y
    dex
    sta g_screens,x
    ldx screen.eol1s,y
    dex
    sta g_screens,x
    rts

.putchar:
    cmp #13 ; carriage return (ASCII 13) as added by str directive
    beq screen.newline
    cmp #10 ; carriage return (ASCII 13) as added by str directive
    beq screen.newline
    phx
    jsr screen.maybe_scroll
    ldy g_selected_screen
    ldx g_screen_pointers,y
    sta g_screens,x
    inx
    stx g_screen_pointers,y
    plx
    rts

.newline:
    phx
    pha
    ldy g_selected_screen
    lda g_screen_pointers,y
    cmp screen.eol1s,y
    bmi .newline_skip
    jsr screen.scrollup
.newline_skip:
    jsr screen.return_to_start_line2
    pla
    plx
    rts

.maybe_scroll:
    pha
    ldy g_selected_screen
    lda g_screen_pointers,y
    cmp screen.eol2s,y
    bne .scroll_skip
    jsr screen.scrollup
    jsr screen.return_to_start_line2
.scroll_skip:
    pla
    rts

.scrollup:
    ldx #0
    ldy g_selected_screen
    lda screen.starts,y
    tay
.scrollup_each_char:
    lda g_screens+16,y
    sta g_screens,y
    lda #' '
    sta g_screens+16,y
    inx
    iny
    sec
    cpx #16
    bne .scrollup_each_char
    rts

.return_to_start_line2:
    ldy g_selected_screen
    lda screen.eol1s,y
    sta g_screen_pointers,y
    rts

.return_home:
    ldy g_selected_screen
    lda screen.starts,y
    sta g_screen_pointers,y
    rts

.starts:
    byte 0,32,64,96

.eol1s:
    byte 16,48,80,112
.eol2s:
    byte 32,64,96,128

.digits: ascii "0123456789abcdef"

.flush_when_time: ; should be called every jiffy
    lda g_ticks
    sec
    sbc g_next_screen_flush
    bpl .flush_now
    rts
.flush_now:
    lda g_nmi_count
    and #(NUM_SCREENS - 1)
    jsr screen.flush
    jsr screen.flush_init
    rts

.flush_init:
    lda g_ticks
    clc
    adc #5 ; 20 times/sec ; TODO: bit slower, say 10/sec to avoid waiting on lcd
    sta g_next_screen_flush
    rts

;;; macros
screen_flush_selected: macro ; whatever screen is selected for writing
    pha
    lda g_selected_screen
    jsr screen.flush
    pla
endmacro
