;;; TODO: sort out stuff in here.

;;; put byte passed in accumulator as 2 digit hex number
put_hex:
    pha
    lsr
    lsr
    lsr
    lsr
    tax
    lda digits,x
    jsr scrolling_putchar
    pla
    and #%1111
    tax
    lda digits,x
    jsr scrolling_putchar
    rts

digits: ascii "0123456789abcdef"

;;; left-over from fib7 split...

put_hex_byte: ; TODO: review this code. wrote it a long time ago!
    pha
    lsr
    lsr
    lsr
    lsr
    tax
    lda digits,x
    jsr screen_putchar
    pla
    and #%1111
    tax
    lda digits,x
    jsr screen_putchar
    rts

digits: ascii "0123456789abcdef"

tiny_pause:
    pha
    lda #3
    jsr sleep_blocking
    pla
    rts

print_hex_word: macro L
    ;lda #'['
    ;jsr screen_putchar
    lda \L + 1
    jsr put_hex_byte
    lda \L
    jsr put_hex_byte
    ;lda #']'
    ;jsr screen_putchar
    jsr print_screen
    ;lda #5
    ;jsr sleep_blocking
endmac


copy_word_local_to_heap: macro L, H
    lda \L
    store_heap \H
    lda \L+1
    store_heap \H+1
endmacro


wipe_space_a:
    lda #0
    sta temp
    lda #>SPACE_A_START
    sta temp + 1
outer_loop$:
    lda temp + 1
    ldy $0
inner_loop$:
    lda $ee
    sta (temp),y
    iny
    bne inner_loop$
    lda temp + 1
    inc
    sta temp + 1
    cmp #>SPACE_A_END
    bne outer_loop$
    rts

wipe_space_b:
    lda #0
    sta temp
    lda #>SPACE_B_START
    sta temp + 1
outer_loop$:
    lda temp + 1
    ldy $0
inner_loop$:
    lda $ee
    sta (temp),y
    iny
    bne inner_loop$
    lda temp + 1
    inc
    sta temp + 1
    cmp #>SPACE_B_END
    bne outer_loop$
    rts
