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
