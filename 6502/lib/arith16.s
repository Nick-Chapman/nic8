
increment16: macro A
    inc \A
    bne .\@
    inc \A + 1
.\@:
endmacro

add16: macro A, B, RES ; A+B --> RES
    clc
    lda \A
    adc \B
    sta \RES
    lda \A + 1
    adc \B + 1
    sta \RES + 1
endmacro

sub16 : macro A, B, RES ; A-B --> RES
    sec
    lda \A
    sbc \B
    sta \RES
    lda \A + 1
    sbc \B + 1
    sta \RES + 1
endmacro

compare16: macro A,B ; if B > A, clear carry
    sec
    lda \A + 1
    cmp \B + 1
    bne .\@
    lda \A
    cmp \B
.\@:
endmacro
