
increment16: macro A
    inc \A
    bne .\@
    inc \A + 1
.\@:
endmacro

add16: macro A, B, C ; A+B --> C
    clc
    lda \A
    adc \B
    sta \C
    lda \A + 1
    adc \B + 1
    sta \C + 1
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
