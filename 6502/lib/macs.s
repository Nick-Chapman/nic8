
saveA: macro P, N
    ldy #\N
    sta (\P),y
endmacro

loadA: macro P, N
    ldy #\N
    lda (\P),y
endmacro


load8: macro P,N, V
    loadA \P, \N
    sta \V
endmacro

load8_x: macro P,N, V
    loadA \P, \N
    sta \V, x
endmacro

save8: macro V, P,N
    lda \V
    saveA \P, \N
endmacro

save8_x: macro V, P,N
    lda \V, x
    saveA \P, \N
endmacro

copy8: macro A, B
    lda \A
    sta \B
endmacro

copy8_x: macro A, B
    lda \A, x
    sta \B, x
endmacro

copyFrom8_x: macro A, B
    lda \A, x
    sta \B
endmacro

copyTo8_x: macro A, B
    lda \A
    sta \B, x
endmacro

transfer8: macro P,I, Q,J
    loadA \P, \I
    saveA \Q, \J
endmacro


load16: macro P,N, V
    load8 \P,\N, \V
    load8 \P,\N+1, \V+1
endmacro

load16_x: macro P,N, V
    load8_x \P,\N, \V
    load8_x \P,\N+1, \V+1
endmacro

save16: macro V, P,N
    save8 \V, \P, \N
    save8 \V+1, \P, \N+1
endmacro

save16_x: macro V, P,N
    save8_x \V, \P, \N
    save8_x \V+1, \P, \N+1
endmacro

copy16: macro A, B
    copy8 \A, \B
    copy8 \A+1, \B+1
endmacro

copy16_x: macro A, B
    copy8_x \A, \B
    copy8_x \A+1, \B+1
endmacro

copyFrom16_x: macro A, B
    copyFrom8_x \A, \B
    copyFrom8_x \A+1, \B+1
endmacro

copyTo16_x: macro A, B
    copyTo8_x \A, \B
    copyTo8_x \A+1, \B+1
endmacro

transfer16: macro P,I, Q,J
    transfer8 \P, \I, \Q, \J
    transfer8 \P, \I+1, \Q, \J+1
endmacro

store8i: macro IMM, V
    lda #\IMM
    sta \V
endmacro

store16i: macro IMM, V
    lda #<\IMM
    sta \V
    lda #>\IMM
    sta \V + 1
endmacro

store16i_x: macro IMM, V
    lda #<\IMM
    sta \V, x
    lda #>\IMM
    sta \V + 1, x
endmacro

;;; ----------------------------------------------------------------------
;;; special case for offset-0

save16i_0: macro IMM, P
    lda #<\IMM
    sta (\P)
    lda #>\IMM
    saveA \P, 1
endmacro

load16_0: macro P, V
    lda (\P)
    sta \V
    load8 \P, 1, \V + 1
endmacro
