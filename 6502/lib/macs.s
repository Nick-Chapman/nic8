
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

save8: macro V, P,N
    lda \V
    saveA \P, \N
endmacro

copy8: macro A, B
    lda \A
    sta \B
endmacro

transfer8: macro P,I, Q,J
    loadA \P, \I
    saveA \Q, \J
endmacro


load16: macro P,N, V
    load8 \P,\N, \V
    load8 \P,\N+1, \V+1
endmacro

save16: macro V, P,N
    save8 \V, \P, \N
    save8 \V+1, \P, \N+1
endmacro

copy16: macro A, B
    copy8 \A, \B
    copy8 \A+1, \B+1
endmacro

transfer16: macro P,I, Q,J
    transfer8 \P, \I, \Q, \J
    transfer8 \P, \I+1, \Q, \J+1
endmacro

store16i: macro IMM, V
    lda #<\IMM
    sta \V
    lda #>\IMM
    sta \V + 1
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
