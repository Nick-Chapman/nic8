
;;; TODO: which macros are used; and where?

copy_word: macro source, dest
    lda \source
    sta \dest
    lda \source + 1
    sta \dest + 1
endmacro

copy16_literal_to_var: macro lit, V
    lda #<\lit
    sta \V
    lda #>\lit
    sta \V + 1
endmacro

copy_code_pointer_to_heap0: macro code ; TODO: avoid special case for 0
    lda #<\code
    store_heap0
    lda #>\code
    store_heap 1
endmacro

copy_code_pointer_to_local: macro code, L ; TODO: same as copy16_literal_to_var
    lda #<\code
    sta \L
    lda #>\code
    sta \L + 1
endmacro

store_heap0: macro ; TODO: avoid special case for 0
    sta (clo)
endmacro

store_heap: macro N ; must follow alloc
    ldy #\N
    sta (clo),y ; TODO: unify (fp) and (hp) versions
endmacro

load_frame_var0: macro ; TODO: avoid special case
    lda (fp)
endmacro

load_frame_var: macro N
    ldy #\N
    lda (fp),y
endmacro

copy_byte_local_to_heap: macro L, H
    lda \L
    store_heap \H
endmacro

copy_word_local_to_heap: macro L, H
    lda \L
    store_heap \H
    lda \L+1
    store_heap \H+1
endmacro

copy_word_from_frame0: macro dest ; TODO: avoid special case
    load_frame_var0
    sta \dest
    load_frame_var 1
    sta \dest + 1
endmacro

copy_word_from_frame: macro F, L
    load_frame_var \F
    sta \L
    load_frame_var \F+1
    sta \L + 1
endmacro

copy_word_frame_to_heap: macro F, H
    load_frame_var \F
    store_heap \H
    load_frame_var \F+1
    store_heap \H+1
endmacro

;;; Arithmetic
sub16 : macro A, B, RES
    sec
    lda \A
    sbc \B
    sta \RES
    lda \A + 1
    sbc \B + 1
    sta \RES + 1
endmacro

inc16_var: macro V
    ;; can I use inc? Does this set the carry flag. No!
    ;; But that's ok because it does set zero.
    inc \V
    bne _done$
    inc \V + 1
_done$:
endmacro
