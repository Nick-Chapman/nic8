;;; TODO: sort out stuff in here.

;;; left-over from fib7 split...

tiny_pause:
    pha
    lda #3
    jsr sleep_blocking
    pla
    rts

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





;; wipe_mem: ; wipe 62 pages: $0200 -- $3fff, with value $ee
;;     print_char 'w'
;;     lda #0
;;     sta temp
;;     lda #2
;;     sta temp + 1
;; outer_loop$:
;;     lda temp + 1
;;     ldy $0
;; inner_loop$:
;;     lda $ee
;;     sta (temp),y
;;     iny
;;     bne inner_loop$
;;     lda temp + 1
;;     inc
;;     sta temp + 1
;;     cmp #$40
;;     bne outer_loop$

;;     print_char 'x'
;;     rts


;; eeee_roots:
;;     panic 'W'
;; eeee_evacuate:
;;     panic 'X'
;; eeee_scavenge:
;;     panic 'Y'
;; eeee_code:
;;     panic 'Z'

;;     org ($eeee - 6)
;;     word eeee_roots, eeee_evacuate, eeee_scavenge
;;     jmp eeee_code
