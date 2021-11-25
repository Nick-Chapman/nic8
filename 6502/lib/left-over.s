
;;; CPS version of fib. Allocates stack frames on a heap. No GC yet. But soon!

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


;; tiny_pause:
;;     pha
;;     lda #3
;;     jsr sleep_blocking
;;     pla
;;     rts

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



;;; arguments/results to functions/continutaion are in ZP vars: 0,1,...
;;; code pointers and 16 bit values are little endian lo;hi

;;; Only have 16k available of my 32k SRAM :(
;;; heap grows upwards for GC-scavenge
;;; Two spaces (A/B). Each space is 7k

;;; 2x 256k heap space...
;; SPACE_A_START = $1000
;; SPACE_A_END = $1100
;; SPACE_B_START = $2000
;; SPACE_B_END = $2100

;;; 2x 1k heap space...
;; SPACE_A_START = $1000
;; SPACE_A_END = $1400
;; SPACE_B_START = $2000
;; SPACE_B_END = $2400



wipe_space_a:
    ;print_char 'a'
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
    ;print_char 'b'
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



;;; ----------------------------------------------------------------------
;;; GC stuff below here...




;;; ----------------------------------------------------------------------
;;; scavenge rootargs... (those which are known to be pointers)


;;; ----------------------------------------------------------------------

;;; ----------------------------------------------------------------------
;;; evacuate routines assume 'ev' to be set, and should in turn set 'clo'

;;; TODO: capture common evacuate code which uses a loop
;;; have special-case routines dispatch to that
;;; OR, have the common routine discover the N via offset from the code-pointer

