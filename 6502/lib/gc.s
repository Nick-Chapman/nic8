
;;; 2x 7k heap space...
SPACE_A_START = $800
SPACE_A_END = $2400
SPACE_B_START = $2400
SPACE_B_END = $4000

;;; allocate [N(acc)] bytes in the heap; adjusting hp
alloc:
    sta n_bytes
    copy_word hp, clo
    lda n_bytes
    clc
    adc hp
    sta hp
    bcc _$
    lda hp + 1
    inc
    cmp heap_end_page
    beq heap_exhausted
    sta hp + 1
_$:
    rts

heap_exhausted:
    jsr gc_start
    lda n_bytes
    jmp alloc_again

;;; This inner alloc must succeed !
;;; i.e. we do the exhaustion check, and it must not fail.
;;; We call it from the evacuation routines
;;; And also, for the pending alloc which cause GC to be initiated.
alloc_again:
    pha
    copy_word hp, clo
    pla
    clc
    adc hp
    sta hp
    bcc _$
    lda hp + 1
    inc
    cmp heap_end_page
    beq heap_exhausted_still
    sta hp + 1
_$:
    rts

heap_exhausted_still:
    panic 'H'

get_code_pointer_offset_function: macro HP, N
    lda (\HP)
    sec
    sbc #\N ; negative offset from code-pointer
    sta cp
    ldy #1
    lda (\HP),y
    sta cp + 1
    bcs _done$
    dec cp + 1
_done$:
endmacro

;;; double indirect jump to 'cp' (using 'temp')
jump_cp: macro
    lda (cp)
    sta temp
    ldy #1
    lda (cp),y
    sta temp + 1
    jmp (temp)
endmacro

evacuate_roots:
    get_code_pointer_offset_function fp, 7
    jump_cp

scavenge:
    ;; scavenging the closure at 'lw' (pointer into TO-HEAP)
    ;print_char 's'
    ;print_hex_word lw
    get_code_pointer_offset_function lw, 3
    jump_cp

evacuate:
    ;; evacuate the closure at 'ev' (pointer into FROM-HEAP)
    ;print_char 'e'
    ;print_hex_word ev
    get_code_pointer_offset_function ev, 5
    ;; TODO: after evacuation, we ought to set a fowarding pointer
    ;; to preserve pointer sharing
    ;; But I don't think sharing is ever possible in my examples so far
    jump_cp

gc_start:
    print_char '{' ;G

    inc16_var gc_count
    print_decimal_word gc_count

    ;print_char '%' ;G
    jsr switch_space
    copy_word hp, heap_start
    copy_word hp, lw

    ;; TODO: should roots be evacuated by 'fp' ?
    jsr evacuate_roots

    ;; evacuate 'fp'...
    copy_word fp, ev
    jsr evacuate
    copy_word clo, fp

    ;panic 'q'

    jmp gc_loop


switch_space:
    jmp (space_switcher)

set_heap_space_a:
    ;print_char 'A'
    ;copy_code_pointer_to_local wipe_space_b, wipe_old_space
    copy_code_pointer_to_local set_heap_space_b, space_switcher
    lda #<SPACE_A_START
    sta hp
    lda #>SPACE_A_START
    sta hp + 1
    lda #>SPACE_A_END
    sta heap_end_page
    rts

set_heap_space_b:
    ;print_char 'B'
    ;copy_code_pointer_to_local wipe_space_a, wipe_old_space
    copy_code_pointer_to_local set_heap_space_a, space_switcher
    lda #<SPACE_B_START
    sta hp
    lda #>SPACE_B_START
    sta hp + 1
    lda #>SPACE_B_END
    sta heap_end_page
    rts


;;; keep scavenging until 'lw' catches up with 'hp'
;;; scavenge routines jump back here when thet are done
gc_loop:
    lda lw
    cmp hp
    beq cmp_byte2$
    jmp scavenge
cmp_byte2$:
    lda lw + 1
    cmp hp + 1
    beq gc_finished
    jmp scavenge

gc_finished:
    print_char ':'
    ;print_hex_word hp
    ;print_char '-'
    ;print_hex_word heap_start
    ;print_char '='
    sub16 hp, heap_start, temp
    print_decimal_word temp
    print_char '}'
    ;jmp (wipe_old_space) ; fill 28 pages with $ee
    rts

shift_low_water: macro N
    lda lw
    clc
    adc #\N
    sta lw
    bcc _done$
    inc lw + 1
_done$:
endmacro


;;; Working from 'lw' pointing to an evacuated closure not yet scavenged.
;;; We will call evacuate on the cell (2 byte pointer) at offset-N
;;; By first setting 'ev'; calling evacuate; then assigning 'clo' back to the cell
scavange_cell_at : macro N
    ldy #\N
    lda (lw),y
    sta ev
    ldy #\N + 1
    lda (lw),y
    sta ev + 1
    ;; now 'ev is setup
    jsr evacuate
    ;; repoint the scavenged word to the relocated closure
    lda clo
    ldy #\N
    sta (lw),y
    lda clo + 1
    ldy #\N + 1
    sta (lw),y
endmacro

evacuate_byte: macro N
    ldy #\N
    lda (ev),y
    store_heap \N
endmacro
