
;;; 2x 7k heap space...
SPACE_A_START = $800
SPACE_A_END = $2400
SPACE_B_START = $2400
SPACE_B_END = $4000


;;; Client entry points

init_gc:
    jsr gc.set_heap_space_a
    copy_word hp, heap_start
    rts

;;; allocate [N(acc)] bytes in the heap; adjusting hp
alloc:
    sta n_bytes ; TODO: put this on stack to avoid global
    copy_word hp, clo
    lda n_bytes
    clc
    adc hp
    sta hp
    bcc .ok
    lda hp + 1
    inc
    cmp heap_end_page
    beq .heap_exhausted
    sta hp + 1
.ok:
    rts

.heap_exhausted:
    jsr gc.start
    lda n_bytes
    jmp .again

;;; This inner alloc must succeed !
;;; i.e. we do the exhaustion check, and it must not fail.
;;; We call it from the evacuation routines
;;; And also, for the pending alloc which cause GC to be initiated.

.again: ; TODO: avoid code repetition w.r.t alloc
    pha
    copy_word hp, clo
    pla
    clc
    adc hp
    sta hp
    bcc .again_done
    lda hp + 1
    inc
    cmp heap_end_page
    beq .heap_exhausted_still
    sta hp + 1
.again_done:
    rts

.heap_exhausted_still:
    panic 'H'


;;; macro for internal use
get_code_pointer_offset_function: macro HP, N
    lda (\HP)
    sec
    sbc #\N ; negative offset from code-pointer
    sta cp
    ldy #1
    lda (\HP),y
    sta cp + 1
    bcs .get_cp_offset_done
    dec cp + 1
.get_cp_offset_done:
endmacro

;;; macro for internal use
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
    get_code_pointer_offset_function fp, 6
    jump_cp


gc_scavenge:
    ;; scavenging the closure at 'lw' (pointer into TO-HEAP)
    get_code_pointer_offset_function lw, 2
    jump_cp

gc_evacuate:
    ;; evacuate the closure at 'ev' (pointer into FROM-HEAP)
    get_code_pointer_offset_function ev, 4
    ;; TODO: after evacuation, we ought to set a fowarding pointer to preserve sharing
    ;; But I don't think sharing is ever possible in my examples so far
    jump_cp

gc: ; private namespace marker

.start:
    print_char '{'
    inc16_var gc_count
    print_decimal_word gc_count
    jsr .switch_space
    copy_word hp, heap_start
    copy_word hp, lw
    jsr evacuate_roots
    ;; evacuate 'fp'... ; TODO: fp=0, and treat like any other root
    copy_word fp, ev
    jsr gc_evacuate
    copy_word clo, fp
    jmp .scavenge_loop

.switch_space:
    jmp (space_switcher)

;;; This inner alloc must succeed !
;;; i.e. we do the exhaustion check, and it must not fail.
;;; We call it from the evacuation routines
;;; And also, for the pending alloc which cause GC to be initiated.

.set_heap_space_a:
    copy_code_pointer_to_local .set_heap_space_b, space_switcher
    lda #<SPACE_A_START
    sta hp
    lda #>SPACE_A_START
    sta hp + 1
    lda #>SPACE_A_END
    sta heap_end_page
    rts

.set_heap_space_b:
    copy_code_pointer_to_local .set_heap_space_a, space_switcher
    lda #<SPACE_B_START
    sta hp
    lda #>SPACE_B_START
    sta hp + 1
    lda #>SPACE_B_END
    sta heap_end_page
    rts

;;; keep scavenging until 'lw' catches up with 'hp'
;;; scavenge routines jump back here when thet are done
.scavenge_loop:
    lda lw
    cmp hp
    beq .scavenge_loop_cmp_second_byte
    jmp gc_scavenge
.scavenge_loop_cmp_second_byte:
    lda lw + 1
    cmp hp + 1
    beq gc.finished
    jmp gc_scavenge

.finished:
    print_char ':'
    sub16 hp, heap_start, temp
    print_decimal_word temp
    print_char '}'
    rts

;;; Macros for external use: TODO: better as subs?

scavenge_done: macro N
    lda lw
    clc
    adc #\N
    sta lw
    bcc .scavenge_done_done
    inc lw + 1
.scavenge_done_done:
    jmp gc.scavenge_loop
endmacro

;;; Working from 'lw' pointing to an evacuated closure not yet scavenged.
;;; We will call evacuate on the cell (2 byte pointer) at offset-N
;;; By first setting 'ev'; calling evacuate; then assigning 'clo' back to the cell
scavenge_cell_at: macro N
    ldy #\N ; TODO: use word macros to do copy: into ev; back from clo
    lda (lw),y
    sta ev
    ldy #\N + 1
    lda (lw),y
    sta ev + 1
    ;; now 'ev is setup
    jsr gc_evacuate
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
