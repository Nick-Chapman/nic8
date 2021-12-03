;;; PROVIDES: init_gc, alloc, evacuate, scavenge_cell_at, scavenge_done

;;; 2x 7k heap space...
SPACE_A_START = $800
SPACE_A_END = $2400
SPACE_B_START = $2400
SPACE_B_END = $4000


;;; Client entry points

init_gc:
    lda #0
    sta gc_debug
    sta gc_count
    sta gc_count + 1
    jsr gc.set_heap_space_a
    copy_word hp, heap_start
    rts

;;; Macros for external use

no_evacuate_because_static: macro
    copy_word ev, clo
    rts
endmacro

impossible_scavenge_because_static: macro
    panic 'S'
endmacro

impossible_roots: macro
    panic 'R'
endmacro

gc_root_at: macro N
    copy_word \N, ev
    jsr gc.dispatch_evacuate
    copy_word clo, \N
endmacro

evacuate: macro N
    lda #\N
    pha
    jsr alloc.again
    ply
    jsr gc.evacuate_sub
    rts
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
    jsr gc.dispatch_evacuate
    ;; repoint the scavenged word to the relocated closure
    lda clo
    ldy #\N
    sta (lw),y
    lda clo + 1
    ldy #\N + 1
    sta (lw),y
endmacro

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

;;; allocate [N(acc)] bytes in the heap; adjusting hp
alloc:
    sta n_bytes ; TODO: put this on stack to avoid global

    ;; HACK trigger flush screen from allocation
    jsr screen_flush_when_time

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
    bcs .\@
    dec cp + 1
.\@:
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


gc: ; private namespace marker


.evacuate_roots:
    get_code_pointer_offset_function fp, 6
    jump_cp

.gc_scavenge:
    ;; scavenging the closure at 'lw' (pointer into TO-HEAP)
    get_code_pointer_offset_function lw, 2
    jump_cp

.dispatch_evacuate:
    ;; evacuate the closure at 'ev' (pointer into FROM-HEAP)
    get_code_pointer_offset_function ev, 4
    ;; TODO: after evacuation, we ought to set a fowarding pointer to preserve sharing
    ;; But I don't think sharing is ever possible in my examples so far
    jump_cp

.start:
    jsr .debug_start_gc
    jsr .switch_space
    copy_word hp, heap_start
    copy_word hp, lw
    jsr .evacuate_roots
    ;; evacuate 'fp'... ; TODO: fp=0, and treat like any other root
    copy_word fp, ev
    jsr .dispatch_evacuate
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
    jmp .gc_scavenge
.scavenge_loop_cmp_second_byte:
    lda lw + 1
    cmp hp + 1
    beq gc.finished
    jmp .gc_scavenge
.finished:
    jsr .debug_end_gc
    rts

.debug_start_gc:
    ;; if gc_debug is non-zero, then it contains the screen# to print to
    lda gc_debug
    beq .return1
    ldx g_selected_screen
    phx
    sta g_selected_screen
    jsr screen_return_home
    print_char 'G'
    print_char 'C'
    print_char ':'
    inc16_var gc_count
    print_decimal_word gc_count
    plx
    stx g_selected_screen
.return1
    rts

.debug_end_gc:
    lda gc_debug
    beq .return2
    ldx g_selected_screen
    phx
    sta g_selected_screen
    jsr screen_newline
    print_char 'l'
    print_char 'i'
    print_char 'v'
    print_char 'e'
    print_char ':'
    sub16 hp, heap_start, temp
    print_decimal_word temp
    print_char ' '
    print_char ' '
    print_char ' '
    plx
    stx g_selected_screen
.return2
    rts

.evacuate_sub: ; N passed in Y; N>=1
.ev_loop:
    dey
    php
    lda (ev),y ; copy from old closure in FROM-space
    sta (clo),y ; into newly allocated closure in TO-space
    plp
    bne .ev_loop
    rts
