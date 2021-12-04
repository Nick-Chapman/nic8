;;; PROVIDES: init_gc, alloc, evacuate, scavenge_cell_at, scavenge_done

;;; 2x 7k heap space...
SPACE_A_START = $800
SPACE_A_END = $2400
SPACE_B_START = $2400
SPACE_B_END = $4000

BASE = 10
arg2 = BASE + 2
arg3 = BASE + 3
arg4 = BASE + 4
arg5 = BASE + 5
arg6 = BASE + 6

;;; Client entry points

init_gc: macro Screen_Number
    pha
    lda #\Screen_Number
    jsr internal_init_gc_sub
    pla
endmacro

internal_init_gc_sub: ; screen number for GC debug passed in acc
    sta gc_screen
    lda #0
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
    panic 'Scav'
endmacro

impossible_roots: macro
    panic 'Roots'
endmacro

gc_root_at: macro N
    copy_word \N, ev
    jsr gc.dispatch_evacuate
    copy_word clo, \N
endmacro

evacuate: macro N
    ;debug 'e'
    lda #\N
    pha
    jsr alloc_sub.again ; TODO, hmm
    ply
    jsr gc.evacuate_sub
    rts
endmacro

;;; Working from 'lw' pointing to an evacuated closure not yet scavenged.
;;; We will call evacuate on the cell (2 byte pointer) at offset-N
;;; By first setting 'ev'; calling evacuate; then assigning 'clo' back to the cell
scavenge_cell_at: macro N
    ;debug 's'
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

heap_alloc: macro C, N
    lda #\N
    jsr alloc_sub
endmacro


alloc_sub:
    sta n_bytes ; TODO: put this on stack to avoid global

    ;; Something needs to do a regular flush...
    ;; Really we should do it on the JUMP/ENTER
    ;; But for now...
    ;; HACK trigger flush screen from allocation
    jsr screen_flush_when_time ; needed for forever-fib

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
    ;debug 'X'
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
    panic 'Heap Exhausted'


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


;; show_closure_tag: macro HP
;;     lda (\HP)
;;     sec
;;     sbc #7 ; negative offset from code-pointer -- SHARE
;;     sta cp
;;     ldy #1
;;     lda (\HP),y
;;     sta cp + 1
;;     bcs .\@
;;     dec cp + 1
;; .\@:
;;     lda (cp)
;;     jsr screen_putchar
;;     screen_flush_selected
;; endmacro


gc: ; private namespace marker


.evacuate_roots:
    ;debug 'R'
    ;show_closure_tag fp
    get_code_pointer_offset_function fp, 6
    jump_cp

.gc_scavenge:
    ;debug 'S'
    ;show_closure_tag lw
    ;; scavenging the closure at 'lw' (pointer into TO-HEAP)
    get_code_pointer_offset_function lw, 2
    jump_cp

.dispatch_evacuate:
    ;debug 'E'
    ;show_closure_tag ev
    ;; evacuate the closure at 'ev' (pointer into FROM-HEAP)
    get_code_pointer_offset_function ev, 4
    ;; TODO: after evacuation, we ought to set a fowarding pointer to preserve sharing
    ;; But I don't think sharing is ever possible in my examples so far
    jump_cp

.start:
    ;lda gc_screen
    ;; SWITCH TO GC SCREEN
    ;; ldx g_selected_screen
    ;; phx ; save caller's selected screen
    ;; sta g_selected_screen ; set screen for GC debug
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
    ;debug 'L'
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
    ;; RESTORE CALLER SCREEN
    ;; plx ; restore caller's selected screen
    ;; stx g_selected_screen
    rts

.debug_start_gc:
    ;; newline
    ;; newline
    ;; jsr screen_return_home
    ;; print_string 'GC{'
    rts

.debug_end_gc:
    ;; print_char '}'
    ;; newline

    ;; SWITCH TO GC SCREEN
    ldx g_selected_screen
    phx ; save caller's selected screen
    lda gc_screen
    sta g_selected_screen ; set screen for GC debug

    newline
    print_string 'GC:'
    inc16_var gc_count
    print_decimal_word gc_count
    newline
    print_string 'live:'
    sub16 hp, heap_start, temp
    print_decimal_word temp
    ;print_string '  ' ; blank digits

    ;; RESTORE CALLER SCREEN
    plx ; restore caller's selected screen
    stx g_selected_screen

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
