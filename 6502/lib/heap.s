;;; PROVIDES: init_gc, alloc, evacuate, scavenge_cell_at, scavenge_done

PAGES_PER_SEMI_SPACE = 28 ; 7K
SPACE_B_END = $4000
SPACE_B_START = SPACE_B_END - ($100 * PAGES_PER_SEMI_SPACE)
SPACE_A_END = SPACE_B_START
SPACE_A_START = SPACE_A_END - ($100 * PAGES_PER_SEMI_SPACE)

;; PAGES_PER_SEMI_SPACE = 8
;; SPACE_A_START = $1000
;; SPACE_A_END = SPACE_A_START + ($100 * PAGES_PER_SEMI_SPACE)
;; SPACE_B_START = $2000
;; SPACE_B_END = SPACE_B_START + ($100 * PAGES_PER_SEMI_SPACE)

temp = 0
lw = 2
ev = 4
clo = 6
cp = 8

BASE = 10

fp = BASE
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
    copy16 g_heap_pointer, heap_start
    rts

;;; Macros for external use

impossible_scavenge_because_static: macro
    panic 'Scav'
endmacro

impossible_roots: macro
    panic 'Roots'
endmacro

gc_root_at: macro N
    copy16 \N, ev
    jsr gc.dispatch_evacuate
    copy16 ev, \N
endmacro

evacuate: macro N
    lda #\N
    pha
    jsr alloc_sub.again
    ply
    jsr gc.evacuate_sub
    copy16 clo, ev
    rts
endmacro

;;; Working from 'lw' pointing to an evacuated closure not yet scavenged.
;;; We will call evacuate on the cell (2 byte pointer) at offset-N
;;; By first setting 'ev'; calling evacuate; then assigning 'ev' back to the cell
scavenge_cell_at: macro N
    load16 lw, \N, ev
    ;; now 'ev is setup
    jsr gc.dispatch_evacuate
    ;; repoint the scavenged word to the relocated 'ev'
    save16 ev, lw,\N
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
    copy16 g_heap_pointer, clo
    lda n_bytes
    clc
    adc g_heap_pointer
    sta g_heap_pointer
    bcc .ok
    lda g_heap_pointer + 1
    inc
    cmp heap_end_page
    beq .heap_exhausted
    sta g_heap_pointer + 1
.ok:
    rts

.heap_exhausted:
    ;debug '{'
    jsr gc.start
    ;debug '}'
    lda n_bytes
    jmp .again ; TODO: inline this jump

;;; This inner alloc must succeed !
;;; i.e. we do the exhaustion check, and it must not fail.
;;; We call it from the evacuation routines
;;; And also, for the pending alloc which cause GC to be initiated.

.again: ; TODO: avoid code repetition w.r.t alloc
    pha
    copy16 g_heap_pointer, clo
    pla
    clc
    adc g_heap_pointer
    sta g_heap_pointer
    bcc .again_done
    lda g_heap_pointer + 1
    inc
    cmp heap_end_page
    beq .heap_exhausted_still
    sta g_heap_pointer + 1
.again_done:
    rts

.heap_exhausted_still:
    panic 'Heap Exhausted'

;;; macro for internal use
get_code_pointer_offset_function: macro P, N
    lda (\P)
    sec
    sbc #\N ; negative offset from code-pointer
    sta cp
    load8 \P,1, cp + 1
    bcs .\@
    dec cp + 1
.\@:
endmacro

;;; macro for internal use
;;; double indirect jump to 'cp' (using 'temp')
jump_cp: macro
    load16_0 cp, temp
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
    copy16 g_heap_pointer, heap_start
    copy16 g_heap_pointer, lw
    jsr .evacuate_roots
    ;; TODO: evacuate 'fp' like any other root; caller must identify it ?
    copy16 fp, ev
    jsr .dispatch_evacuate
    copy16 ev, fp
    jmp .scavenge_loop

.switch_space:
    jmp (space_switcher)

;;; This inner alloc must succeed !
;;; i.e. we do the exhaustion check, and it must not fail.
;;; We call it from the evacuation routines
;;; And also, for the pending alloc which cause GC to be initiated.

.set_heap_space_a:
    store16i .set_heap_space_b, space_switcher
    lda #<SPACE_A_START
    sta g_heap_pointer
    lda #>SPACE_A_START
    sta g_heap_pointer + 1
    lda #>SPACE_A_END
    sta heap_end_page
    rts

.set_heap_space_b:
    store16i .set_heap_space_a, space_switcher
    lda #<SPACE_B_START
    sta g_heap_pointer
    lda #>SPACE_B_START
    sta g_heap_pointer + 1
    lda #>SPACE_B_END
    sta heap_end_page
    rts

;;; keep scavenging until 'lw' catches up with 'hp'
;;; scavenge routines jump back here when thet are done
.scavenge_loop:
    lda lw
    cmp g_heap_pointer
    beq .scavenge_loop_cmp_second_byte
    jmp .gc_scavenge
.scavenge_loop_cmp_second_byte:
    lda lw + 1
    cmp g_heap_pointer + 1
    beq gc.finished
    jmp .gc_scavenge
.finished:
    jsr .debug_end_gc
    rts

.debug_start_gc: ; TODO: inline/remove this
    rts

.debug_end_gc: ; TODO: inline
    ;; SWITCH TO GC SCREEN
    ldx g_selected_screen
    phx ; save caller's selected screen
    lda gc_screen
    sta g_selected_screen
    newline
    print_string 'GC:'
    increment16 gc_count
    print_decimal_word gc_count
    newline
    print_string 'live:'
    sub16 g_heap_pointer, heap_start, temp
    print_decimal_word temp
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
