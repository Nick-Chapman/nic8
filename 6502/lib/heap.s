;;; PROVIDES: init_heap, heap_alloc,
;;; gc_root_at, evacuate, scavenge_cell_at, scavenge_done

PAGES_PER_SEMI_SPACE = 28 ; 7K
SPACE_B_END = $4000
SPACE_B_START = SPACE_B_END - ($100 * PAGES_PER_SEMI_SPACE)
SPACE_A_END = SPACE_B_START
SPACE_A_START = SPACE_A_END - ($100 * PAGES_PER_SEMI_SPACE)

;; PAGES_PER_SEMI_SPACE = 4 ; 1K
;; SPACE_A_START = $1000
;; SPACE_A_END = SPACE_A_START + ($100 * PAGES_PER_SEMI_SPACE)
;; SPACE_B_START = $2000
;; SPACE_B_END = SPACE_B_START + ($100 * PAGES_PER_SEMI_SPACE)

temp = 0
lw = 2  ; low-water mark. pointer into (TO-HEAP).
ev = 4  ; points to closure being evacuated (FROM-HEAP)
clo = 6 ; points to closure just allocated  (TO-HEAP)
cp = 8  ; code-pointer
;fp = $a  ; frame-pointer

;;; Client entry points

init_heap: macro Screen_Number
    pha
    lda #\Screen_Number
    jsr heap.init
    pla
endmacro

find_roots_from: macro P
    jmp .skip\@
.dispatch_roots\@:
    jump_indirect_offset \P, 6
.skip\@:
    jsr .dispatch_roots\@
    gc_root_at \P
endmacro

;;; allocate #bytes in the heap

heap_alloc: macro N ; TODO: preserve acc?
    lda #\N
    jsr heap.allocate_may_gc
endmacro

;;; Roots...

impossible_roots: macro
    panic 'Roots'
endmacro

gc_root_at: macro N
    copy16 \N, ev
    jsr heap.dispatch_evacuate
    copy16 ev, \N
endmacro

gc_root_at_x: macro N
    copyFrom16_x \N, ev
    jsr heap.dispatch_evacuate
    copyTo16_x ev, \N
endmacro

;;; Evacuate...

evacuate: macro N
    lda #\N
    jmp heap.evacuateN
endmacro

;;; Scavenge...

scavenge_done: macro N
    lda #\N
    jmp heap.scavenge_done
endmacro

impossible_scavenge_because_static: macro
    panic 'Scav'
endmacro

scavenge_cell_at: macro N
    load16 lw, \N, ev
    jsr heap.dispatch_evacuate
    save16 ev, lw,\N
endmacro


heap: ; marker for internal routines


.init: ; screen number for GC report passed in acc
    sta gc_screen
    lda #0
    sta gc_count
    sta gc_count + 1
    jsr .set_heap_space_a
    copy16 g_heap_pointer, heap_start
    jsr .report_collection ; to get info on screen prior to first collection
    rts

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

.switch_space:
    jmp (space_switcher)


alloc_orelse: macro FAIL ; #bytes in acc
    ;; if successful, the allocated space can be reference by 'clo'
    pha
    copy16 g_heap_pointer, clo
    pla
    clc
    adc g_heap_pointer
    sta g_heap_pointer
    bcc .ok\@
    lda g_heap_pointer + 1
    inc
    cmp heap_end_page
    beq \FAIL
    sta g_heap_pointer + 1
.ok\@:
    endmacro

.allocate_may_gc: ; #bytes in acc
    pha
    alloc_orelse .exhausted_trigger_collection
    pla
    rts ; return to caller of heap_alloc

.exhausted_trigger_collection:
    jsr .run_collection
    increment16 gc_count
    jsr .report_collection
    pla
    alloc_orelse .exhausted_still_after_collection
    rts ; return to caller of heap_alloc

.exhausted_still_after_collection:
    panic 'Heap'


.run_collection:
    jsr .switch_space
    copy16 g_heap_pointer, heap_start
    copy16 g_heap_pointer, lw
    jsr find_roots

    ;; scavenge routines jump back here when they are done
.scavenge_loop:
    ;; while 'lw' has not caught up with 'hp', then scavenge
    lda lw
    cmp g_heap_pointer
    bne .dispatch_scavenge
    lda lw + 1
    cmp g_heap_pointer + 1
    bne .dispatch_scavenge
    rts ; collection is done

.scavenge_done: ; #bytes in acc
    clc
    adc lw
    sta lw
    bcc .scavenge_loop
    inc lw + 1
    jmp .scavenge_loop

jump_indirect_offset: macro P, N
    lda (\P)
    sec
    sbc #\N ; negative offset from code-pointer
    sta cp
    load8 \P,1, cp + 1
    bcs .skip\@
    dec cp + 1
.skip\@:
    load16_0 cp, temp
    cmp #$80
    bcc .oor\@
    jmp (temp)
.oor\@:
    panic 'JIO' ; out of rom
endmacro


.dispatch_evacuate:
    jump_indirect_offset ev, 4

.dispatch_scavenge:
    jump_indirect_offset lw, 2

.evacuateN: ; #bytes in acc
    pha
    alloc_orelse .unexpected_exhaustion_during_collection
    ply
.ev_loop:
    dey
    php
    lda (ev),y ; copy from old closure in FROM-space
    sta (clo),y ; into newly allocated closure in TO-space
    plp
    bne .ev_loop
    ;; set fowarding-pointer (inside a broken-heart) to preserve sharing
    save16i_0 broken_heart.code, ev
    save16 clo, ev,2
    ;; update ev to the evacuated closure
    copy16 clo, ev
    rts

.unexpected_exhaustion_during_collection:
    panic 'Evac'

.report_collection:
    ;; SWITCH TO GC SCREEN
    lda g_selected_screen
    pha ; save caller's selected screen
    lda gc_screen
    sta g_selected_screen
    newline
    print_string 'GC:'
    print_decimal_word gc_count
    newline
    print_string 'live:'
    sub16 g_heap_pointer, heap_start, temp
    print_decimal_word temp
    ;; RESTORE CALLER SCREEN
    pla ; restore caller's selected screen
    sta g_selected_screen
    rts

broken_heart:
.roots:
    panic "heart:roots"
.evac:
    ;; update ev to the forwarding-pointer contained in the broken-heart
    load16 ev,2, temp
    copy16 temp, ev
    rts
.scav:
    panic "heart:scav"
    word .roots, .evac, .scav
.code:
    panic "heart:code"
