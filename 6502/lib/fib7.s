
;;; CPS version of fib. Allocates stack frames on a heap. No GC yet. But soon!

put_hex_byte:
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

digits: .ascii "0123456789abcdef"


tiny_pause:
    pha
    lda #20
    jsr sleep_blocking
    pla
    rts

print_char: .macro CHAR
    pha
    lda #\CHAR
    jsr screen_putchar
    jsr print_screen
    jsr tiny_pause
    pla
.endmac

print_hex_word: .macro L
    lda #'['
    jsr screen_putchar
    lda \L + 1
    jsr put_hex_byte
    lda \L
    jsr put_hex_byte
    lda #']'
    jsr screen_putchar
    jsr print_screen
.endmac

load_frame_var0: .macro
    lda (fp)
.endmacro

load_frame_var: .macro N
    ldy #\N
    lda (fp),y
.endmacro

store_heap0: .macro
    sta (clo)
.endmacro

store_heap: .macro N ; must follow alloc
    ldy #\N
    sta (clo),y
.endmacro

copy_code_pointer_to_heap0: .macro code
    lda #<\code
    store_heap0
    lda #>\code
    store_heap 1
.endmacro

copy_code_pointer_to_local: .macro code, L
    lda #<\code
    sta \L
    lda #>\code
    sta \L + 1
.endmacro

copy_word: .macro source, dest
    lda \source
    sta \dest
    lda \source + 1
    sta \dest + 1
.endmacro

copy_word_from_frame0: .macro dest
    load_frame_var0
    sta \dest
    load_frame_var 1
    sta \dest + 1
.endmacro

copy_word_from_frame: .macro F, L
    load_frame_var \F
    sta \L
    load_frame_var \F+1
    sta \L + 1
.endmacro

copy_word_frame_to_heap: .macro F, H
    load_frame_var \F
    store_heap \H
    load_frame_var \F+1
    store_heap \H+1
.endmacro

copy_word_local_to_heap: .macro L, H
    lda \L
    store_heap \H
    lda \L+1
    store_heap \H+1
.endmacro

copy_byte_local_to_heap: .macro L, H
    lda \L
    store_heap \H
.endmacro

copy_word_local_to_heap: .macro L, H
    lda \L
    store_heap \H
    lda \L+1
    store_heap \H+1
.endmacro


;;; arguments/results to functions/continutaion are in ZP vars: 0,1,...
;;; code pointers and 16 bit values are little endian lo;hi

;;; Only have 16k available of my 32k SRAM :(
;;; heap grows upwards for GC-scavenge
;;; Two spaces (A/B). Each space is 7k
SPACE_A_START = $800
SPACE_A_END = $2400

SPACE_B_START = $2400
SPACE_B_END = $4000

stop:
    print_char '!'
spin:
    jmp spin

set_heap_space_a:
    print_char 'A'
    copy_code_pointer_to_local wipe_space_b, wipe_old_space
    copy_code_pointer_to_local set_heap_space_b, space_switcher
    lda #<SPACE_A_START
    sta hp
    lda #>SPACE_A_START
    sta hp + 1
    lda #>SPACE_A_END
    sta heap_end_page
    rts

set_heap_space_b:
    print_char 'B'
    copy_code_pointer_to_local wipe_space_a, wipe_old_space
    copy_code_pointer_to_local set_heap_space_a, space_switcher
    lda #<SPACE_B_START
    sta hp
    lda #>SPACE_B_START
    sta hp + 1
    lda #>SPACE_B_END
    sta heap_end_page
    rts

wipe_space_a:
    print_char 'a'
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
    print_char 'b'
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

;;; allocate [N(acc)] bytes in the heap; adjusting hp
alloc:
    sta n_bytes
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

alloc_again:
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
    print_char '*'
    jmp stop

;;; THE closure calling convention (TODO: just inline))
enter_fp:
    copy_word_from_frame0 cp ; TODO: avoid cp; using pha/pha/rts
    jmp (cp)


fib7_name:
    .string "7: CPS/Heap"
    .word fib7_name
fib7_entry:
    ;; N(acc) --> fib7 [N KL KH] where K is fib7_done []
    sta 0
    ;; initialize heap
    jsr wipe_space_a
    jsr wipe_space_b
    jsr set_heap_space_a
    ;; allocate final continuation -- TODO: no need for this to be heap allocated
    copy_word hp, clo
    lda #2
    jsr alloc
    ;; fill in closure
    copy_code_pointer_to_heap0 fib7_done
    ;; setup args
    copy_word clo, 1
    copy_code_pointer_to_local fib7_recurse_static_closure, fp
    jmp fib7_recurse


;;; RL RH -->
    .text "fib7_done"
    .word evacuate2, scavenge_nothing_of2
    .byte 2
fib7_done:
    ;; move final result to pre-allocated space on stack
    tsx
    lda 0
    sta $103,x
    lda 1
    sta $104,x
    rts ; return to original caller


fib7_recurse_static_closure:
    .word fib7_recurse

;;; [] N KL KH --> fib7 [N-1 JL JH] where J is fib7_cont1 [N KL KH]
    .text "fib7_recurse"
    .word evacuate_do_nothing, scavenge_error_if_called
    .byte 3
fib7_recurse:
    ;; access N
    lda 0
    sec
    cmp #2
    bcc fib7_base ; N<2 ?
    ;; allocate cont1
    copy_word hp, clo
    lda #5
    jsr alloc
    ;; fill in closure
    copy_code_pointer_to_heap0 fib7_cont1
    copy_byte_local_to_heap 0, 2
    copy_word_local_to_heap 1, 3
    ;; setup args
    lda 0 ; N
    sec
    sbc #1 ; N-1
    sta 0
    copy_word clo, 1
    copy_code_pointer_to_local fib7_recurse_static_closure, fp
    jmp fib7_recurse

;;; N KL KH --> K [N #0]
fib7_base:
    ;; move K into fp
    copy_word 1,fp
    ;; RL is N (already in 0)
    lda #0
    sta 1 ; setup RH
    jmp enter_fp

;;; TODO: switch N/K to standard order
;;; (doesn't matter while we have specific scavenge routines for each-shape)
;;; [. . N KL KH] AL AH -->  fib7 [N-2 JL JH] where J is fib7_cont2 [KL KH AL AH]
    .text "fib7_cont1"
    .word evacuate6, scavenge_at3_of6
    .byte 2
fib7_cont1:
    ;; allocate cont2
    copy_word hp, clo
    lda #6
    jsr alloc
    ;; fill in closure
    copy_code_pointer_to_heap0 fib7_cont2
    copy_word_frame_to_heap 3, 2 ; K
    copy_word_local_to_heap 0, 4 ; A
    ;; setup args
    load_frame_var 2 ; N
    sec
    sbc #2 ; N-2
    sta 0
    copy_word clo,1
    copy_code_pointer_to_local fib7_recurse_static_closure, fp
    jmp fib7_recurse

;;; [. . KL HL AL AH] BL BH (TmpL TmpH) --> RL RH (where R = A + B)
    .text "fib7_cont2"
    .word evacuate5, scavenge_at2_of5 ; evaluate counted in bytes; scavenge in words
    .byte 2
fib7_cont2:
    ;; 16-bit addition
    clc
    ;; macro for 16 bit addition?
    load_frame_var 4 ; AL
    adc 0 ; BL
    sta 0 ; RL
    load_frame_var 5 ; AH
    adc 1 ; BH
    sta 1 ; RH
    ;; return to caller
    copy_word_from_frame 2, 2 ; K
    copy_word 2, fp
    jmp enter_fp


gc_start:
    print_char 'G'
    jsr switch_space
    copy_word hp, lw
    jmp scavenge_roots ; TODO: dont jump. remove scavenge_roots label


scavenge_roots:
    ;; scavenge 'fp'
    copy_word fp, ev
    jsr evacuate
    copy_word clo, fp
    ;; TODO: scavenge all local var pairs which are heap-pointers.
    ;; We need to know which ones they are!
    ;; And it depends on our current context; as determined by the 'fp'
    ;; idea: have a function per closure...
    ;; TODO: dont just fall through, but jump to gc_loop


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

switch_space: ; TODO: move to just below call
    jmp (space_switcher)

gc_finished:
    print_char 'X'
    ;; TODO: report how many bytes collected, or just the hp will do!
    print_hex_word hp
    jmp (wipe_old_space) ; fill 28 pages with $ee
    rts


;;; double indirect jump to 'cp' (using 'temp')
jump_cp: .macro
    lda (cp)
    sta temp
    ldy #1
    lda (cp),y
    sta temp + 1
    jmp (temp)
.endmacro


get_code_pointer_offset_function: .macro HP, N
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
.endmacro

evacuate:
    ;; evacuate the closure at 'ev' (pointer into FROM-HEAP)
    print_char 'e'
    print_hex_word ev
    get_code_pointer_offset_function ev, 5
    jump_cp

scavenge:
    ;; scavenging the closure at 'lw' (pointer into TO-HEAP)
    print_char 's'
    ;print_hex_word lw
    get_code_pointer_offset_function lw, 3
    jump_cp

;;; ----------------------------------------------------------------------

shift_low_water: .macro N
    lda lw
    clc
    adc #\N
    sta lw
    bcc _done$
    inc lw + 1
_done$:
.endmacro

;;; Working from 'lw' pointing to an evacuated closure not yet scavenged.
;;; We will call evacuate on the cell (2 byte pointer) at offset-N
;;; By first setting 'ev'; calling evacuate; then assigning 'clo' back to the cell
scavange_cell_at : .macro N
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
.endmacro

    .text "scavenge_at2_of5"
scavenge_at2_of5:
    scavange_cell_at 2
    shift_low_water 5
    jmp gc_loop

    .text "scavenge_at3_of6"
scavenge_at3_of6:
    scavange_cell_at 3
    shift_low_water 6
    jmp gc_loop

    .text "scavenge_nothing_of2"
scavenge_nothing_of2:
    shift_low_water 2
    jmp gc_loop

scavenge_error_if_called:
    print_char 'Q'
spin$:
    jmp spin$

;;; ----------------------------------------------------------------------
;;; evacuate routines assume 'ev' to be set, and should in turn set 'clo'

;;; TODO: capture common evacuate code which uses a loop
;;; have special-case routines dispatch to that
;;; OR, have the common routine discover the N via offset from the code-pointer

evacuate_byte: .macro N
    ldy #\N
    lda (ev),y
    store_heap \N
.endmacro

    .text "evacuate2"
evacuate2:
    copy_word hp, clo
    lda #2
    jsr alloc
    evacuate_byte 0
    evacuate_byte 1
    rts

    .text "evacuate5"
evacuate5:
    copy_word hp, clo
    lda #5
    jsr alloc
    evacuate_byte 0
    evacuate_byte 1
    evacuate_byte 2
    evacuate_byte 3
    evacuate_byte 4
    rts

    .text "evacuate6"
evacuate6:
    copy_word hp, clo ; TODO: do this as part of alloc, not each alloc caller
    lda #6
    jsr alloc
    evacuate_byte 0
    evacuate_byte 1
    evacuate_byte 2
    evacuate_byte 3
    evacuate_byte 4
    evacuate_byte 5
    rts

    .text "evacuate_do_nothing"
evacuate_do_nothing:
    print_char '-'
    jsr stop ; never yet seen this called, so stop
    ;; TODO: set clo!
    rts
