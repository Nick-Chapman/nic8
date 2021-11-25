
;;; TODO: move this panic macro to a new file; take a string, not just a char

panic_stop:
    pha
    lda #'!'
    jsr screen_putchar
    pla
    jsr screen_putchar
    jsr print_screen
panic_spin:
    jmp panic_spin

panic: .macro CHAR
    lda #\CHAR
    jmp panic_stop
.endmac

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

digits: .ascii "0123456789abcdef"


;; tiny_pause:
;;     pha
;;     lda #3
;;     jsr sleep_blocking
;;     pla
;;     rts

print_char: .macro CHAR
    pha
    lda #\CHAR
    jsr screen_putchar
    jsr print_screen
    ;jsr tiny_pause
    pla
.endmac

print_hex_word: .macro L
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
.endmac


print_decimal_word: .macro L
    pha
    phx
    lda \L
    ldx \L + 1
    jsr decimal_put_word
    jsr print_screen
    plx
    pla
.endmacro

copy16_literal_to_var: .macro lit, V
    lda #<\lit
    sta \V
    lda #>\lit
    sta \V + 1
.endmacro

sub16 : .macro A, B, RES
    sec
    lda \A
    sbc \B
    sta \RES
    lda \A + 1
    sbc \B + 1
    sta \RES + 1
.endmacro

inc16_var: .macro V
    ;; can I use inc? does this set the carry flag. No1
    ;; but that's ok because it does set zero!
    inc \V
    bne _done$
    inc \V + 1
_done$:
.endmacro

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

copy_code_pointer_to_local: .macro code, L ; TODO: same as copy16_literal_to_var
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

;;; 2x 7k heap space...
SPACE_A_START = $800
SPACE_A_END = $2400
SPACE_B_START = $2400
SPACE_B_END = $4000


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



fib7_name:
    .string "7: CPS/Heap"
    .word fib7_name
fib7_entry:
    ;; N(acc) --> fib7 [N KL KH] where K is fib7_done []
    sta 0

    ;; ;; How deep is the page-1 stack?
    ;; lda #'<'
    ;; jsr screen_putchar
    ;; tsx
    ;; txa
    ;; jsr put_hex_byte
    ;; lda #'>'
    ;; jsr screen_putchar
    ;; jsr print_screen

    copy16_literal_to_var 0, gc_count ; TODO: 0

    ;; initialize heap
    jsr wipe_space_a
    jsr wipe_space_b
    jsr set_heap_space_a
    copy_word hp, heap_start
    ;; allocate final continuation -- TODO: no need for this to be heap allocated
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
    .word rootargs_impossible, evacuate2, scavenge_nothing_of2
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

;;; fib7 is a top level function
;;; and so we have a static closure
;;; and so, we do nothing if asked to evacuate - just return the same static clo
;;; and because we didn't evacuate (we never were in the heap, and we still aren't)
;;; it is impossible that this static closure will be subject to scavenging
;;; (i.e. that process where we walk along the new-heap
;;; using the low-water 'lw' pointer.. until catches up with 'hp')
;;; [] N KL KH --> fib7 [N-1 JL JH] where J is fib7_cont1 [N KL KH]
    .text "fib7_recurse"
    .word rootargs_at1, evacuate_do_nothing, scavenge_impossible
    .byte 3 ; TODO: This 'arg-count' byte is used nowhere!
fib7_recurse:
    ;; access N
    lda 0
    sec
    cmp #2
    bcc fib7_base ; N<2 ?
    ;; allocate cont1
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
    ;print_char "."
    ;; move K into fp
    copy_word 1,fp
    ;; RL is N (already in 0)
    lda #0
    sta 1 ; setup RH
    copy_word_from_frame0 cp ; TODO: avoid cp; using pha/pha/rts
    ;print_hex_word 0
    jmp (cp) ; enter_fp

;;; TODO: switch N/K to standard order
;;; (doesn't matter while we have specific scavenge routines for each-shape)
;;; [. . N KL KH] AL AH -->  fib7 [N-2 JL JH] where J is fib7_cont2 [KL KH AL AH]
    .text "fib7_cont1"
    .word rootargs_none, evacuate5, scavenge_at3_of5
    .byte 2
fib7_cont1:
    ;; allocate cont2
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

;;; We say rootargs_impossible here rather than rootargs_none
;;; because GC should never be initiated whilst the closure is set as 'fp'
;;; because no allocation occurs here!
;;;
;;; [. . KL HL AL AH] BL BH (TmpL TmpH) --> RL RH (where R = A + B)
    .text "fib7_cont2"
    .word rootargs_impossible, evacuate6, scavenge_at2_of6
    .byte 2
fib7_cont2:
    ;; jsr screen_newline
    ;; print_hex_word 0
    ;; print_char '+'
    ;; copy_word_from_frame 4, temp
    ;; print_hex_word temp
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
    copy_word_from_frame0 cp ; TODO: avoid cp; using pha/pha/rts
    ;print_char '='
    ;print_hex_word 0
    jmp (cp) ; enter_fp


;;; ----------------------------------------------------------------------
;;; GC stuff below here...

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

evacuate_roots:
    get_code_pointer_offset_function fp, 7
    jump_cp


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

;;; ----------------------------------------------------------------------
;;; scavenge rootargs... (those which are known to be pointers)

rootargs_none:
    rts

rootargs_impossible:
    panic 'R'

rootargs_at1:
    copy_word 1, ev
    jsr evacuate
    copy_word clo, 1
    rts

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

    .text "scavenge_at2_of6"
scavenge_at2_of6:
    scavange_cell_at 2
    shift_low_water 6
    jmp gc_loop

    .text "scavenge_at3_of5"
scavenge_at3_of5:
    scavange_cell_at 3
    shift_low_water 5
    jmp gc_loop

    .text "scavenge_nothing_of2"
scavenge_nothing_of2:
    shift_low_water 2
    jmp gc_loop

scavenge_impossible:
    panic 'S'

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
    lda #2
    jsr alloc_again
    evacuate_byte 0
    evacuate_byte 1
    rts

    .text "evacuate5"
evacuate5:
    lda #5
    jsr alloc_again
    evacuate_byte 0
    evacuate_byte 1
    evacuate_byte 2
    evacuate_byte 3
    evacuate_byte 4
    rts

    .text "evacuate6"
evacuate6:
    lda #6
    jsr alloc_again
    evacuate_byte 0
    evacuate_byte 1
    evacuate_byte 2
    evacuate_byte 3
    evacuate_byte 4
    evacuate_byte 5
    rts

    .text "evacuate_do_nothing"
evacuate_do_nothing:
    copy_word ev, clo
    rts
