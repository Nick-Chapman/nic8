;;; Example program which runs fib forver on increasing values of N.
;;; Entire program written in the CPS/GC style, with no outer harness.
;;; Purpose is to motivate a clean interface to the GC alloc.
;;; And to understand exactly what invariants are placed on user code.

    org $fffc
    word reset_main
    word ticks_irq

    org $8000

;;; bytes
heap_end_page = $f8
n_bytes = $f9
g_ticks = $51
gc_debug = $4f

;;; words
hp = $f0
fp = $f2
cp = $f4
clo = $f6
ev = $77
lw = $88
space_switcher = $fa
temp = $fc
gc_count = $8a
heap_start = $8c
g_divisor = $72 ; decimal.s
g_mod10 = $74 ; decimal.s
g_screen_pointer = $53

;;; buffers
g_screen = $200 ; 32 bytes

    include via.s
    include ticks.s
    include lcd.s
    include screen.s
    include decimal.s
    include print.s
    include panic.s
    include macs.s
    include gc.s

reset_main:
    ldx #$ff
    txs
    jsr init_via
    jsr init_ticks
    jsr init_lcd
    jsr lcd_clear_display
    jsr init_screen
    jsr init_gc
    jmp start_example


start_example:
    lda #0
    sta 0
    copy_code_pointer_to_local fib_iter.static_closure, fp
    jmp fib_iter.code


;; [] I --> print("$I-"); fib [I KL KH] where K is fib_iter2 [. . I]
fib_iter:
    word .roots, .evac, .scav
.code:
    lda $0
    ;; allocate & fill in closure
    lda #3
    jsr alloc
    copy_code_pointer_to_heap0 fib_iter2.code
    copy_byte_local_to_heap 0, 2 ; I
    ;; setup args & fp
    ;; 0 already contains I
    copy_word clo, 1 ; K
    copy_code_pointer_to_local fib_recurse.static_closure, fp
    jmp fib_recurse.code
.roots:
    rts ; no roots
.evac:
    copy_word ev, clo ; TODO: change evac inteface to be ev->ev, then this line wont be needed
    rts
.scav:
    panic 'S' ; static, so scav impossible
.static_closure:
    word fib_iter.code


;;; TODO: extend to 24 bit results

;;; [. . I] RL RH --> print("$result "); fib_iter [I+1]
fib_iter2:
    word .roots, .evac, .scav
.code:
    lda 0 ; RL
    ldx 1 ; RH
    jsr decimal_put_word
    lda #' '
    jsr screen_putchar
    jsr print_screen
    load_frame_var 2 ; I
    inc
    sta 0 ; I+1
    copy_code_pointer_to_local fib_iter.static_closure, fp
    jmp fib_iter.code
.roots:
    rts
.evac:
    evacuate 3
.scav:
    scavenge_done 3


;;; [] N KL KH --> fib [N-1 JL JH] where J is fib_cont1 [N KL KH]
fib_recurse:
    word .roots, .evac, .scav
.code:
    ;; access N
    lda 0
    sec
    cmp #2
    bcc fib_base ; N<2 ?
    ;; allocate cont1
    lda #5
    jsr alloc
    ;; fill in closure
    copy_code_pointer_to_heap0 fib_cont1.code
    copy_byte_local_to_heap 0, 2
    copy_word_local_to_heap 1, 3
    ;; setup args
    lda 0 ; N
    sec
    sbc #1 ; N-1
    sta 0
    copy_word clo, 1
    ;; TODO: extract standard entry protocol, 'enter'
    copy_code_pointer_to_local fib_recurse.static_closure, fp
    jmp fib_recurse.code
.roots:
    gc_root_at 1
    rts
.evac:
    copy_word ev, clo
    rts
.scav:
    panic 'S'
.static_closure:
    word fib_recurse.code


;;; N KL KH --> K [N #0]
fib_base:
    copy_word 1,fp ; K
    ;; N (low-byte of result) is already in 0
    lda #0
    sta 1 ; zero high-byte of result
    copy_word_from_frame0 cp ; TODO: avoid cp; using pha/pha/rts
    jmp (cp)


;;; [. . N KL KH] AL AH -->  fib [N-2 JL JH] where J is fib_cont2 [KL KH AL AH]
fib_cont1:
    word .roots, .evac, .scav
.code:
    ;; allocate cont2
    lda #6
    jsr alloc
    ;; fill in closure
    copy_code_pointer_to_heap0 fib_cont2.code ; TODO: alloc/fill via macro?
    copy_word_frame_to_heap 3, 2 ; K
    copy_word_local_to_heap 0, 4 ; A
    ;; setup args
    load_frame_var 2 ; N
    sec
    sbc #2 ; N-2
    sta 0
    copy_word clo,1
    copy_code_pointer_to_local fib_recurse.static_closure, fp
    jmp fib_recurse.code
.roots:
    rts
.evac:
    evacuate 5
.scav:
    scavenge_cell_at 3
    scavenge_done 5


;;; [. . KL HL AL AH] BL BH (TmpL TmpH) --> RL RH (where R = A + B)
fib_cont2:
    word .roots, .evac, .scav
.code:
    clc
    ;; TODO: use macro for 16 bit addition - already written!
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
    jmp (cp)
.roots:
    panic 'R' ; impossible because no allocation in this code
.evac:
    evacuate 6
.scav:
    scavenge_cell_at 2
    scavenge_done 6
