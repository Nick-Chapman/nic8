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
    jsr screen_flush
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

    include fib.s
