;;; Example program which runs fib forver on increasing values of N.
;;; Entire program written in the CPS/GC style, with no outer harness.
;;; Purpose is to motivate a clean interface to the GC alloc.
;;; And to understand exactly what invariants are placed on user code.

    org $fffa
    word nmi
    word reset_main
    word irq

    org $8000

;;; bytes
heap_end_page = $30
n_bytes = $31
g_ticks = $32
gc_debug = $33
g_selected_screen = $34
g_nmi_count = $35
g_nmi_blocked = $36
g_next_screen_flush = $37

;;; words
hp = $40
fp = $42
cp = $44
clo = $46
ev = $48
lw = $4a
space_switcher = $4c
temp = $4e
gc_count = $50
heap_start = $52
g_divisor = $54 ; decimal.s
g_mod10 = $56 ; decimal.s

;;; quad
g_screen_pointers = $80

;;; buffers
g_screens = $200 ; 4*32 bytes

    include via.s
    include ticks.s
    include lcd.s
    include mscreen.s
    include decimal.s
    include print.s
    include panic.s
    include macs.s
    include gc.s

nmi:
    pha
    lda g_nmi_blocked
    bne .done
    lda #25 ; debounce time
    sta g_nmi_blocked
    inc g_nmi_count
.done:
    pla
    rti

irq: ; copy & extend version in ticks.s
    pha
    bit T1CL ; acknowledge interrupt
    inc g_ticks
    lda g_nmi_blocked
    beq .done
    dec g_nmi_blocked
.done:
    pla
    rti

init_nmi:
    stz g_nmi_blocked
    stz g_nmi_count
    rts

reset_main:
    ldx #$ff
    txs
    jsr init_via
    jsr init_ticks
    jsr init_nmi
    jsr init_lcd
    jsr lcd_clear_display
    jsr init_screen
    jsr init_gc

    ;; send GC debug to screen #1
    lda #1
    sta gc_debug

    jsr screen_flush_now

    jmp start_example


screen_flush_when_time:
    lda g_next_screen_flush
    sec
    sbc g_ticks
    beq screen_flush_now
    rts
screen_flush_now:
    lda g_nmi_count
    and #%1 ; use nmi-count to pick screen #0 or #1
    jsr screen_flush
    lda g_ticks
    clc
    adc #5 ; 20 times/sec
    sta g_next_screen_flush
    rts


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
    ;jsr screen_flush
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

    include fib16.s
