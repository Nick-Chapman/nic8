;;; Explore various coding of parameter passing for fib functions
;;; including different stack and heap disciplines

    org $fffa
    word nmi
    word reset_main
    word irq

    org $8000

;;; bytes
gc_screen = $4f
g_arg = $50 ; used by fib1
g_ticks = $51
g_selected_version_index = $54
g_selected_screen = $55
g_nmi_count = $56
g_nmi_blocked = $57
g_next_screen_flush = $58

;;; words
g_res = $70 ; used by fib1
g_divisor = $72 ; decimal.s
g_mod10 = $74 ; decimal.s
g_putchar = $76 ; decimal.s
gc_count = $8a
heap_start = $8c
g_selected_version_ptr = $90
g_version_name = $92
g_mptr = $94
g_heap_pointer = $f0
heap_end_page = $f8 ; (byte)
space_switcher = $fa

NUM_SCREENS = 2
g_screen_pointers = $a0 ; 8 bytes

;;; buffers
g_screens = $200 ; 8x 32 bytes

BASE = 10
arg2 = BASE + 2
arg3 = BASE + 3
arg4 = BASE + 4
arg5 = BASE + 5
arg6 = BASE + 6

flush: macro
    pha
    lda g_nmi_count
    and #%1 ; use nmi-count to pick screen #0 or #1
    jsr screen_flush_sub
    pla
endmacro

    include via.s
    include ticks.s
    include arith16.s
    include acia.s
    include sound.s
    include lcd.s
    include screen.s
    include sleep.s
    include macs.s
    include decimal.s
    include print.s
    include debug.s
    include panic.s
    include macs.s
    include arith16.s
    include heap.s
fp = $a  ; frame-pointer

find_roots:
    find_roots_from fp
    rts

NEXT: macro A
    jsr screen_flush_when_time
    jmp \A
endmacro

    include nmi_irq.s

    ;; various implementations of fib
    include fib1.s
    include fib2.s
    include fib3.s
    include fib4.s
    include fib5.s
    include fib6.s
    include fib7.s

num_versions_minus_1 = (((version_table_end - version_table) >> 1) - 1)

version_table:
    word fib1_entry
    word fib2_entry
    word fib3_entry
    word fib4_entry
    word fib5_entry
    word fib6_entry
    word fib7_entry
version_table_end:

reset_main:
    ldx #$ff
    txs

    jsr init_via
    jsr init_ticks
    jsr init_nmi_irq
    jsr init_sound ; silence
    jsr init_acia
    jsr init_lcd
    jsr lcd_clear_display
    jsr init_screen
    jmp example

example:
    jsr select_version
    jsr put_version_name
    flush

    jsr pause
    jsr pause
    jsr screen_newline
    lda #10 ; Compute fib(N) for N = ...
    pha ; keep N on the stack

example_loop:

    ;; Access N from the stack without popping it..
    tsx
    lda $101,x

    ldx #0
    jsr decimal_put_word ; ..so we can print it
    print_char '-'
    flush
    ;jsr pause

    ;; All versions have same interface: byte argument in A; 2 bytes space on stack for result

    pha ; reserve 2-bytes for timing-result
    pha
    jsr trial_start_timer
    pha ; reserve 2-bytes for FIB-result
    pha
    tsx
    lda $105,x ; Access N again (now under 4 bytes), to setup the argument to fib (in acc)
    jsr version_dispatch
    jsr trial_stop_timer

    pla ; result-LO into A, and
    plx ; result-HI into X, which..
    jsr decimal_put_word ; ..is the calling convention to print a word

    print_string " ("
    pla ; timer-LO into A, and
    plx ; timer-HI into X, which..
    jsr decimal_put_word ; ..as before
    print_char ')'
    flush

    tsx

    lda $101,x
    cmp #50
    bne _1$
    jmp finish
_1$:

    inc $101,x ; increment N (in place) on stack

    jsr pause
    jsr screen_newline
    flush

    jmp example_loop


finish:
    print_char '$'
    flush
spin:
    jmp spin

trial_start_timer:
    tsx
    lda g_ticks
    sta $103,x ; timing-word under: 2-bytes return-addr
    lda #0
    sta $104,x
    rts

trial_stop_timer:
    tsx
    lda g_ticks
    sec
    sbc $105,x ; timing-word under: 2-bytes return-addr, 2-bytes fib-result
    sta $105,x
    rts

version_dispatch:
    jmp (g_selected_version_ptr)

select_version:
    sec
    lda #num_versions_minus_1
    cmp g_selected_version_index ; on power up may contain any value
    bcs .after_reset             ; dont reset if g_selected_version_index in range
    lda #0
    sta g_selected_version_index ; select first version
.after_reset:
    lda g_selected_version_index
    inc g_selected_version_index ; next version on reset
    asl
    tay
    lda version_table,y
    sta g_selected_version_ptr
    lda version_table+1,y
    sta g_selected_version_ptr + 1
    rts

put_version_name:
    lda g_selected_version_ptr
    sec
    sbc #2
    sta g_version_name
    lda g_selected_version_ptr + 1
    bcs .no_wrap
    sbc #1
.no_wrap:
    sta g_version_name + 1
    ldy #1
    lda (g_version_name),y ;hi
    pha
    dey
    lda (g_version_name),y ;lo
    pha
    jsr put_string
    pla
    pla
    rts

pause:
    pha
    lda #50
    jsr sleep_blocking
    pla
    rts
