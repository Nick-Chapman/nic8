;;; Explore fib example...
;;; (1) coded using normal control stack -- DONE
;;; (2) coded in CPS style, using heap -- TODO

    .org $fffc
    .word reset_main
    .word ticks_irq

    .org $8000

;;; Globals for fib7 GC
;;; Heap pointer and frame pointer in ZP
hp = $f0
fp = $f2
cp = $f4
clo = $f6 ; pointer to (space for) the closure just allocated
heap_end_page = $f8 ; (byte)
n_bytes = $f9 ; number of bytes to allocate (byte)
space_switcher = $fa
temp = $fc
;wipe_old_space = $fe
ev = $77 ; word being evacuated
lw = $88 ; low water mark in to-heap; the point from which we scavenge

;;; bytes
g_arg = $50 ; used by fib1
g_ticks = $51
g_sleep_ticks = $52
g_screen_pointer = $53
g_selected_version_index = $54

;;; words
g_res = $70 ; used by fib1
g_divisor = $72
g_mod10 = $74

g_selected_version_ptr = $90
g_id_ptr = $92
g_mptr = $94

;;; buffers
g_screen = $200 ; 32 bytes

    include via.s
    include ticks.s
    include sound.s
    include lcd.s
    include screen.s
    include sleep.s
    include decimal.s

    ;; various implementations of fib
    include fib1.s
    include fib2.s
    include fib3.s
    include fib4.s
    include fib5.s
    include fib6.s
    .text "<fib7..."
    include fib7.s
    .text "...fib7>"

num_versions_minus_1 = (((version_table_end - version_table) >> 1) - 1)

version_table:
    .word fib1_entry
    .word fib2_entry
    .word fib3_entry
    .word fib4_entry
    .word fib5_entry
    .word fib6_entry
    .word fib7_entry
version_table_end:

reset_main:
    ldx #$ff
    txs

    jsr init_via
    jsr init_ticks
    jsr init_sound ; silence
    jsr init_lcd
    jsr lcd_clear_display
    jsr init_screen
    jmp example

example:
    jsr select_version
    jsr put_version_name
    jsr print_screen
    ;jsr pause
    jsr pause
    jsr screen_newline
    lda #10 ; Compute fib(N) for N = ...
    pha ; keep N on the stack

example_loop:

    ;; Access N from the stack without popping it..
    tsx
    lda $101,x

    jsr decimal_put_byte ; ..so we can print it
    lda #'-'
    jsr screen_putchar
    jsr print_screen
    jsr pause

    ;; All versions have same interface: byte argument in A; 2 bytes space on stack for result

    pha ; reserve 2-bytes for timing-result
    pha
    jsr start_timer
    pha ; reserve 2-bytes for FIB-result
    pha
    tsx
    lda $105,x ; Access N again (now under 4 bytes), to setup the argument to fib (in acc)
    jsr version_dispatch
    jsr stop_timer

    pla ; result-LO into A, and
    plx ; result-HI into X, which..
    jsr decimal_put_word ; ..is the calling convention to print a word
    lda #' '
    jsr screen_putchar

    lda #'('
    jsr screen_putchar
    pla ; timer-LO into A, and
    plx ; timer-HI into X, which..
    jsr decimal_put_word ; ..as before
    lda #')'
    jsr screen_putchar

    jsr print_screen

    tsx

    lda $101,x
    cmp #50
    bne _1$
    jmp stop
_1$:

    inc $101,x ; increment N (in place) on stack

    jsr pause
    jsr pause
    jsr screen_newline
    jsr print_screen

    jmp example_loop

start_timer:
    tsx
    lda g_ticks
    sta $103,x ; timing-word under: 2-bytes return-addr
    lda #0
    sta $104,x
    rts

stop_timer:
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
    bcs after_reset_to_version0  ; dont reset if g_selected_version_index in range
    lda #0
    sta g_selected_version_index ; select first version
after_reset_to_version0:
    lda g_selected_version_index
    inc g_selected_version_index ; next version on reset
    asl
    tay
    lda version_table,y
    sta g_selected_version_ptr
    lda version_table+1,y
    sta g_selected_version_ptr + 1
    rts

put_version_name: ; TODO: avoid use of g_id_ptr
    lda g_selected_version_ptr
    sec
    sbc #2
    sta g_id_ptr
    lda g_selected_version_ptr + 1
    bcs no_wrap
    sbc #1
no_wrap:
    sta g_id_ptr + 1
    ldy #1
    lda (g_id_ptr),y ;hi
    pha
    dey
    lda (g_id_ptr),y ;lo
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

put_string:
    tsx
    lda $103,x ; string-pointer-word (under return-address-word)
    sta g_mptr
    lda $104,x
    sta g_mptr + 1
    ldy #0
put_string_loop:
    lda (g_mptr),y
    beq put_string_done
    jsr screen_putchar
    iny
    jmp put_string_loop
put_string_done:
    rts



;;; TODO: remove this!
eeee_evacuate:
    print_char '1'
    jmp stop
eeee_scavenge:
    print_char '2'
    jmp stop
eeee_code:
    print_char '3'
    jmp stop

    .org ($eeee - 5)
    .word eeee_evacuate, eeee_scavenge
    .byte 7
    jmp eeee_code
