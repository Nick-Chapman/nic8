;;; Explore fib example...
;;; (1) coded using normal control stack -- DONE
;;; (2) coded in CPS style, using heap -- TODO

    .org $fffc
    .word reset_main
    .word ticks_irq

    .org $8000

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
g_selected_version_ptr = $76
g_id_ptr = $78
g_mptr = $80

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

num_versions_minus_1 = (((version_table_end - version_table) >> 1) - 1)

version_table:
    .word fib1_entry
    .word fib2_entry
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
    jsr pause
    jsr pause
    jsr screen_newline
    lda #10
    pha

example_loop:

    tsx
    lda $101,x

    jsr decimal_put_byte
    lda #'-'
    jsr screen_putchar
    jsr print_screen

    ;; All versions have same interface: byte argument in A; 2 bytes space on stack for result
    tsx
    lda $101,x
    pha
    pha
    jsr version_dispatch
    pla
    plx

    jsr decimal_put_word
    lda #' '
    jsr screen_putchar
    jsr print_screen
    jsr pause
    jsr screen_newline
    jsr print_screen

    tsx
    inc $101,x

    jmp example_loop


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
