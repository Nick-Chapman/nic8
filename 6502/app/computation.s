
;;; Play with (forth-based) ideas to structure computations:
;;; - 0 basic 16bit leaf ops (+, inc) which can be combined DONE
;;; - 1 use of data-stack for arguments and results DONE
;;; - 2 forth style composition DONE
;;; - 3 maintain own return stack, so...
;;; - 4 task switching (each with own pair of stacks)
;;; - 5 explore co-operative vs pre-emptive task switching
;;; Try out assembly macros
;;; Idea for computations:
;;; - counting DONE
;;; - collatz
;;; - slow fib calculation (see fib.s)
;;; - prime generation
;;; - binary2decimal DONE
;;; - multiplication
;;; - ops for implementing a clock
;;; Beter LCD abstraction
;;; - perhaps manipulate screen-contents in memory DONE
;;; - with sep thread which keeps LCD up to date with mem (see pre-serial.s)

    org $fffc
    word reset_main
    word ticks_irq

    org $8000

;;; bytes
g_ticks = $50
g_screen_pointer = $51

;;; words
g_message_ptr = $70
g_number = $72
g_divisor = $74
g_mod10 = $76

;;; buffers
g_screen = $200 ; 32 bytes

    include via.s
    include ticks.s
    include sound.s
    include lcd.s
    include screen.s
    include decimal.s

reset_main:
    jsr init_via
    jsr init_ticks
    jsr init_sound ; silence
    jsr init_lcd
    jsr lcd_clear_display
    jsr init_screen
    jsr example
spin:
    jmp spin

;;;--------------------
;;; example which manipulates 16bit number in mem
;;; by dispatching to stack based ops
example:
    jsr init_number
    jsr init_ds_stack
    jsr print_screen_now
example_loop:
    jsr put_number_dec
    jsr put_dot
    jsr print_screen_when_time
    jsr push_number
    jsr ds_increment
    jsr pull_number
    jmp example_loop

next_screen_print = $33
print_screen_when_time:
    lda next_screen_print
    sec
    sbc g_ticks
    beq print_screen_now
    rts
print_screen_now:
    jsr print_screen
    lda g_ticks
    clc
    adc #5 ; 20 times/sec
    sta next_screen_print
    rts

init_number:
    lda #0
    sta g_number
    lda #0
    sta g_number + 1
    rts

put_number_dec: ; print 1-5 decimal digits for 16 bit number
    jsr screen_return_home
    lda g_number
    ldx g_number + 1
    jsr decimal_put_word
    rts

put_dot:
    pha
    lda #'.'
    jsr screen_putchar
    pla
    rts

push_number:
    lda g_number
    ldx g_number + 1
    jsr ds_push_ax
    rts

pull_number:
    jsr ds_pull_ax
    sta g_number
    stx g_number + 1
    rts

;;;--------------------
;;; derived data-stack (ds)  ops

ds_triple: ;not used at moment
    jsr ds_dup
    jsr ds_dup
    jsr ds_add
    jsr ds_add
    rts

ds_increment:
    jsr ds_push_one
    jsr ds_add
    rts

ds_decrement:
    jsr ds_push_minus_one
    jsr ds_add
    rts

ds_push_one:
    lda #1 ; lo
    ldx #0 ; hi
    jsr ds_push_ax
    rts

ds_push_minus_one:
    lda #$ff ; lo
    ldx #$ff ; hi
    jsr ds_push_ax
    rts

;;;--------------------
;;; primitive data-stack (ds) of 16bit values
;;; keep data stack in zero page, growing downwards; using y to index

init_ds_stack:
    ldy #$ff
    rts

ds_push_ax: ; ( -- x;a )
    dey
    dey
    sta 1,y
    stx 2,y
    rts

ds_pull_ax: ; ( x;a -- )
    lda 1,y
    ldx 2,y
    iny
    iny
    rts

ds_dup: ; (V -- V V)
    dey
    dey
    lda 4,y
    sta 2,y
    lda 3,y
    sta 1,y
    rts

ds_add: ; (A B -- A+B)
    clc
    lda 1,y
    adc 3,y ;lo
    sta 3,y
    lda 2,y
    adc 4,y ;hi
    sta 4,y
    iny
    iny
    rts