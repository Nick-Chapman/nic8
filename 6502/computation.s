
;;; Play with (forth-based) ideas to structure computations:
;;; - 0 basic 16bit leaf ops (+, inc) which can be combined
;;; - 1 use of data-stack for arguments and results
;;; - 2 forth style composition
;;; - 3 maintain own return stack, so...
;;; - 4 task switching (each with own pair of stacks)
;;; - 5 explore co-operative vs pre-emptive task switching
;;; Try out assembly macros
;;; Idea for computations:
;;; - counting
;;; - collatz
;;; - slow fib calculation
;;; - prime generation
;;; - binary2decimal
;;; - multiplication
;;; - ops for implementing a clock
;;; Beter LCD abstraction
;;; - perhaps manipulate screen-contents in memory
;;; - with sep thread which keeps LCD up to date with mem

    .org $fffc
    .word reset_main
    .word irq

    .org $8000

PORTB = $6000
DDRB = $6002

ticks = $10
g_number = $11 ; 2 bytes

g_divisor = $13 ; 2 bytes
g_mod10 = $15 ; 2 bytes

    include ticks.s
    include lcd.s

reset_main:
    jsr init_via
    jsr init_timer
    jsr init_display
    jsr clear_display
    jsr example
    jsr spin

init_via:
    lda #%11111111
    sta DDRB
    rts
spin:
    jmp spin

;;;--------------------
;;; example which manipulates 16bit number in mem
;;; by dispatching to stack based ops
example:
    jsr init_number
    jsr init_ds_stack
example_loop:
    jsr print_number_dec
    lda #1
    jsr sleep
    jsr push_number
    jsr ds_increment
    jsr pull_number
    jmp example_loop

init_number:
    ;; stz (not supported by assembler)
    lda #0
    sta g_number + 1
    sta g_number
    rts

print_number_dec: ; print 1-5 decimal digits for 16 bit number
    jsr clear_display
    lda g_number
    ldx g_number + 1
    jsr print_word_ax_in_decimal
    rts

print_dot:
    pha ; make sure this debug routine changes no registers!
    lda #'.'
    jsr print_char
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

ds_push_one:
    lda #1 ; lo
    ldx #0 ; hi
    jsr ds_push_ax
    rts

;;;--------------------
;;; primitive data-stack (ds) of 16bit (2byte) values
;;; keep data stack in zero page, using y to index

init_ds_stack:
    ldy #$ff
    rts

ds_push_ax: ; ( -- x;a )
    stx 0,y
    dey
    sta 0,y
    dey
    rts

ds_pull_ax: ; ( x;a -- )
    iny
    lda 0,y
    iny
    ldx 0,y
    rts

ds_dup: ; (v -- v v)
    dey
    dey
    lda 4,y
    sta 2,y
    lda 3,y
    sta 1,y
    rts

ds_add: ; (a b -- a+b)
    clc
    lda 1,y
    adc 3,y
    sta 3,y
    lda 2,y
    adc 4,y
    sta 4,y
    iny
    iny
    rts

;;;--------------------
;;; sleep for N (in acc) 1/100s
sleep:
    clc
    adc ticks
    pha ; goal ticks
sleep_wait:
    sec
    pla
    pha
    sbc ticks
    bne sleep_wait
    pla
    rts

;;;--------------------
;;; print word passed in a/x as 1-5 digit decimal number
print_word_ax_in_decimal:
    sta g_divisor ;lo
    stx g_divisor + 1 ;hi
    lda #0
    pha ; marker for print
each_digit:
    lda #0
    sta g_mod10
    sta g_mod10 + 1
    clc
    ldx #16
each_bit:
    rol g_divisor
    rol g_divisor + 1
    rol g_mod10
    rol g_mod10 + 1
    sec
    lda g_mod10
    sbc #10
    pha ; save
    lda g_mod10 + 1
    sbc #0
    bcc ignore_result
    sta g_mod10 + 1
    pla
    pha
    sta g_mod10
ignore_result:
    pla ;drop
    dex
    bne each_bit
    rol g_divisor
    rol g_divisor + 1
    clc
    lda g_mod10
    adc #'0'
    pha ;save on stack for reverse print
    lda g_divisor
    ora g_divisor + 1
    bne each_digit
print_from_stack:
    pla
    beq done
    jsr print_char
    jmp print_from_stack
done:
    rts
