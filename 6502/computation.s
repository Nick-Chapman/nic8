
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
    jsr step_number
    jsr print_number
    jsr example_pause
    jmp example_loop

init_number:
    ;; stz (not supported by assembler)
    lda #0
    sta g_number + 1
    sta g_number
    rts
print_number: ; print 4 hex digits for 16 bit number
    jsr clear_display
    ;;jsr print_dot
    lda g_number + 1
    jsr print_byte_as_hex
    lda g_number
    jsr print_byte_as_hex
    rts
print_dot:
    lda #'.'
    jsr print_char
    rts
example_pause:
    lda #100 ; 1 sec
    jsr sleep
    rts

step_number:
    jsr push_number
    jsr ds_triple
    jsr ds_increment
    jsr pull_number
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

ds_triple:
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
    lda #1
    ldx #0
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
;;; print byte passed in accumulator as 2 digit hex number
print_byte_as_hex
    pha
    lsr
    lsr
    lsr
    lsr
    tax
    lda digits,x
    jsr print_char
    pla
    and #%1111
    tax
    lda digits,x
    jsr print_char
    rts

digits: .ascii "0123456789abcdef"
