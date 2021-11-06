
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
    jsr even_simpler_example
spin:
    jmp spin

init_via:
    lda #%11111111
    sta DDRB
    rts


even_simpler_example:
    jsr init_screen
    jsr sleep1
    jsr update_pause
    lda #'1'
    jsr putchar
    jsr update_pause
    lda #'2'
    jsr putchar
    jsr update_pause
    lda #'3'
    jsr putchar
    jsr update_pause
    lda #'x'
    jsr putchar
    jsr putchar
    jsr putchar
    jsr putchar
    jsr putchar
    jsr putchar
    jsr putchar
    jsr putchar
    jsr putchar
    jsr putchar
    jsr update_pause
    lda #'4'
    jsr putchar
    jsr update_pause
    lda #'5'
    jsr putchar
    jsr update_pause
    lda #'6'
    jsr putchar
    jsr update_pause
    lda #'7'
    jsr putchar
    jsr update_pause
    lda #'8'
    jsr putchar
    jsr update_pause
    rts

update_pause:
    ;jsr clear_display
    jsr print_screen
    jsr sleep1
    rts

simple_example: ; just while testing asyn print
    jsr return_home
    lda #<789
    ldx #>789
    jsr print_word_ax_in_decimal
    jsr print_dot

    jsr sleep1
    jsr return_home
    lda #<1234
    ldx #>1234
    jsr print_word_ax_in_decimal
    jsr print_dot

    jsr sleep1
    jsr return_home
    lda #42
    ldx #0
    jsr print_word_ax_in_decimal
    jsr print_dot
    rts


sleep1:
    pha
    lda #100
    jsr sleep
    pla
    rts

;;;--------------------
;;; example which manipulates 16bit number in mem
;;; by dispatching to stack based ops
example:
    jsr init_number
    jsr init_ds_stack
example_loop:
    jsr print_number_dec
    jsr print_dot
    jsr push_number
    jsr ds_decrement
    jsr pull_number
    jmp example_loop

init_number:
    lda #100
    sta g_number
    lda #0
    sta g_number + 1
    rts

print_number_dec: ; print 1-5 decimal digits for 16 bit number
    jsr return_home
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

;;;--------------------
;;; async lcd printing

g_screen = $200 ; 32 bytes
g_screen_pointer = $220

putchar:
    ;jsr print_char
    ldx g_screen_pointer
    sta g_screen,x
    inc g_screen_pointer
    rts

return_home:
    ;jsr lcd_return_home
    lda #0
    sta g_screen_pointer
    rts

init_screen:
    jsr return_home
    ;; copy patten into screen array: a..z, 3 spaces, 2 Xs, !
    ldx #0
each_pat_char:
    txa
    clc
    adc #'a'
    sta g_screen,x
    inx
    sec
    cpx #26
    bne each_pat_char
    lda #' ' ; space
    sta g_screen+26
    sta g_screen+27
    sta g_screen+28
    lda #'X'
    sta g_screen+29
    sta g_screen+30
    lda #'!'
    sta g_screen+31 ; bang in last place
    rts


print_screen:
    ;; copy screen to lcd
    jsr lcd_return_home
    ldx #0
each_line1_char:
    lda g_screen,x
    jsr print_char
    inx
    sec
    cpx #16
    bne each_line1_char
    ;; reposition to line2. do 24 dummy prints
    lda #'+' ;dont expect to see this
    ldx #24
each_dummy_print:
    jsr print_char
    dex
    bne each_dummy_print
    ldx #16
each_line2_char:
    lda g_screen,x
    jsr print_char
    inx
    sec
    cpx #32
    bne each_line2_char
    rts
