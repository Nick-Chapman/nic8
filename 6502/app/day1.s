;;; AoC 2021, day1, parts 1 & 2

    org $fffc
    word reset_main
    word irq

    org $8000

    include via.s
    include ticks.s
    include lcd.s
    include macs.s
    include decimal16.s
    include screen.s
    include print.s
    include sleep.s
    include arith16.s

irq:
    bit via.T1CL ; acknowledge interrupt
    inc g_ticks
    rti

reset_main:
    jsr via.init
    jsr init_ticks
    jsr lcd.init
    jsr screen.init
    print_string 'Part1 '
    screen_flush_selected
    jsr part1
    newline
    print_string 'Part2 '
    screen_flush_selected
    jsr part2
spin:
    jmp spin

copy16: macro A, B
    lda \A
    sta \B
    lda \A + 1
    sta \B + 1
endmacro

debug: macro C
    print_char \C
    screen_flush_selected
endmacro

print16: macro A
    print_decimal_word \A
    screen_flush_selected
endmacro

g_ticks = $0

g_mod10 = $2
g_divisor = $4
g_selected_screen = $6
item = $8
last = $a
last2sum = $c
count = $e
res = $10
ptr = $12
ptr2 = $14
g_mptr = $58 ; print.s
g_putchar = $5a ; decimal16.s

g_nmi_count = $35 ;byte
g_next_screen_flush = $37 ;byte

NUM_SCREENS = 1
g_screen_pointers = $80
g_screens = $200
window = $300

set_pointer: macro P, W
    lda #<\W
    sta \P
    lda #>\W
    sta \P + 1
endmacro

read_item: macro P, A
    lda (\P)
    sta \A
    ldy #1
    lda (\P),y
    sta \A + 1
    increment16 \P
    increment16 \P
endmacro

store_item: macro A, P
    lda \A
    sta (\P)
    lda \A + 1
    ldy #1
    sta (\P),y
    increment16 \P
    increment16 \P
endmacro

part1:
    set_pointer ptr, data
    jsr count_increasing_at_ptr
    rts

part2:
    jsr prepare_sliding_window_data
    set_pointer ptr, window
    jsr count_increasing_at_ptr
    rts

prepare_sliding_window_data:
    set_pointer ptr, data
    set_pointer ptr2, window
    read_item ptr, item
    read_item ptr, last
    add16 last, item, last2sum
.each_item:
    read_item ptr, item
    ;; if item is zero then stop
    lda item
    ora item + 1
    beq .finish
    ;; compute sliding window of 3 items
    add16 item, last2sum, res
    store_item res, ptr2
    add16 item, last, last2sum
    copy16 item, last
    jmp .each_item
.finish:
    store_item item, ptr2 ; write 0-terminator
    rts

count_increasing_at_ptr:
    ;; init count
    stz count
    stz count + 1
    read_item ptr, last
.each_item:
    read_item ptr, item
    ;; if item is zero then stop
    lda item
    ora item + 1
    beq .finish
    ;; if item > last, then increment count
    compare16 last, item
    bcs .dont_count
    increment16 count
.dont_count:
    copy16 item, last
    jmp .each_item
.finish:
    print16 count
    rts

pause:
    pha
    lda #25
    jsr sleep_blocking
    pla
    rts

data:
    ;include "day1.input.sam-words"
    include "day1.input-words"
    word 0
