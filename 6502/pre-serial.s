
;;; setup dev framework code for serial link

    .org $fffc
    .word reset_main
    .word irq

    .org $8000

PORTB = $6000
DDRB = $6002

ticks = $71                     ; TODO: rename g_tick & fixup all files
g_screen_pointer = $72
g_next_screen_print = $73
g_time_put_next_message_char = $74
g_message_ptr = $75 ; 2 bytes
; $77 next

g_screen = $200 ; 32 bytes

    include ticks.s
    include lcd.s
    include screen.s

reset_main:
    jsr init_via
    jsr init_timer
    jsr init_display
    jsr clear_display
    jsr init_screen
    jmp example

init_via:
    lda #%11111111
    sta DDRB
    rts

example:
    jsr print_screen_now
    jsr init_put_message
example_loop:
    jsr print_screen_when_time
    jsr put_next_message_char_when_time
    jmp example_loop

spin:
    jmp spin

print_screen_when_time:
    lda g_next_screen_print
    sec
    sbc ticks
    beq print_screen_now
    rts
print_screen_now:
    jsr print_screen
    lda ticks
    clc
    adc #5 ; 20 times/sec
    sta g_next_screen_print
    rts


init_put_message:
    jsr init_message_ptr
    lda ticks
    clc
    adc #100 ; wait 1 sec to start
    sta g_time_put_next_message_char
    rts

put_next_message_char_when_time:
    lda g_time_put_next_message_char
    sec
    sbc ticks
    bne put_next_message_char_done
    ldy #0
    lda (g_message_ptr),y
    beq spin ; spin when reach end of message
    jsr maybe_scroll
    pha ;save char
    jsr putchar ; only call to underlying (screen)putchar; follows maybe_scroll, so is safe
    jsr increment_message_ptr
    pla ;get char
    tay
    lda #0
    cpy ' '
    bne not_a_space
    clc
    adc #50 ; extra 1/2 second for a space
not_a_space:
    adc #25 ; everything else, print at 4 chars/sec
    adc ticks
    sta g_time_put_next_message_char
put_next_message_char_done:
    rts

init_message_ptr:
    lda #<message
    sta g_message_ptr
    lda #>message
    sta g_message_ptr + 1
    rts

increment_message_ptr:
    inc g_message_ptr
    bne increment_message_ptr_done
    inc g_message_ptr + 1
increment_message_ptr_done:
    rts

message:
    .asciiz "one two three four five six seven eight nine ten eleven twelve thrirteen"

maybe_scroll:
    pha
    lda g_screen_pointer
    cmp #32
    bne maybe_scroll_done
    jsr scroll
    jsr screen_return_to_start_line2
maybe_scroll_done:
    pla
    rts

scroll:
    ldx #0
each_scroll_char:
    lda g_screen+16,x
    sta g_screen,x
    lda #' '
    sta g_screen+16,x
    inx
    sec
    cpx #16
    bne each_scroll_char
    rts

screen_return_to_start_line2:
    lda #16
    sta g_screen_pointer
    rts
