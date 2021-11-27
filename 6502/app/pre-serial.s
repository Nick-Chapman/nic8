
;;; setup dev framework code for serial link -- hmm, didn't actually make much progress on the serial stuff!

    org $fffc
    word reset_main
    word ticks_irq

    org $8000

;;; bytes
g_ticks = $50
g_screen_pointer = $51
g_next_screen_print = $52
g_time_put_next_message_char = $53

;;; words
g_message_ptr = $75

;;; buffers
g_screen = $200 ; 32 bytes

    include via.s
    include ticks.s
    include sound.s
    include lcd.s
    include screen.s

reset_main:
    jsr init_via
    jsr init_ticks
    jsr init_sound ; silence
    jsr init_lcd
    jsr lcd_clear_display
    jsr init_screen
    jmp example

example:
    jsr screen_flush_now
    jsr init_put_message
example_loop:
    jsr screen_flush_when_time
    jsr put_next_message_char_when_time
    jmp example_loop

spin:
    jmp spin

screen_flush_when_time:
    lda g_next_screen_print
    sec
    sbc g_ticks
    beq screen_flush_now
    rts
screen_flush_now:
    jsr screen_flush
    lda g_ticks
    clc
    adc #5 ; 20 times/sec
    sta g_next_screen_print
    rts

init_put_message:
    jsr init_message_ptr
    lda g_ticks
    clc
    adc #50 ; wait 1/2 sec to start
    sta g_time_put_next_message_char
    rts

put_next_message_char_when_time:
    lda g_time_put_next_message_char
    sec
    sbc g_ticks
    bne put_next_message_char_done
    ldy #0
    lda (g_message_ptr),y
    beq spin ; spin when reach end of message
    pha ;save char
    jsr screen_putchar
    jsr increment_message_ptr
    pla ;get char
    tay
    lda #0
    cpy #' '
    bne not_a_space
    clc
    adc #20 ; extra time for a space
not_a_space:
    adc #10 ; everything else, print at 10 chars/sec
    adc g_ticks
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
    str "zero one two"
    str "three four"
    str "five six"
    str "seven eight nine"
    string "ten eleven twelve thirteen fourteen fifteen sixteen"
    byte 0
