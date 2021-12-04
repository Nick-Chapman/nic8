
;;; setup dev framework code for serial link -- no actual progress on the serial stuff!

;;; But this example does demonstate:
;;; - screen print/wrap/scroll
;;; - multi-screen support

    org $fffa
    word nmi
    word reset_main
    word irq

    org $8000

;;; bytes
g_ticks = $50
g_selected_screen = $51
g_next_screen_print = $53
g_time_put_next_message_char = $54
g_nmi_count = $55
g_nmi_blocked = $56
g_next_screen_flush = $57

;;; words
g_message_ptr = $70
g_mptr = $72 ; print.s

NUM_SCREENS = 4

;;; quads (4 bytes)
g_screen_pointers = $80

;;; buffers
g_screens = $200 ; 4 * 32 bytes

    include via.s
    include ticks.s
    include sound.s
    include lcd.s
    include screen.s
    include print.s

nmi:
    pha
    lda g_nmi_blocked
    bne .done
    lda #25 ; debounce time
    sta g_nmi_blocked
    inc g_nmi_count
.done:
    pla
    rti

irq: ; copy & extend version in ticks.s
    pha
    bit T1CL ; acknowledge interrupt
    inc g_ticks
    lda g_nmi_blocked
    beq .done
    dec g_nmi_blocked
.done:
    pla
    rti

init_nmi:
    stz g_nmi_blocked
    stz g_nmi_count
    rts

reset_main:
    jsr init_via
    jsr init_ticks
    jsr init_nmi
    jsr init_sound ; silence
    jsr init_lcd
    jsr lcd_clear_display
    jsr init_screen

    ;; write fixed messages to screen #2 and #3
    lda #2
    sta g_selected_screen
    print_char 'x'
    lda #3
    sta g_selected_screen
    lda #<message
    sta 0
    lda #>message
    sta 1
    print_hex_word 0
    lda #0
    sta g_selected_screen

    jmp example

example:
    jsr screen_flush_now
    jsr init_put_message
example_loop:
    jsr screen_flush_when_time
    jsr put_next_message_char_when_time
    jmp example_loop

spin_with_flush:
    jsr screen_flush_when_time
    jmp spin_with_flush

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
    beq spin_with_flush ; spin when reach end of message
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
    jsr toggle_screen_write
not_a_space:
    adc #10 ; everything else, print at 10 chars/sec
    adc g_ticks
    sta g_time_put_next_message_char
put_next_message_char_done:
    rts

;;; we toggle between screen 0 and 1 on every work in the message
toggle_screen_write: ; between screen #0 and #1
    pha
    lda g_selected_screen
    eor #1
    sta g_selected_screen
    pla
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
    text "zero one two "
    text "three four "
    text "five six "
    text "seven eight nine "
    text "ten eleven twelve thirteen fourteen fifteen sixteen"
    byte 0
