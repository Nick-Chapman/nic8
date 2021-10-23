
send_message:
    ldy #0
send_message_loop:
    lda (MPTR),y
    beq send_message_done
    jsr print_char
    iny
    jmp send_message_loop
send_message_done:
    rts
