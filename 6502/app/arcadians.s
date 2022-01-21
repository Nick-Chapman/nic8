
;;; Play the start music from the BBC Micro's Arcadians

    org $fffc
    word main_reset
    word irq

    org $8000

;;; bytes
g_ticks = $50
g_sleep_ticks = $51

;;; words
g_ptr = $70

    include via.s
    include ticks.s
    include sound.s
    include lcd.s

irq:
    bit via.T1CL ; acknowledge interrupt
    inc g_ticks
    rti

main_reset:
    jsr via.init
    jsr init_ticks
    jsr sound.init
    jsr lcd.init
    jsr lcd.clear_display
    jsr print_message
    jmp play_music

print_message:
    ldy #0
print_message_loop:
    lda message,y
    beq print_message_done
    jsr lcd.putchar
    iny
    jmp print_message_loop
print_message_done:
    rts

message: asciiz "** Arcadians ** "

play_music:
    lda #(data & $ff)       ;lo
    sta g_ptr
    lda #(data >> 8)        ;hi
    sta g_ptr+1
    ldy #0
    lda (g_ptr),y           ; read DEL
    asl                     ; double it; to change units from 1/50s -> 1/100s
    clc
    adc g_ticks
    sta g_sleep_ticks
top_loop:
    jsr send_if_time_to_send
    jmp top_loop

send_if_time_to_send:
    sec
    lda g_sleep_ticks
    sbc g_ticks
    beq send_bytes
    rts
send_bytes:
    ;; g_ptr points to the delay we just did, at +1 we have #bytes to send
    ldy #1
    lda (g_ptr),y               ; read N, the #bytes to send
    beq finish                  ; stop if 0
    tax
continue_send_bytes:
    iny                         ; y=2 (skipping DEL,N)
    ;; x: N, N-1 ... 1
    ;; y: 2, 3 ... N+2
    lda (g_ptr),y               ; read data byte to send
    jsr sound.send_byte
    dex
    bne continue_send_bytes
    iny                     ; y is now N+2
    ;; read next DEL and increment g_sleep_ticks
    lda (g_ptr),y
    asl                         ; double DEL
    clc
    adc g_sleep_ticks
    sta g_sleep_ticks
    ;; shift buffer pointer by y (N+2)
    tya
    clc
    adc g_ptr
    sta g_ptr
    bcc shift_btr_no_carry
    inc g_ptr + 1
shift_btr_no_carry:
    rts

finish:
spin:
    jmp spin

;;; buffer format: repeats of: DEL, N, b1,b2..bN,
;;; DEL in 1/50s, N is number of following bytes

data:
    include arc-data.s
    byte 0, 0                     ;FINISH
